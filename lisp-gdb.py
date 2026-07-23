"""
GDB Python support script for Lisp.

See: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Python.html
"""

from enum import IntEnum
import gdb
from gdb.types import make_enum_dict

IR_BIAS = 0x8000
REF_FIRST = IR_BIAS + 1

IR_TYPE = 0x3F
IR_GUARD = 0x40
REG_NONE = 0x80


def symbol_name(LispSymbolPtr, v):
    sym = (v - 1).cast(LispSymbolPtr)
    return sym["name"].string("utf-8", "replace", sym["len"])


def disassemble(n, xs, indent=0):
    """Disassemble Lisp bytecode."""

    LispObjectPtr = gdb.lookup_type("LispObject").pointer()
    LispSymbolPtr = gdb.lookup_type("struct LispSymbol").pointer()
    Prototype = gdb.lookup_type("struct Prototype")
    Op = IntEnum("Op", make_enum_dict(gdb.lookup_type("enum Op")))

    def k():
        return (xs + i - b).cast(LispObjectPtr).dereference()

    s = ""
    i = 0
    while i < n:
        x = xs[i]
        i += 1
        a = int(x["a"])
        b = int(x["b"])
        c = int(x["c"])

        s += " " * indent + f"{i:04} {x["op"]} "
        match x["op"]:
            case Op.MOV:
                s += f"{a} <- {c}\n"
            case Op.LOADNIL:
                s += f"{a} <- NIL\n"
            case Op.LOADOBJ:
                s += f"{a} <- {int(k()):x}\n"
            case Op.LOADSHORT:
                s += f"{a} <- {(b ^ 0x8000) - 0x8000}\n"
            case Op.GETGLOBAL:
                s += f"{a} <- [{symbol_name(LispSymbolPtr, k())}]\n"
            case Op.SETGLOBAL:
                s += f"{a} -> [{symbol_name(LispSymbolPtr, k())}]\n"
            case Op.GETUPVALUE:
                s += f"{a} <- {c}\n"
            case Op.SETUPVALUE:
                s += f"{a} -> {c}\n"
            case Op.JMP:
                s += f"=> {i + b:04}\n"
            case Op.JNIL:
                s += f"if {a} == NIL => {i + b:04}\n"
            case Op.CALL | Op.TAILCALL:
                args = "".join(f" {a + 2 + j}" for j in range(c))
                s += f"{a} <- ({a}{args})\n"
            case Op.RET:
                s += f"{a}\n"
            case Op.FNEW:
                proto = (xs + i).cast(Prototype.pointer())
                arity = int(proto["arity"])
                num_upvals = int(proto["num_upvalues"])
                s += f"{a} <- (arity: {arity} num_upvals: {num_upvals}):\n"
                metadata_size = Prototype.sizeof + num_upvals + x.type.sizeof - 1
                body = proto["body"].dereference().address
                s += disassemble(b - metadata_size // x.type.sizeof, body, indent + 2)
                i += b
            case Op.CLO | Op.FHDR | Op.FHDR_INTERPR | Op.FHDR_JIT:
                s += "\n"
            case _:
                raise ValueError("Invalid operation code")
    return s


def dump_trace(trace):
    LispObjectHeaderPtr = gdb.lookup_type("struct LispObjectHeader").pointer()
    LispSymbolPtr = gdb.lookup_type("struct LispSymbol").pointer()
    LispCFunctionPtr = gdb.lookup_type("struct LispCFunction").pointer()
    NodePtr = gdb.lookup_type("union Node").pointer()
    SnapshotPtr = gdb.lookup_type("struct Snapshot").pointer()
    SnapshotEntryPtr = gdb.lookup_type("struct SnapshotEntry").pointer()
    LispType = IntEnum("LispType", make_enum_dict(gdb.lookup_type("enum LispType")))
    SsaOp = IntEnum("SsaOp", make_enum_dict(gdb.lookup_type("enum SsaOp")))

    type_names = ["AxB", "sym", "str", "cfn", "clo", None, None, "nil", "int", "___"]
    num_snapshots = int(trace["num_snapshots"])
    num_consts = int(trace["num_consts"])
    length = int(trace["len"]) - num_consts
    insns = trace["data"].cast(NodePtr)
    snap = snapshots = (insns + (num_consts + length)).cast(SnapshotPtr)
    entries = (snapshots + num_snapshots).cast(SnapshotEntryPtr)

    def is_var(ref):
        return ref >= IR_BIAS

    def fmt_ref(ref):
        if is_var(ref):
            return f"{ref - IR_BIAS:04}"
        v = insns[num_consts + ref - IR_BIAS]["v"]
        if not v & 1:
            return f"{((int(v) & 0xffffffff ^ 0x80000000) - 0x80000000) // 2:+}"
        match (v - 1).cast(LispObjectHeaderPtr)["tag"]:
            case LispType.LISP_NIL:
                return "nil "
            case LispType.LISP_SYMBOL:
                return f"[{symbol_name(LispSymbolPtr, v)}]"
            case LispType.LISP_CFUNCTION:
                name = (v - 1).cast(LispCFunctionPtr)["name"].string("utf-8", "replace")
                return f"<{name}>"
            case _:
                return f"{int(v):#x}"

    def fmt_snap(entries, snap):
        offset = int(snap["offset"])
        s = "[ "
        j = 0
        for i in range(int(snap["num_entries"])):
            entry = entries[offset + i]
            slot = int(entry["slot"])
            s += "---- " * (slot - j) + fmt_ref(int(entry["ref"])) + " "
            j = slot + 1
        return s + "]"

    s = "---- TRACE IR\n"
    i = 1
    snap_idx = 0
    while True:
        if snap_idx < num_snapshots and IR_BIAS + i >= snap["beg"]:
            s += f"....            SNAP  #{snap_idx:<3} {fmt_snap(entries, snap)}\n"
            snap_idx += 1
            snap += 1
        if i >= length:
            break

        x = insns[num_consts + i]
        ty = int(x["ty"])
        reg = int(x["reg"])
        guard = ">" if ty & IR_GUARD else " "
        sp = "~" if x["spill_slot"] else " "
        reg_str = "" if reg == 0xFF else reg & ~REG_NONE
        s += f"{i:04} {guard}{sp} {type_names[ty & IR_TYPE]} {reg_str:3} {str(x["op"])[3:]:<5} "
        a = int(x["a"])
        b = int(x["b"])
        match x["op"]:
            case SsaOp.IR_SLOAD:
                s += f"#{a}\n"
            case SsaOp.IR_GLOAD:
                s += fmt_ref(a) + "\n"
            case SsaOp.IR_ULOAD:
                s += f"#{b} from {fmt_ref(a)}\n"
            case SsaOp.IR_PLOAD:
                s += f"sp: {b}\n" if b else f"reg: {a}\n"
            case SsaOp.IR_CALL:
                ap = insns + num_consts + i + 1
                args = " ".join(fmt_ref(int(ap[j]["a"])) for j in range(b))
                s += f"{fmt_ref(a)} ({args})\n"
                i += b
            case SsaOp.IR_CALLARG:
                raise ValueError("Invalid trace")
            case SsaOp.IR_RET:
                s += "\n"
            case SsaOp.IR_LOOP:
                s += "------------\n"
            case _:
                s += (fmt_ref(a) + (" " + fmt_ref(b) if b else "") if a else "") + "\n"
        i += 1
    return s + "---- TRACE stop\n"


class LispChunkPrinter(gdb.ValuePrinter):
    """Print a Chunk object."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        LispObject = gdb.lookup_type("LispObject")
        Instruction = gdb.lookup_type("struct Instruction")
        num_consts = self.val["num_consts"]
        count = int(self.val["count"])
        constants = self.val["data"].cast(LispObject.pointer())
        insns = (constants + num_consts).cast(Instruction.pointer())
        return "-- BYTECODE --\n" + disassemble(count, insns)


class LispTracePrinter(gdb.ValuePrinter):
    """Print a LispTrace object."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        return dump_trace(self.val)


class DumpBcParam(gdb.Parameter):
    """Dump executed bytecode."""

    def __init__(self):
        super().__init__("lisp dump-bc", gdb.COMMAND_DATA, gdb.PARAM_BOOLEAN)
        self.bp = None

    def get_set_string(self):
        class Bp(gdb.Breakpoint):
            def stop(self):
                chunk = gdb.selected_frame().read_var("chunk")
                print(LispChunkPrinter(chunk).to_string())
                return False

        if self.bp:
            self.bp.enabled = self.value
        elif self.value:
            self.bp = Bp("src/vm.c:run", gdb.BP_BREAKPOINT, gdb.WP_WRITE, internal=True)
        return ""


class DumpIrParam(gdb.Parameter):
    """Dump recorded IR with interleaved snapshots."""

    def __init__(self):
        super().__init__("lisp dump-ir", gdb.COMMAND_DATA, gdb.PARAM_BOOLEAN)
        self.bp = None

    def get_set_string(self):
        class Bp0(gdb.Breakpoint):
            def stop(self):
                Bp1(gdb.selected_frame(), True)
                return False

        class Bp1(gdb.FinishBreakpoint):
            def stop(self):
                print(dump_trace(self.return_value))
                return False

        if self.bp:
            self.bp.enabled = self.value
        elif self.value:
            spec = "src/jit.c:assemble_trace"
            self.bp = Bp0(spec, gdb.BP_BREAKPOINT, gdb.WP_WRITE, internal=True)
        return ""


def lisp_printer_lookup(val):
    type = val.type.unqualified()
    if type.code != gdb.TYPE_CODE_PTR:
        return None
    match str(type.target().unqualified()):
        case "struct Chunk":
            return LispChunkPrinter(val)
        case "struct LispTrace":
            return LispTracePrinter(val)
        case _:
            return None


def register(objfile):
    if objfile is None:
        objfile = gdb
    objfile.pretty_printers.append(lisp_printer_lookup)


register(gdb.current_objfile())
gdb.ParameterPrefix("lisp", gdb.COMMAND_NONE)
DumpBcParam()
DumpIrParam()
