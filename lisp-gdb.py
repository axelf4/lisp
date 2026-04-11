"""
GDB Python support script for Lisp.

See: https://sourceware.org/gdb/current/onlinedocs/gdb.html/Python.html
"""

from enum import IntEnum
import gdb
from gdb.types import make_enum_dict


def disassemble(n, xs, indent=0):
    """Disassemble Lisp bytecode."""

    def k():
        return (xs + i - b).cast(LispObjectPtr).dereference()

    def k_sym():
        sym = (k() - 1).cast(LispSymbolPtr)
        return sym["name"].string("utf-8", "replace", sym["len"])

    LispObjectPtr = gdb.lookup_type("LispObject").pointer()
    LispSymbolPtr = gdb.lookup_type("struct LispSymbol").pointer()
    Prototype = gdb.lookup_type("struct Prototype")
    Op = IntEnum("Op", make_enum_dict(gdb.lookup_type("enum Op")))

    s = ""
    i = 0
    while i < n:
        x = xs[i]
        i += 1
        a = int(x["a"])
        b = int(x["b"])
        c = int(x["c"])

        s += " " * indent + f"{i:04} "
        match x["op"]:
            case Op.MOV:
                s += f"MOV {a} <- {c}\n"
            case Op.LOADNIL:
                s += f"LOADNIL {a} <- NIL\n"
            case Op.LOADOBJ:
                s += f"LOADOBJ {a} <- {int(k()):x}\n"
            case Op.LOADSHORT:
                s += f"LOADSHORT {a} <- {(b ^ 0x8000) - 0x8000}\n"
            case Op.GETGLOBAL:
                s += f"GETGLOBAL {a} <- [{k_sym()}]\n"
            case Op.SETGLOBAL:
                s += f"SETGLOBAL {a} -> [{k_sym()}]\n"
            case Op.GETUPVALUE:
                s += f"SETUPVALUE {a} <- {c}\n"
            case Op.SETUPVALUE:
                s += f"SETUPVALUE {a} -> {c}\n"
            case Op.JMP:
                s += f"JMP => {i + b:04}\n"
            case Op.JNIL:
                s += f"JMP if {a} == NIL => {i + b:04}\n"
            case Op.CALL | Op.TAILCALL:
                prefix = "TAIL" if x["op"] == Op.TAIL_CALL else ""
                args = "".join(f" {a + 2 + j}" for j in range(c))
                s += f"{prefix}CALL {a} <- ({a}{args})\n"
            case Op.RET:
                s += f"RET {a}\n"
            case Op.FNEW:
                proto = (xs + i).cast(Prototype.pointer())
                arity = int(proto["arity"])
                num_upvals = int(proto["num_upvalues"])
                s += f"FNEW {a} <- (arity: {arity} num_upvals: {num_upvals}):\n"
                metadata_size = Prototype.sizeof + num_upvals + x.type.sizeof - 1
                body = proto["body"].dereference().address
                s += disassemble(b - metadata_size // x.type.sizeof, body, indent + 2)
                i += b
            case Op.CLO:
                s += "CLO >= {a}\n"
            case Op.FHDR | Op.FHDR_INTERPR | Op.FHDR_JIT:
                s += "FHDR\n"
            case _:
                raise ValueError("Invalid operation code")
    return s


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


def lisp_printer_lookup(val):
    type = val.type.unqualified()
    if (
        type.code == gdb.TYPE_CODE_PTR
        and str(type.target().unqualified()) == "struct Chunk"
    ):
        return LispChunkPrinter(val)
    else:
        return None


def register(objfile):
    if objfile is None:
        objfile = gdb
    objfile.pretty_printers.append(lisp_printer_lookup)


register(gdb.current_objfile())
