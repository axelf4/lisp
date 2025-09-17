# Run with gdb -iex 'add-auto-load-safe-path /home/axel/vc/lisp/lisp-gdb.py' build/lisp

import gdb

IR_MARK = 0x80

SsaInstructionPtr = gdb.lookup_type("union SsaInstruction").pointer()
SnapshotPtr = gdb.lookup_type("struct Snapshot").pointer()


def print_trace(trace):
    type_names = ["AxB", "sym", "str", "cfn", "clo", None, None, "nil", "int", "___"]

    print("---- TRACE IR");
    num_snapshots = int(trace["num_snapshots"])
    length = int(trace["len"])
    num_consts = int(trace["num_consts"])
    print("len:", length, "num_consts:", num_consts, "num_snapshots", num_snapshots)
    instructions = trace["data"].cast(SsaInstructionPtr)
    snapshots = (instructions + (num_consts + length)).cast(SnapshotPtr)
    print("snapshots:", snapshots)

    i = 0
    snap_idx = 0
    while True:
        snap = snapshots[snap_idx]
        if snap_idx < num_snapshots and i >= snap["ir_start"]:
            print(f"....         SNAP  #{snap_idx:<3} [ ]")
            snap_idx += 1

        if i >= length:
            break

        x = instructions[num_consts + i]
        ty = int(x["ty"]) & ~IR_MARK
        print(f"{i:04} {type_names[ty]} {int(x["reg"]):3} ");
        i += 1

    snap = snapshots[1]
    print(snap)


class LispPrintTrace(gdb.Command):
    def __init__(self):
        gdb.Command.__init__ (self, "lisp-print-trace", gdb.COMMAND_DATA, gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        trace = gdb.parse_and_eval(args)
        print_trace(trace)

        # frame = Frame.get_selected_python_frame()
        # if not frame:
        #     print('Unable to locate python frame')
        #     return

        # pyop_frame = frame.get_pyop()
        # if not pyop_frame:
        #     print(UNABLE_READ_INFO_PYTHON_FRAME)
        #     return

        # pyop_var, scope = pyop_frame.get_var_by_name(name)

        # if pyop_var:
        #     print('%s %r = %s'
        #            % (scope,
        #               name,
        #               pyop_var.get_truncated_repr(MAX_OUTPUT_LEN)))
        # else:
        #     print('%r not found' % name)

LispPrintTrace()

# gdb.current_objfile().pretty_printers += pretty_printer_lookup

print("hej2")
