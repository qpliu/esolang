import os
import sys

import interp
import parser
import tokenizer

def write_value(value, stream):
    b = 0
    bit = 128
    while value.value() != None:
        if value.value():
            b = b | bit
        if bit == 1:
            stream.write(bytes([b]))
            bit = 128
            b = 0
        else:
            bit = bit >> 1
        value = value.next()
        
def parse_sources(files):
    fns = {}
    for file in files:
        with open(file) as f:
            parser.parse_file(fns, f)
    parser.parse_bodies(fns)
    return fns

def main(argv):
    source_files = []
    fn = None
    args = None
    stdin = interp.FileVal(sys.stdin.buffer)
    for arg in argv:
        if fn == None:
            if arg != '-':
                source_files.append(arg)
            else:
                fn = os.path.basename(source_files[0])
                if fn.find('.') > 0:
                    fn = fn[0:fn.find('.')]
        elif args == None:
            args = []
            fn = arg
        elif arg == '-':
            args.append(stdin)
        else:
            args.append(interp.FileVal(open(arg, "rb")))
    if fn == None:
        fn = os.path.basename(source_files[-1])
        if fn.find('.') > 0:
            fn = fn[0:fn.find('.')]
    if args == None:
        args = []
    fns = parse_sources(source_files)
    arity = len(fns[fn][0].patterns)
    if len(args) < arity:
        args.append(stdin)
    while len(args) < arity:
        args.append(interp.NilVal())
    write_value(interp.apply(fns, fn, args[0:arity]), sys.stdout.buffer)

main(sys.argv[1:])
