import sys
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')
sys.path.append('../')

from rpybeam.beam_file import BeamRoot
from pyrlang.rpybeam.pretty_print import *
from rpybeam.beam_code import CodeParser
from interpreter.interp import BeamRunTime
from pyrlang.interpreter.datatypes.number import *

def parse_arg(argv):
	arg_len = len(argv) - 2
	entry_func = "start"
	args = []
	if arg_len > 0:
		entry_func = argv[2]
		if arg_len > 1:
			for i in range(0, arg_len - 1):
				args.append(W_IntObject(int(argv[3+i])))
	return (entry_func, args)

def main(argv):
	f = open(argv[1], "rb")
	(entry_func, args) = parse_arg(argv)
	try:
		s = BeamRoot(f)
		#print_Root(s)
		code = s.getCode()
		at = s.getAtomTable()

		#print_labelTable(cp.labelTable)
		#print cp.entry_addr
		brt = BeamRunTime(at, s.impTChunk.asArray())
		brt.init_entry_arguments(args)
		res = brt.execute(code, entry_func, len(args))
		print_value(res)
		#res = brt.func_list[0].invoke([W_IntObject(3),
			#W_IntObject(5)])
		#print "res in invoke: %d"%(res.intval)
		#print brt.func_list[1].invoke([W_FloatObject(0.1)]).floatval
	finally:
		f.close()
	return 0

def target(driver, args):
	return main, None

if __name__ == '__main__':
	import sys
	main(sys.argv)
