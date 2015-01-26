import sys
# used for my MacBook
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')
# used for lab's MacPro
sys.path.append('/Users/huangruochen/src/python/pypy')
sys.path.append('../')

from pyrlang.rpybeam.beam_file import BeamRoot
from pyrlang.rpybeam.pretty_print import *
from pyrlang.rpybeam.beam_code import CodeParser
from pyrlang.interpreter.interp import BeamRunTime
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
		b = BeamRoot(f)
		brt = BeamRunTime()
		brt.init_entry_arguments(args)
		cp = CodeParser(b, argv[1])
		index = cp.search_exports(entry_func, len(args), cp.export_header, cp.atoms)
		func_addr = cp.label_to_addr(cp.export_header[index][2])
		try:
			res = brt.execute(cp, func_addr)
		except:
			return 0

		print_value(res)
	finally:
		f.close()
	return 0

def target(driver, args):
	return main, None

if __name__ == '__main__':
	import sys
	main(sys.argv)
