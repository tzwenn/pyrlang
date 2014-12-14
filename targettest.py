import sys
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')

from rpybeam.beam_file import BeamRoot
from rpybeam.pretty_print import *
from rpybeam.beam_code import CodeParser
from interpreter.interp import BeamRunTime
from interpreter.datatypes.number import *

def main(argv):
	f = open(argv[1], "rb")
	try:
		s = BeamRoot(f)
		#print_Root(s)
		code = s.getCode()
		at = s.getAtomTable()
		cp = CodeParser(code,at,"module_info")
		lt = cp.createLabelTable()
		#print_labelTable(lt)
		#print cp.entry_addr
		brt = BeamRunTime(cp, at, s.impTChunk.asArray())
		res = brt.func_list[0].invoke([W_IntObject(3),
			W_IntObject(5)])
		print "res in invoke: %d"%(res.intval)
		print brt.func_list[1].invoke([W_FloatObject(0.1)]).floatval
	finally:
		f.close()
	return 0

def target(driver, args):
	return main, None

if __name__ == '__main__':
	import sys
	main(sys.argv)
