import sys
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')

from beam_file import BeamRoot
from pretty_print import *
from beam_code import CodeParser

def main(argv):
	f = open(argv[1], "rb")
	try:
		s = BeamRoot(f)
		print_Root(s)
		code = s.getCode()
		at = s.getAtomTable()
		cp = CodeParser(code,at,"module_info")
		lt = cp.createLabelTable()
		print_labelTable(lt)
		print cp.entry_addr
	finally:
		f.close()
	return 0

def target(driver, args):
	return main, None

if __name__ == '__main__':
	import sys
	main(sys.argv)
