import os
from pyrlang.rpybeam.beam_file import BeamRoot

class ModFileLoader:
	def find(self, module_name):
		b = None
		fname = "test_beam/" + module_name + ".beam"
		if os.path.isfile(fname):
			f = open(fname, "rb")
		else:
			f = open("lib/lib_beam/" + module_name + ".beam", "rb")
		try:
			b = BeamRoot(f)
		finally:
			f.close()
		return b
