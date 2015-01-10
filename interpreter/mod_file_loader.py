from pyrlang.rpybeam.beam_file import BeamRoot

class ModFileLoader:
	def find(self, module_name):
		b = None
		f = open("test_beam/" + module_name + ".beam", "rb")
		try:
			b = BeamRoot(f)
		finally:
			f.close()
		return b
