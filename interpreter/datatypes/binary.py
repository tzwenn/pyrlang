from root import W_Root
from rpython.rlib.rstring import StringBuilder

class W_BinaryObject(W_Root):
	# size by byte!
	def __init__(self, size=0, init=''):
		raise NotImplementedError
