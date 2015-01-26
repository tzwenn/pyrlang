from root import W_Root

class W_AddrObject(W_Root):
	def __init__(self, intval):
		assert(isinstance(intval, int))
		self.addrval = intval

class W_CodeParserWrapperObject(W_Root):
	def __init__(self, cp):
		self.cp = cp
