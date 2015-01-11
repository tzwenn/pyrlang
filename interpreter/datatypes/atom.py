from root import W_Root

class W_AtomObject(W_Root):
	def __init__(self, val):
		assert(isinstance(val, str))
		self.strval = val
