from root import W_Root

class W_AtomObject(W_Root):
	def __init__(self, index):
		assert(isinstance(index, int))
		self.indexval = index
