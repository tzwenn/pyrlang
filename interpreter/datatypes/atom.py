from root import W_Root

class W_AtomObject(W_Root):
	def __init__(self, val):
		assert(isinstance(val, str))
		self.strval = val

	def clone(self):
		return W_AtomObject(self.strval)

	def is_equal(self, other):
		if isinstance(other, W_AtomObject):
			return self.strval == other.strval
		else:
			return False

	def to_list(self):
		return [ord(c) for c in self.strval]
