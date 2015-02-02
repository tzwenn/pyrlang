from root import W_Root
from number import W_IntObject

class W_TupleObject(W_Root):
	def __init__(self, vals):
		assert(isinstance(vals, list))
		self.vals = vals

	def element_from_int_obj(self, index):
		assert isinstance(index, W_IntObject)
		return self.element(index.intval)

	def element(self, index):
		assert isinstance(index, int)
		return self.vals[index]

	def size_to_int_obj(self):
		return W_IntObject(self.size())

	def size(self):
		return len(self.vals)

	def clone(self):
		return W_TupleObject([val.clone() for val in self.vals])

	def is_equal(self, other):
		if isinstance(other, W_TupleObject):
			if self.size() == other.size():
				for i in range(0, self.size()):
					if not self.element(i).is_equal(other.element(i)):
						return False
				return True
			else:
				return False
		else:
			return False
