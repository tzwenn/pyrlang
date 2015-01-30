from root import W_Root
from number import W_IntObject

class W_TupleObject(W_Root):
	def __init__(self, vals):
		self.vals = vals

	def element(self, index):
		assert isinstance(index, W_IntObject)
		return self.vals[index.intval]

	def size(self):
		return W_IntObject(len(self.vals))

	def clone(self):
		return W_TupleObject([val.clone() for val in self.vals])
