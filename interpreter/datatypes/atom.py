from root import W_Root
from rpython.rlib import jit

class W_AbstractAtomObject(W_Root):
	def get_str(self):
		return ""

	@jit.unroll_safe
	def to_list(self):
		return [ord(c) for c in self.get_str()]

class W_StrAtomObject(W_AbstractAtomObject):
	_immutable_fields_ = ['strval']
	def __init__(self, val):
		#assert(isinstance(val, str))
		self.strval = val

	def clone(self):
		return W_StrAtomObject(self.strval)

	def is_equal(self, other):
		if isinstance(other, W_StrAtomObject):
			return self.strval == other.strval
		else:
			return False

	def get_str(self):
		return self.strval

class W_IndexAtomObject(W_AbstractAtomObject):
	_immutable_fields_ = ['index','table_pointer']
	def __init__(self, v, p):
		self.index = v
		self.table_pointer = p

	def clone(self):
		return W_IndexAtomObject(self.index, self.table_pointer)

	def is_equal(self, other):
		if isinstance(other, W_IndexAtomObject):
			if self is other:
				return True
			if self.index == other.index and self.table_pointer is other.table_pointer:
				return True
			if self.get_str() == other.get_str():
				return True
			return False
		elif isinstance(other, W_StrAtomObject):
			return self.get_str() == other.strval
		else:
			return False

	def get_str(self):
		return self.table_pointer[self.index]
