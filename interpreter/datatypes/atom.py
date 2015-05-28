from root import W_Root
from rpython.rlib import jit

#class W_AbstractAtomObject(W_Root):
	#def get_str(self):
		#return ""

	#@jit.unroll_safe
	#def to_list(self):
		#return [ord(c) for c in self.get_str()]

#class W_StrAtomObject(W_AbstractAtomObject):
	#_immutable_fields_ = ['strval']
	#def __init__(self, val):
		##assert(isinstance(val, str))
		#self.strval = val

	#def clone(self):
		#return W_StrAtomObject(self.strval)

	#def is_equal(self, other):
		#if isinstance(other, W_StrAtomObject):
			#return self.strval == other.strval
		#else:
			#return False

	#def get_str(self):
		#return self.strval

class W_AtomObject(W_Root):
	_immutable_fields_ = ['index']
	def __init__(self, v):
		self.index = v

	def clone(self):
		return self

	def is_equal(self, other):
		return self is other

	def get_str(self):
		from pyrlang.interpreter.atom_table import global_atom_table
		return global_atom_table.get_str_at(self.index)

	@jit.unroll_safe
	def to_list(self):
		return [ord(c) for c in self.get_str()]

class W_BoolAtomObject(W_AtomObject):
	pass
