from root import W_Root

# the inner object should never call clone function 
def raise_inner_copy_error():
	raise Exception("shouldn't call clone function for an inner object anyway")

def raise_inner_is_equal_error():
	raise Exception("shouldn't call is_equal function for an inner object anyway")

class W_AddrObject(W_Root):
	def __init__(self, intval):
		assert(isinstance(intval, int))
		self.addrval = intval

	def clone(self):
		raise_inner_copy_error()

	def is_equal(self, other):
		raise_inner_is_equal_error()

class W_CodeParserWrapperObject(W_Root):
	def __init__(self, cp):
		self.cp = cp

	def clone(self):
		raise_inner_copy_error()

	def is_equal(self, other):
		raise_inner_is_equal_error()
