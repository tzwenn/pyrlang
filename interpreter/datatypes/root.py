class W_Root:
	_attrs_ = ()

	def clone(self):
		return W_Root()

	def is_equal(self, other):
		return False
