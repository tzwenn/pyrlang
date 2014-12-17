from root import W_Root

class W_NilObject(W_Root):
	pass

class W_ListObject(W_Root):
	def __init__(self, left, right = W_NilObject()):
		if isinstance(left, W_NilObject):
			raise Exception("list head should not be nil!")
		self.left = left
		self.right = right

	def head(self):
		return self.left

	def tail(self):
		return self.right
