from root import W_Root
from rpython.rlib import jit

class W_NilObject(W_Root):
	def clone(self):
		return W_NilObject()

	def is_equal(self, other):
		return isinstance(other, W_NilObject)

	def length(self):
		return 0

	def member(self, v):
		return False

class W_ListObject(W_Root):
	_immutable_fields_ = ['left', 'right']

	def __init__(self, left, right = W_NilObject()):
		#if isinstance(left, W_NilObject):
			#print left 
			#print right
			#raise Exception("list head should not be nil!")
		#assert isinstance(right, W_NilObject) or isinstance(right, W_ListObject)
		self.left = left
		self.right = right

	def head(self):
		return self.left

	def tail(self):
		return self.right

	def clone(self):
		return W_ListObject(self.left.clone(), self.right.clone())

	def length(self):
		l = 1
		tail = self
		while isinstance(tail.tail(), W_ListObject):
			l += 1
			tail = tail.tail()
		return l

	def is_equal(self, other):
		if isinstance(other, W_ListObject):
			return self.left.is_equal(other.left) and self.right.is_equal(other.right)
		else:
			return False

	@jit.unroll_safe
	def member(self, v):
		curr = self
		while isinstance(curr, W_ListObject):
			#print curr.head().intval,v.intval
			if curr.head().is_equal(v):
				return True
			curr = curr.tail()
		return False

class W_StrListObject(W_ListObject):
	def clone(self):
		return W_StrListObject(self.left.clone(), self.right.clone())
