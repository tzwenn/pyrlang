from root import W_Root
from rpython.rlib.rbigint import (
		InvalidEndiannessError, InvalidSignednessError, rbigint)

class W_BigIntObject(W_Root):
	def __init__(self, intval):
		self.bigintval = rbigint.fromint(intval)

	def add(self, other):
		assert isinstance(other, W_BigIntObject)
		return W_BigIntObject(self.bigintval.add(other.bigintval))

	def sub(self, other):
		assert isinstance(other, W_BigIntObject)
		return W_BigIntObject(self.bigintval.sub(other.bigintval))

	def mul(self, other):
		assert isinstance(other, W_BigIntObject)
		return W_BigIntObject(self.bigintval.mul(other.bigintval))

	def div(self, other):
		assert isinstance(other, W_BigIntObject)
		return W_BigIntObject(self.bigintval.div(other.bigintval))

	def rem(self, other):
		assert isinstance(other, W_BigIntObject)
		return W_BigIntObject(self.bigintval.mod(other.bigintval))

	def lt(self, other):
		assert isinstance(other, W_BigIntObject)
		return self.bigintval.lt(other.bigintval)

	def to_list(self):
		lst = list(self.bigintval.str())
		return [ord(s[0]) for s in res]

	def is_rough_equal(self, other):
		# TODO: add rough equal for big float (if any)
		if isinstance(other, W_BigIntObject):
			return self.is_equal(other)
		else:
			return False

	def is_equal(self, other):
		assert isinstance(other, W_BigIntObject)
		return self.bigintval.eq(other.bigintval)
