from root import W_Root

class W_IntObject(W_Root):
	def __init__(self, intval):
		self.intval = intval

	def add(self, other): 
		assert isinstance(other, W_IntObject)
		return W_IntObject(self.intval + other.intval)

	def mul(self, other): 
		assert isinstance(other, W_IntObject)
		return W_IntObject(self.intval * other.intval)

	def sub(self, other):
		assert isinstance(other, W_IntObject)
		return W_IntObject(self.intval - other.intval)

	def rem(self, other):
		assert isinstance(other, W_IntObject)
		return W_IntObject(self.intval % other.intval)

	def lt(self, other): 
		assert isinstance(other, W_IntObject)
		return self.intval < other.intval

	def is_true(self):
		return self.intval != 0

	def str(self):
		return str(self.intval)

	def getval(self):
		return self.intval

	def clone(self):
		return W_IntObject(self.intval)

	def is_equal(self, other):
		if isinstance(other, W_IntObject):
			return self.intval == other.intval
		else:
			return False


class W_FloatObject(W_Root):
	def __init__(self, floatval):
		assert(isinstance(floatval, float))
		self.floatval = floatval

	def add(self, other):
		if not isinstance(other, W_FloatObject):
			raise Exception("wrong type")
		return W_FloatObject(self.floatval + other.floatval)

	def lt(self, other): 
		if not isinstance(other, W_FloatObject):
			raise Exception("wrong type")
		return self.floatval < other.floatval

	def str(self):
		return str(self.floatval)

	def getval(self):
		return self.floatval

	def clone(self):
		return W_FloatObject(self.floatval)

	def is_equal(self, other):
		if isinstance(other, W_FloatObject):
			return self.floatval == other.floatval
		else:
			return False
