from root import W_Root
from rpython.rlib.rarithmetic import ovfcheck
from rpython.rlib import jit
from rpython.rlib.rbigint import (
		InvalidEndiannessError, InvalidSignednessError, rbigint)

class W_AbstractIntObject(W_Root):
	pass

class W_IntObject(W_AbstractIntObject):
	_immutable_fields_ = ['intval']

	def __init__(self, intval):
		self.intval = intval

	def toint(self):
		return self.intval

	def is_positive(self):
		return self.intval > 0

	def is_zero(self):
		return self.intval == 0

	def add(self, other): 
		if isinstance(other, W_IntObject):
			try:
				return W_IntObject(ovfcheck(self.intval + other.intval))
			except OverflowError:
				b_i = rbigint.fromint(self.intval)
				o_i = rbigint.fromint(other.intval)
				return W_BigIntObject(b_i.add(o_i))
		else:
			return self.to_bigint().add(other)

	def to_bigint(self):
		return W_BigIntObject(rbigint.fromint(self.intval))

	def mul(self, other): 
		if isinstance(other, W_IntObject):
			try: 
				return W_IntObject(ovfcheck(self.intval * other.intval))
			except OverflowError:
				b_i = rbigint.fromint(self.intval)
				o_i = rbigint.fromint(other.intval)
				return W_BigIntObject(b_i.mul(o_i))
		else:
			return self.to_bigint().mul(other)

	def sub(self, other):
		if isinstance(other, W_IntObject):
			try: 
				return W_IntObject(ovfcheck(self.intval - other.intval))
			except OverflowError:
				b_i = rbigint.fromint(self.intval)
				o_i = rbigint.fromint(other.intval)
				return W_BigIntObject(b_i.sub(o_i))
		else:
			return self.to_bigint().sub(other)

	def lshift(self, other):
		assert isinstance(other, W_IntObject)
		if other.intval > 0: 
			try:
				return W_IntObject(ovfcheck(self.intval << other.intval))
			except OverflowError:
				return self.to_bigint().lshift(other)
		else:
			return W_IntObject(self.intval >> -other.intval)

	def rshift(self, other):
		assert isinstance(other, W_IntObject)
		if other.intval > 0:
			return W_IntObject(self.intval >> other.intval)
		else:
			try:
				return W_IntObject(ovfcheck(self.intval << -other.intval))
			except OverflowError:
				return self.to_bigint().lshift(W_IntObject(-other.intval))

	# Never overflow
	def div(self, other):
		if isinstance(other, W_IntObject):
			return W_IntObject(self.intval / other.intval)
		else:
			assert isinstance(other, W_BigIntObject)
			return W_IntObject(0)

	# Never overflow
	def rem(self, other):
		if isinstance(other, W_IntObject):
			return W_IntObject(self.intval % other.intval)
		else:
			assert isinstance(other, W_BigIntObject)
			return W_IntObject(self.intval)

	def lt(self, other): 
		if isinstance(other, W_IntObject):
			return self.intval < other.intval
		else:
			assert isinstance(other, W_BigIntObject)
			if other.is_positive():
				return True
			else:
				return False

	@jit.unroll_safe
	def to_list(self):
		if self.intval == 0:
			return [0]
		else:
			res = []
			v = self.intval
			if self.intval < 0:
				res.append("-")
				v = -v
			while v != 0:
				res.append(str(v % 10))
				v = int(v / 10)
			res.reverse()
			return [ord(s[0]) for s in res]

	def str(self):
		return str(self.intval)

	def clone(self):
		return W_IntObject(self.intval)

	def is_rough_equal(self, other):
		if isinstance(other, W_IntObject):
			return self.is_equal(other)
		elif isinstance(other, W_FloatObject):
			return other.floatval == float(self.intval)
		else:
			return False

	def is_equal(self, other):
		if isinstance(other, W_IntObject):
			return self.intval == other.intval
		else:
			return False

class W_FloatObject(W_Root):
	_immutable_fields_ = ['floatval']
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

	def is_rough_equal(self, other):
		if isinstance(other, W_IntObject):
			try:
				return self.floatval == float(other.intval)
			except OverflowError:
				return False
		else:
			return self.is_equal(other)

	def is_equal(self, other):
		if isinstance(other, W_FloatObject):
			return self.floatval == other.floatval
		else:
			return False

class W_BigIntObject(W_AbstractIntObject):
	_immutable_fields_ = ['bigintval']
	def __init__(self, intval):
		print "big"
		self.bigintval = intval

	def str(self):
		return self.bigintval.str()

	def toint(self):
		return self.bigintval.toint()

	def return_wrap(self, bigint):
		try:
			return W_IntObject(bigint.toint())
		except OverflowError:
			return W_BigIntObject(bigint)

	def is_zero(self):
		return self.bigintval.sign == 0

	def is_positive(self):
		return self.bigintval.sign != -1

	def add(self, other):
		if isinstance(other, W_IntObject):
			return self._add(other.to_bigint())
		else:
			return self._add(other)

	def _add(self, other):
		assert isinstance(other, W_BigIntObject)
		return self.return_wrap(self.bigintval.add(other.bigintval))

	def sub(self, other):
		if isinstance(other, W_IntObject):
			return self._sub(other.to_bigint())
		else:
			return self._sub(other)

	def _sub(self, other):
		assert isinstance(other, W_BigIntObject)
		return self.return_wrap(self.bigintval.sub(other.bigintval))

	def mul(self, other):
		if isinstance(other, W_IntObject):
			return self._mul(other.to_bigint())
		else:
			return self._mul(other)

	def _mul(self, other):
		assert isinstance(other, W_BigIntObject)
		if other.is_zero():
			return W_IntObject(0)
		else:
			return self.return_wrap(self.bigintval.mul(other.bigintval))

	def div(self, other):
		if isinstance(other, W_IntObject):
			return self._div(other.to_bigint())
		else:
			return self._div(other)

	def _div(self, other):
		assert isinstance(other, W_BigIntObject)
		return self.return_wrap(self.bigintval.div(other.bigintval))

	def rem(self, other):
		if isinstance(other, W_IntObject):
			return self._rem(other.to_bigint())
		else:
			return self._rem(other)

	def _rem(self, other):
		assert isinstance(other, W_BigIntObject)
		return self.return_wrap(self.bigintval.mod(other.bigintval))

	def lshift(self, other):
		assert isinstance(other, W_IntObject)
		if other.intval > 0:
			return self.return_wrap(self.bigintval.lshift(other.intval))
		else:
			return self.return_wrap(self.bigintval.rshift(-other.intval))

	def rshift(self, other):
		assert isinstance(other, W_IntObject)
		if other.intval > 0:
			return self.return_wrap(self.bigintval.rshift(other.intval))
		else:
			return self.return_wrap(self.bigintval.lshift(-other.intval))

	def lt(self, other):
		if isinstance(other, W_IntObject):
			return self._lt(other.to_bigint())
		else:
			return self._lt(other)

	def _lt(self, other):
		assert isinstance(other, W_BigIntObject)
		return self.bigintval.lt(other.bigintval)

	@jit.unroll_safe
	def to_list(self):
		lst = list(self.bigintval.str())
		return [ord(s[0]) for s in lst]

	def is_rough_equal(self, other):
		# TODO: add rough equal for big float (if any)
		if isinstance(other, W_BigIntObject):
			return self.is_equal(other)
		else:
			return False

	def is_equal(self, other):
		if isinstance(other, W_BigIntObject):
			return self.bigintval.eq(other.bigintval)
		else:
			return False
