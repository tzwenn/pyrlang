from root import W_Root
from number import W_IntObject, W_AbstractIntObject
from rpython.rlib.unroll import unrolling_iterable
from interpreter import constant

class W_AbstractTupleObject(W_Root):
	def __init__(self, values):
		pass

	def element_from_int_obj(self, index):
		assert isinstance(index, W_AbstractIntObject)
		return self.element(index.toint()-1)

	def elements(self):
		raise NotImplementedError

	def element(self, index):
		raise NotImplementedError

	def setelement(self, index, v):
		raise NotImplementedError

	def size_to_int_obj(self):
		return W_IntObject(self.size())

	def size(self):
		raise NotImplementedError

	def clone(self):
		raise NotImplementedError

	def is_equal(self, other):
		if isinstance(other, W_AbstractTupleObject):
			if self.size() == other.size():
				for i in range(0, self.size()):
					if not self.element(i).is_equal(other.element(i)):
						return False
				return True
			else:
				return False
		else:
			return False

def W_0TupleObject(W_AbstractTupleObject):
	def elements(self):
		return []

	def element(self, index):
		raise IndexError

	def setelement(self, index, v):
		raise IndexError

	def size(self):
		return 0

	def clone(self):
		return W_0TupleObject()

def specialised_tuple_factory(tuple_size):
	assert tuple_size >= 0
	iter_n = unrolling_iterable(range(tuple_size))

	class cls(W_AbstractTupleObject):
		_immutable_fields_ = ['value%s' % i for i in iter_n]

		def __init__(self, values):
			assert len(values) == tuple_size
			for i in iter_n:
				setattr(self, 'value%s' % i, values[i])

		def element(self, index):
			for i in iter_n:
				if i == index:
					return getattr(self, 'value%s' % i) 
			else:
				raise IndexError

		def elements(self):
			res = [None] * tuple_size
			for i in iter_n:
				value = getattr(self, 'value%s' % i)
				res[i] = value
			return res

		def setelement(self, index, v):
			return self.__class__([self.element(i) if not i == index else v for i in iter_n])

		def size(self):
			return tuple_size

		def clone(self):
			return self.__class__(self.elements()[:])

	cls.__name__ = 'W_%sTupleObject' % tuple_size
	return cls

specialised_tuples = [specialised_tuple_factory(i) for i in unrolling_iterable(range(constant.TUPLE_S_SIZE+1))]

#class W_2TupleObject(W_AbstractTupleObject):
	#def __init__(self, val1, val2):
		#self.fst = val1
		#self.snd = val2

	#def elements(self):
		#return [self.fst, self.snd]

	#def element(self, index):
		#if index == 0:
			#return self.fst
		#elif index == 1:
			#return self.snd
		#else:
			#raise IndexError
	
	#def setelement(self, index, v):
		#if index == 0:
			#return W_2TupleObject(v, self.snd)
		#elif index == 1:
			#return W_2TupleObject(self.fst, v)
		#else:
			#raise IndexError

	#def size(self):
		#return 2

	#def clone(self):
		#return W_2TupleObject(self.fst, self.snd)

#class W_3TupleObject(W_AbstractTupleObject):
	#def __init__(self, val1, val2, val3):
		#self.val1 = val1
		#self.val2 = val2
		#self.val3 = val3

	#def elements(self):
		#return [self.val1, self.val2, self.val3]

	#def element(self, index):
		#if index == 0:
			#return self.val1
		#elif index == 1:
			#return self.val2
		#elif index == 2:
			#return self.val3
		#else:
			#raise IndexError

	#def setelement(self, index, v):
		#if index == 0:
			#return W_3TupleObject(v, self.val2, self.val3)
		#elif index == 1:
			#return W_3TupleObject(self.val1, v, self.val3)
		#elif index == 2:
			#return W_3TupleObject(self.val1, self.val2, v)
		#else:
			#raise IndexError

	#def size(self):
		#return 3

	#def clone(self):
		#return W_3TupleObject(self.val1, self.val2, self.val3)

class W_TupleObject(W_AbstractTupleObject):
	_immutable_fields_ = ['vals[*]']
	def __init__(self, vals):
		assert(isinstance(vals, list))
		self.vals = vals

	def elements(self):
		return self.vals

	def element(self, index):
		assert isinstance(index, int)
		return self.vals[index]

	def setelement(self, index, v):
		new_lst = list(self.vals)
		new_lst[index] = v
		return W_TupleObject(new_lst)

	def size(self):
		return len(self.vals)

	def clone(self):
		return W_TupleObject([val.clone() for val in self.vals])
