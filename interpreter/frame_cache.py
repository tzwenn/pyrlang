from pyrlang.interpreter.datatypes.inner import W_AddrObject
from rpython.rlib import jit

cache_size = 10

class FrameCache:
	_immutable_fields_ = ['cache[*]']
	def __init__(self):
		self.cache = [None] * cache_size
		self.position = 0

	def increase_postion(self):
		self.position += 1
		if self.position >= cache_size - 1:
			self.position = 0

	@jit.unroll_safe
	def try_cache(self, cp, pc):
		for addr in self.cache:
			if addr:
				if addr.pc == pc and addr.cp is cp:
					return addr
			else:
				return self._create_new_cache(cp, pc)
		return self._create_new_cache(cp, pc)

	def _create_new_cache(self, cp, pc):
		new_addr = W_AddrObject(cp, pc)
		self.cache[self.position] = new_addr
		self.increase_postion()
		return new_addr
