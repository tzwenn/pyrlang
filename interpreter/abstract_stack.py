from rpython.rlib import jit
def create_stack(init_size = 8):
	class AbstractStack:
		def __init__(self):
			self.vals = [None] * init_size
			self.top = -1
			self.size = jit.hint(init_size, promote=True)

		def push(self, val):
			self.top += 1
			if self.top >= self.size:
				self.vals = self.vals + [None] * self.size
				self.size = self.size << 1
			self.vals[top] = val

		def pop(self):
			self.top -= 1
			return self.vals[top]

		def get(self,n):
			return self.vals[n]

		def is_empty(self):
			return self.top == 0

		def depth(self):
			return self.top + 1

		def store(self, n, val):
			self.val[n] = val
	return AbstractStack
