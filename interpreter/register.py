from pyrlang.interpreter.datatypes.number import W_IntObject
class AbstractRegister:
	def get(self, n):
		pass

	def store(self, n, val):
		pass

class X_Register(AbstractRegister):
	_virtualizable_ = ['regs[*]']

	def __init__(self):
		self.regs = []

	def get(self, n):
		return self.regs[n]

	def store(self, n, val):
		regs_len = len(self.regs)
		if n >= regs_len:
			for i in range(0, n - regs_len + 1):
				self.regs.append(W_IntObject(-1))
		self.regs[n] = val

class Y_Register(AbstractRegister):
	_virtualizable_ = ['regs[*]']

	def __init__(self):
		self.regs = []

	def get(self, n):
		return self.regs[-(n+1)]

	def store(self, n, val):
		self.regs[-(n+1)] = val

	def pop(self):
		return self.regs.pop()

	def push(self, value):
		self.regs.append(value)

	def is_empty(self):
		return len(self.regs) == 0

