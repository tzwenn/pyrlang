from root import W_Root

class W_ClosureObject(W_Root):
	def __init__(self, cp, pc, arity, fv_lst):
		self.cp = cp
		self.pc = pc
		self.arity = arity
		self.fv_lst = fv_lst

	def free_variables(self):
		return self.fv_lst

	def position(self):
		return (self.cp, self.pc)
