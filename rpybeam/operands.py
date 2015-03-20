class Operand:
	def __init__(self, tag):
		self.tag = tag

	def get_tag(self):
		return self.tag

	def get_val(self):
		pass

class BaseOperand(Operand):
	def __init__(self, tag, numval):
		Operand.__init__(self, tag)
		self.numval = numval

	def get_val(self):
		return self.numval

class FloatOperand(Operand):
	def __init__(self, tag, floatval):
		Operand.__init__(self, tag)
		self.floatval = floatval

	def get_val(self):
		return self.floatval

class SelectListOperand(Operand):
	def __init__(self, tag, sl_val):
		Operand.__init__(self, tag)
		self.sl_val = sl_val

	def get_val(self):
		return self.sl_val

class AllocListOperand(Operand):
	def __init__(self, tag, al_val):
		Operand.__init__(self, tag)
		self.al_val = al_val

	def get_val(self):
		return self.al_val

class LiteralOperand(Operand):
	def __init__(self, tag, l_val):
		Operand.__init__(self, tag)
		self.l_val = l_val

	def get_val(self):
		return self.l_val
