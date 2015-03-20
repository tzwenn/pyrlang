from rpython.rlib import jit
class Instruction:
	_immutable_fields_ = ['opcode', 'args[*]']
	def __init__(self, opcode, args):
		self.opcode = opcode
		self.args = args

	@jit.elidable
	def arg_values(self):
		return [arg[1] for arg in self.args]

class ListInstruction(Instruction):
	_immutable_fields_ = ['lst']
	def __init__(self, opcode, args, lst):
		Instruction.__init__(self, opcode, args)
		self.lst = lst
