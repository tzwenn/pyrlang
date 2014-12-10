from beam_file import BaseNode
import opcodes

class BeamInstr(BaseNode):
	def parse(stream):
		self.opcode = self.readUCInt(stream)
		self.oprands = []
		for i = range(0, opcodes.arity[self.opcode]):
			rand = BeamOperand(stream)
			self.readlen += rand.readlen
			self.oprands.append(rand)

class BeamOperand(BaseNode):
	def parse(stream):
		

