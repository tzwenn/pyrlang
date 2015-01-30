from root import W_Root

class W_PidOjbect(W_Root):
	def __init__(self, node_num, process_num, serial):
		assert isinstance(node_num, int)
		assert isinstance(process_num, int)
		assert isinstance(serial, int)
		self.node_num = node_num
		self.process_num = process_num
		self.serial = serial

	def is_equal(self, other):
		assert isinstance(other, W_PidOjbect)
		return self.node_num == other.node_num and self.process_num == other.process_num and self.serial == other.serial

	def clone(self):
		return W_PidOjbect(self.node_num, self.process_num, self.serial)
