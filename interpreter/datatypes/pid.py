from root import W_Root

class W_PidObject(W_Root):
	def __init__(self, node_num, process_num, serial):
		assert isinstance(node_num, int)
		assert isinstance(process_num, int)
		assert isinstance(serial, int)
		self.node_num = node_num
		self.process_num = process_num
		self.serial = serial

	def is_equal(self, other):
		if isinstance(other, W_PidObject):
			return self.node_num == other.node_num and self.process_num == other.process_num and self.serial == other.serial
		else:
			return False

	def clone(self):
		return W_PidObject(self.node_num, self.process_num, self.serial)
