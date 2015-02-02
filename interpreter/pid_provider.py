from pyrlang.interpreter.datatypes.pid import W_PidObject

class PidProvider:
	def __init__(self):
		self.process_num = 0

	def create_pid(self):
		pid = W_PidObject(0, self.process_num, 0)
		self.process_num += 1
		return pid
