from pyrlang.interpreter.datatypes.pid import W_PidOjbect

class PidProvider:
	def __init__(self):
		self.process_num = 0

	def create_pid(self):
		pid = W_PidOjbect(0, self.process_num, 0)
		self.process_num += 1
		return pid
