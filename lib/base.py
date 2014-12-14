class BaseModule:
	def __init__(self):
		self.func_dict = self.initFuncDict()

	def initFuncDict(self):
		return {}

	def searchFunc(self, name):
		return self.func_dict[name]

class BaseFunc:
	def invoke(self, args):
		pass
