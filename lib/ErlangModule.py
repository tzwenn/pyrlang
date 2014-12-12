from base import BaseModule, BaseFunc

class ErlangModule(BaseModule):
	def __init__(self):
		self.func_dict = [ "+" : AddFunc ]

	def searchFunc(self, name):
		return self.func_dict[name]
	
class AddFunc(BaseFunc):
	def 
