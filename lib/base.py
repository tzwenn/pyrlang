class BaseModule:
	def __init__(self):
		self.func_dict = self.initFuncDict()

	def initFuncDict(self):
		return {}

	def searchFunc(self, name):
		return self.func_dict[name]

class BaseFunc:
	_attrs_ = ()

class BaseBIF(BaseFunc):
	def invoke(self, args):
		pass

class BaseBIF0(BaseBIF):
	def set_caller(self, caller):
		self.caller = caller

# for the BIF which is called with call_ext_*
class BaseFakeFunc(BaseFunc):
	def invoke(self, cp, pc, runtime):
		pass
