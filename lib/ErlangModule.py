from pyrlang.lib.base import BaseModule, BaseFunc
from pyrlang.interpreter.datatypes.number import *

class ModuleEntity(BaseModule):
	def initFuncDict(self):
		return { "+_2" : AddFunc,
				 "-_2" : SubFunc,
				 "*_2" : MulFunc,
				 "get_module_info_1" : GetModuleInfoFunc_1,
				 "get_module_info_2" : GetModuleInfoFunc_2}
	
class AddFunc(BaseFunc):
	def invoke(self, args):
		(a,b) = args
		return a.add(b)

class SubFunc(BaseFunc):
	def invoke(self, args):
		(a,b) = args
		return a.sub(b)

class MulFunc(BaseFunc):
	def invoke(self, args):
		(a,b) = args
		return a.mul(b)

class GetModuleInfoFunc_1(BaseFunc):
	def invoke(self,arg):
		return W_FloatObject(3.1415926)

class GetModuleInfoFunc_2(BaseFunc):
	pass
