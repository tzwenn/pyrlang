from pyrlang.lib.base import BaseModule, BaseBIF, BaseFakeFunc
from pyrlang.interpreter import fail_class
from pyrlang.interpreter.datatypes.number import *
from pyrlang.interpreter.datatypes.tuple import W_TupleObject

class ModuleEntity(BaseModule):
	def initFuncDict(self):
		return { "+_2" : AddFunc,
				 "-_2" : SubFunc,
				 "*_2" : MulFunc,
				 "error_1" : ErrorFunc_1,
				 "element_2" : ElementFunc_2,
				 "get_module_info_1" : GetModuleInfoFunc_1,
				 "get_module_info_2" : GetModuleInfoFunc_2,
				 "tuple_size_1" : TupleSizeFunc_1}
	
class AddFunc(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.add(b)

class SubFunc(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.sub(b)

class MulFunc(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.mul(b)

# just a hook 
class ErrorFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, runtime):
		reason = runtime.x_reg.get(0)
		return runtime.fail(cp, pc, fail_class.ERROR, reason)

class ElementFunc_2(BaseBIF):
	def invoke(self, args):
		(index, tuple_val) = args
		assert isinstance(tuple_val, W_TupleObject)
		return tuple_val.element(index)

class GetModuleInfoFunc_1(BaseBIF):
	def invoke(self,arg):
		return W_FloatObject(3.1415926)

class GetModuleInfoFunc_2(BaseBIF):
	pass

class TupleSizeFunc_1(BaseBIF):
	def invoke(self, args):
		tuple_val = args[0]
		assert isinstance(tuple_val, W_TupleObject)
		return tuple_val.size()
