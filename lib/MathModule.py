import math
from pyrlang.lib.base import BaseModule, BaseBIF, BaseFakeFunc, WrapEtermBoolean
from pyrlang.interpreter.datatypes.number import W_AbstractNumberObject, W_FloatObject

class SqrtFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		arg = process.x_reg.get(0)
		assert isinstance(arg, W_AbstractNumberObject)
		return W_FloatObject(math.sqrt(arg.tofloat()))

class ModuleEntity(BaseModule):
	_func_dict = { "sqrt_1" : SqrtFunc_1 }

	def initFuncDict(self):
		return self._func_dict
