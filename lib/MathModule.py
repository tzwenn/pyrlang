import math
from pyrlang.lib.base import BaseModule, BaseBIF, BaseFakeFunc, WrapEtermBoolean
from pyrlang.interpreter.datatypes.number import W_AbstractNumberObject, W_FloatObject

class AtanFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		arg = process.x_reg.get(0)
		assert isinstance(arg, W_AbstractNumberObject)
		return W_FloatObject(math.atan(arg.tofloat()))

class Atan2Func_2(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		arg1 = process.x_reg.get(0)
		arg2 = process.x_reg.get(1)
		assert isinstance(arg1, W_AbstractNumberObject)
		assert isinstance(arg2, W_AbstractNumberObject)
		return W_FloatObject(math.atan2(arg1.tofloat(), arg2.tofloat()))

class CosFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		arg = process.x_reg.get(0)
		assert isinstance(arg, W_AbstractNumberObject)
		return W_FloatObject(math.cos(arg.tofloat()))

class SinFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		arg = process.x_reg.get(0)
		assert isinstance(arg, W_AbstractNumberObject)
		return W_FloatObject(math.sin(arg.tofloat()))

class SqrtFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		arg = process.x_reg.get(0)
		assert isinstance(arg, W_AbstractNumberObject)
		return W_FloatObject(math.sqrt(arg.tofloat()))

class ModuleEntity(BaseModule):
	_func_dict = { "atan_1": AtanFunc_1,
			"atan2_2": Atan2Func_2,
			"cos_1": CosFunc_1,
			"sin_1": SinFunc_1,
			"sqrt_1" : SqrtFunc_1 }

	def initFuncDict(self):
		return self._func_dict
