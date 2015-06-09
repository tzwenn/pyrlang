from pyrlang.lib.base import BaseModule, BaseBIF, BaseFakeFunc, WrapEtermBoolean
from pyrlang.interpreter.datatypes.list import W_NilObject, W_ListObject
from pyrlang.utils import eterm_operators
from rpython.rlib import jit

class MemberFunc_2(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		elem = process.x_reg.get(0)
		lst = process.x_reg.get(1)
		assert isinstance(lst, W_ListObject) or isinstance(lst, W_NilObject)
		return WrapEtermBoolean(lst.member(elem))

class ReverseFunc_2(BaseFakeFunc):
	@jit.unroll_safe
	def invoke(self, cp, pc, process):
		v = process.x_reg.get(0)
		tail_list = process.x_reg.get(1)
		contents = eterm_operators.get_list_contents(v)
		# the traverse of list from the left side
		# so it reversed naturally
		for term in contents:
			tail_list = W_ListObject(term, tail_list)
		return tail_list

class ModuleEntity(BaseModule):
	_func_dict = { "reverse_2" : ReverseFunc_2,
			"member_2" : MemberFunc_2
			}

	def initFuncDict(self):
		return self._func_dict
