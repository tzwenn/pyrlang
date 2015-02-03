from pyrlang.lib.base import BaseModule, BaseBIF, BaseFakeFunc
from pyrlang.interpreter import fail_class
from pyrlang.interpreter.datatypes.number import *
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from pyrlang.rpybeam import pretty_print
from pyrlang.utils.eterm_operators import *
from pyrlang.interpreter import constant


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

class DisplayFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		v = process.x_reg.get(0)
		pretty_print.print_value(v)
		return W_AtomObject('true')

class ElementFunc_2(BaseBIF):
	def invoke(self, args):
		(index, tuple_val) = args
		assert isinstance(tuple_val, W_TupleObject)
		return tuple_val.element_from_int_obj(index)

class GetModuleInfoFunc_1(BaseBIF):
	def invoke(self,arg):
		return W_FloatObject(3.1415926)

class GetModuleInfoFunc_2(BaseBIF):
	def invoke(self,arg):
		return W_FloatObject(2.3333333)

class RemFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.rem(b)

class SelfFunc_0(BaseBIF):
	def invoke(self, args):
		return self.caller.pid

class SpawnFunc_3(BaseFakeFunc):
	def _invoke(self, module_name, func_name, args, cp, process):
		mod_cp = cp.get_module_cp_by_name(module_name)
		func_addr = cp.get_func_addr_by_name(func_name, len(args))
		return process._spawn(mod_cp, func_addr, args, constant.PRIORITY_NORMAL)

	def invoke(self, cp, pc, process):
		from pyrlang.lib.ModuleDict import module_dict, is_bif, get_bif_name
		module_name = get_atom_val(process.x_reg.get(0))
		func_name = get_atom_val(process.x_reg.get(1))
		args = get_list_contents(process.x_reg.get(2))
		if module_name in module_dict:
			if is_bif(module_name, func_name, len(args)):
				process.init_entry_arguments(args)
				# it's not so efficient, but whatever, calling 
				# bif in spawn itself is stupid, too.
				fake_func = module_dict[module_name]._func_dict[get_bif_name(func_name, len(args))]()
				assert isinstance(fake_func, BaseFakeFunc)
				_ = fake_func.invoke(cp, pc, process)
				return process.scheduler.create_pid()
			else:
				return self._invoke(module_name, func_name, args, cp, process)
		else:
			return self._invoke(module_name, func_name, args, cp, process)

class TupleSizeFunc_1(BaseBIF):
	def invoke(self, args):
		tuple_val = args[0]
		assert isinstance(tuple_val, W_TupleObject)
		return tuple_val.size_to_int_obj()

class ModuleEntity(BaseModule):
	_func_dict = { "+_2" : AddFunc,
				  "-_2" : SubFunc,
				  "*_2" : MulFunc,
				  "error_1" : ErrorFunc_1,
				  "display_1" : DisplayFunc_1,
				  "element_2" : ElementFunc_2,
				  "get_module_info_1" : GetModuleInfoFunc_1,
				  "get_module_info_2" : GetModuleInfoFunc_2,
				  "rem_2" : RemFunc_2,
				  "self_0" : SelfFunc_0,
				  "spawn_3" : SpawnFunc_3,
				  "tuple_size_1" : TupleSizeFunc_1}

	def initFuncDict(self):
		return self._func_dict	
