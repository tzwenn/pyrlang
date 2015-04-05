from pyrlang.lib.base import BaseModule, BaseBIF, BaseBIF0, BaseFakeFunc
from pyrlang.interpreter import fail_class
from pyrlang.interpreter.datatypes.number import *
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from pyrlang.interpreter.datatypes.list import W_NilObject, W_ListObject, W_StrListObject
from pyrlang.interpreter.atom_table import global_atom_table
from pyrlang.interpreter import constant
from pyrlang.rpybeam import pretty_print
from pyrlang.utils import eterm_operators
from pyrlang.interpreter import constant
from rpython.rlib import jit

def WrapEtermBoolean(flag):
	if flag:
		return global_atom_table.TRUE_ATOM
	else:
		return global_atom_table.FALSE_ATOM

class AddFunc(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		#print a.str() + " + " + b.str() + " = " + a.add(b).str()
		return a.add(b)

class AtomToListFunc_1(BaseFakeFunc):
	@jit.unroll_safe
	def invoke(self, cp, pc, process):
		a_obj = process.x_reg.get(0)
		assert isinstance(a_obj, W_AtomObject)
		lst = a_obj.to_list()
		return eterm_operators.build_strlist_object([W_IntObject(v) for v in lst])

class BslFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.lshift(b)

class BsrFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.rshift(b)

class DivFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.div(b)

class SubFunc(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		#print a.str() + " + " + b.str() + " = " + a.add(b).str()
		return a.sub(b)

class MulFunc(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.mul(b)

class EqualExtFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return WrapEtermBoolean(a.is_equal(b))

class EqualFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		if isinstance(a, W_AbstractIntObject) or isinstance(a , W_FloatObject):
			return WrapEtermBoolean(a.is_rough_equal(b))
		else:
			return WrapEtermBoolean(a.is_equal(b))

# just a hook 
class ErrorFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		reason = process.x_reg.get(0)
		return process.fail(cp, pc, fail_class.ERROR, reason)

class ErrorFunc_2(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		reason = process.x_reg.get(0)
		args = process.x_reg.get(1)
		return process.fail(cp, pc, fail_class.ERROR, reason, args)

class DisplayFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		v = process.x_reg.get(0)
		pretty_print.print_value(v)
		return global_atom_table.TRUE_ATOM

class FloatToListFunc_1(BaseBIF):
	def invoke(self, args):
		raise Exception("Not implemented, sorry")

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

class IntegerToListFunc_1(BaseFakeFunc):
	@jit.unroll_safe
	def invoke(self, cp, pc, process):
		i_obj = process.x_reg.get(0)
		assert isinstance(i_obj, W_AbstractIntObject)
		s = i_obj.str()
		return eterm_operators.build_strlist_object([W_IntObject(ord(v)) for v in s])

class IsAtomFunc_1(BaseBIF):
	def invoke(self, args):
		a_obj = args[0]
		return WrapEtermBoolean(isinstance(a_obj, W_AtomObject))

class LengthFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		l_obj = process.x_reg.get(0)
		assert isinstance(l_obj, W_ListObject) or isinstance(l_obj, W_NilObject)
		return W_IntObject(l_obj.length())

class ListAppendFunc_2(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		lst1 = process.x_reg.get(0)
		lst2 = process.x_reg.get(1)
		if isinstance(lst1, W_ListObject):
			return lst1.append(lst2)
		elif isinstance(lst1, W_NilObject):
			assert isinstance(lst2, W_ListObject) or isinstance(lst2, W_NilObject)
			return lst2

class ListFilterFunc_2(BaseBIF):
	@jit.unroll_safe
	def invoke(self,args):
		(lst1, lst2) = args
		contents1 = eterm_operators.get_list_contents(lst1)
		contents2 = eterm_operators.get_list_contents(lst2)
		for e in contents2:
			for i in range(len(contents1)):
				if e.is_equal(contents1[i]):
					del contents1[i]
		return eterm_operators.build_list_object(contents1)

# nif_error has the same semantics as error/1
# despite of the type of return value, which
# is never cared in interpreter.
class NifErrorFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		reason = process.x_reg.get(0)
		return process.fail(cp, pc, fail_class.ERROR, reason)

class RemFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.rem(b)

class SelfFunc_0(BaseBIF0):
	def invoke(self, args):
		return self.caller.pid

class SetElementFunc_3(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		i_obj = process.x_reg.get(0)
		t_obj = process.x_reg.get(1)
		v = process.x_reg.get(2)
		assert isinstance(t_obj, W_TupleObject)
		return t_obj.setelement(eterm_operators.get_int_val(i_obj) - 1, v)

class SpawnFunc_3(BaseFakeFunc):
	def _invoke(self, module_name, func_name, args, cp, process):
		mod_cp = cp.get_module_cp_by_name(module_name)
		func_addr = cp.get_func_addr_by_name(func_name, len(args))
		return process._spawn(mod_cp, func_addr, args, constant.PRIORITY_NORMAL)

	def invoke(self, cp, pc, process):
		from pyrlang.lib.ModuleDict import module_dict, is_bif, get_bif_name
		module_name = eterm_operators.get_atom_val(process.x_reg.get(0))
		func_name = eterm_operators.get_atom_val(process.x_reg.get(1))
		args = eterm_operators.get_list_contents(process.x_reg.get(2))
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
				  "atom_to_list_1" : AtomToListFunc_1,
				  "bsl_2" : BslFunc_2,
				  "bsr_2" : BsrFunc_2,
				  "div_2" : DivFunc_2,
				  "-_2" : SubFunc,
				  "*_2" : MulFunc,
				  "=:=_2" : EqualExtFunc_2,
				  "==_2" : EqualFunc_2,
				  "error_1" : ErrorFunc_1,
				  "error_2" : ErrorFunc_2,
				  "display_1" : DisplayFunc_1,
				  "element_2" : ElementFunc_2,
				  "float_to_list_1" : FloatToListFunc_1,
				  "get_module_info_1" : GetModuleInfoFunc_1,
				  "get_module_info_2" : GetModuleInfoFunc_2,
				  "integer_to_list_1" : IntegerToListFunc_1,
				  "is_atom_1" : IsAtomFunc_1,
				  "length_1" : LengthFunc_1,
				  "nif_error_1" : NifErrorFunc_1,
				  "++_2" : ListAppendFunc_2,
				  "--_2" : ListFilterFunc_2,
				  "rem_2" : RemFunc_2,
				  "self_0" : SelfFunc_0,
				  "setelement_3" : SetElementFunc_3,
				  "spawn_3" : SpawnFunc_3,
				  "tuple_size_1" : TupleSizeFunc_1}

	def initFuncDict(self):
		return self._func_dict	
