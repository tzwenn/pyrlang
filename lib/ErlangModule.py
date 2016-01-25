from pyrlang.lib.base import BaseModule, BaseBIF, BaseBIF0, BaseFakeFunc, WrapEtermBoolean
from pyrlang.interpreter import fail_class
from pyrlang.interpreter.datatypes.number import *
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject, W_BoolAtomObject
from pyrlang.interpreter.datatypes.inner import W_AddrObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject, W_StrListObject
from pyrlang.interpreter.datatypes.closure import W_ClosureObject
from pyrlang.interpreter.atom_table import global_atom_table
from pyrlang.interpreter import constant
from pyrlang.rpybeam import pretty_print
from pyrlang.utils import eterm_operators
from pyrlang.interpreter import constant
from rpython.rlib import jit
import time

class AbsFunc(BaseBIF):
	def invoke(self, args):
		a = args[0]
		assert isinstance(a, W_AbstractNumberObject)
		return a.abs()

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

class AndFunc_2(BaseBIF):
	def invoke(self, args):
		(left, right) = args
		assert isinstance(left, W_BoolAtomObject)
		assert isinstance(right, W_BoolAtomObject)
		return WrapEtermBoolean(left == global_atom_table.TRUE_ATOM and right == global_atom_table.TRUE_ATOM)

#class ApplyFunc_2(BaseFakeFunc):
	#def invoke(self, cp, pc, process):
		#closure = process.x_reg.get(0)
		#args = eterm_operators.get_list_contents(process.x_reg.get(1))
		#(new_cp, addr, real_arity, fvs) = eterm_operators.get_closure_fields(closure)
		#if len(args) == real_arity:
			#for i in range(real_arity):
				#self.x_reg.store(i, args[i])
			#for i in range(len(fvs)):
				#self.x_reg.store(real_arity + i, fvs[i])
			#return W_AddrObject(new_cp, addr)
		#else:
			#raise Exception("arity mismatch")

class BandFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		return a.and_(b)

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

class DivFloatFunc_2(BaseBIF):
	def invoke(self, args):
		(a,b) = args
		float_obj = a if isinstance(a, W_FloatObject) else W_FloatObject(a.tofloat())
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

class NegFunc(BaseBIF):
	def invoke(self, args):
		a = args[0]
		assert isinstance(a, W_AbstractNumberObject)
		return a.neg()

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
		#print index.intval, tuple_val.vals
		assert isinstance(tuple_val, W_TupleObject)
		return tuple_val.element_from_int_obj(index)

class GetModuleInfoFunc_1(BaseBIF):
	def invoke(self,arg):
		return W_FloatObject(3.1415926)

class GetModuleInfoFunc_2(BaseBIF):
	def invoke(self,arg):
		return W_FloatObject(2.3333333)

class HdFunc_1(BaseBIF):
	def invoke(self, arg):
		list_val = arg[0]
		assert isinstance(list_val, W_ListObject)
		return list_val.head()

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

class LengthFunc_1(BaseBIF):
	def invoke(self, args):
		l_obj = args[0]
		assert isinstance(l_obj, W_ListObject) or isinstance(l_obj, W_NilObject)
		return W_IntObject(l_obj.length())

class ListToIntegerFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		lst_obj = process.x_reg.get(0)
		return W_IntObject(int(eterm_operators.get_str_list_contents(lst_obj)))

class ListAppendFunc_2(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		lst_obj1 = process.x_reg.get(0)
		lst_obj2 = process.x_reg.get(1)
		lst1 = eterm_operators.get_list_contents(lst_obj1)
		#lst2 = eterm_operators.get_list_contents(lst_obj2)
		#if isinstance(lst_obj1, W_StrListObject):
			#return eterm_operators.build_strlist_object(lst1 + lst2)
		#else:
			#assert isinstance(lst_obj1, W_ListObject) or isinstance(lst_obj1, W_NilObject)
			#return eterm_operators.build_list_object(lst1 + lst2)
		right = lst_obj2
		if isinstance(lst_obj1, W_StrListObject):
			right = self._to_str_list(lst1, right)
		elif isinstance(lst_obj1, W_ListObject):
			right = self._to_list(lst1, right)
		else:
			assert isinstance(lst_obj1, W_NilObject)
			right = lst_obj2
		return right

	#@jit.look_inside_iff(
			#lambda self, l, curr: jit.loop_unrolling_heuristic(l, len(l), constant.UNROLLING_CUTOFF))
	@jit.unroll_safe
	def _to_str_list(self, l, curr):
		for i in range(len(l)-1,-1,-1):
			curr = W_StrListObject(l[i], curr)
		return curr

	@jit.unroll_safe
	#@jit.look_inside_iff(
			#lambda self, l, curr: jit.loop_unrolling_heuristic(l, len(l), constant.UNROLLING_CUTOFF))
	def _to_list(self, l, curr):
		for i in range(len(l)-1,-1,-1):
			curr = W_ListObject(l[i], curr)
		return curr

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

class NotFunc_1(BaseBIF):
	def invoke(self, args):
		arg = args[0]
		assert isinstance(arg, W_BoolAtomObject)
		if arg == global_atom_table.TRUE_ATOM:
			return global_atom_table.FALSE_ATOM
		else:
			return global_atom_table.TRUE_ATOM

class NowFunc_0(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		t = time.time()
		mega_sec = int(t / 1000000)
		tmp = mega_sec * 1000000
		sec = int(t - tmp)
		m_sec = int((t - tmp - sec) * 1000000)
		return W_TupleObject([W_IntObject(mega_sec), W_IntObject(sec), W_IntObject(m_sec)])

class OrFunc_2(BaseBIF):
	def invoke(self, args):
		(left, right) = args
		assert isinstance(left, W_BoolAtomObject)
		assert isinstance(right, W_BoolAtomObject)
		return WrapEtermBoolean(left == global_atom_table.TRUE_ATOM or right == global_atom_table.TRUE_ATOM)

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

class SpawnFunc_1(BaseFakeFunc):
	def invoke(self, cp, pc, process):
		cls = process.x_reg.get(0)
		assert isinstance(cls, W_ClosureObject)
		return process._spawn(cls.cp, cls.pc, cls.fv_lst, constant.PRIORITY_NORMAL)

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

class TlFunc_1(BaseBIF):
	def invoke(self, args):
		list_val = args[0]
		assert isinstance(list_val, W_ListObject)
		return list_val.tail()

class TruncFunc_1(BaseBIF):
	def invoke(self, args):
		float_val = args[0]
		return W_IntObject(int(eterm_operators.get_float_val(float_val)))

class TupleSizeFunc_1(BaseBIF):
	def invoke(self, args):
		tuple_val = args[0]
		assert isinstance(tuple_val, W_TupleObject)
		return tuple_val.size_to_int_obj()

class ModuleEntity(BaseModule):
	_func_dict = { 
				  "abs_1" : AbsFunc,
				  "+_2" : AddFunc,
				  "atom_to_list_1" : AtomToListFunc_1,
				  "and_2" : AndFunc_2,
				  #"apply_2" : ApplyFunc_2,
				  #"apply_3" : ApplyFunc_3,
				  "band_2" : BandFunc_2,
				  "bsl_2" : BslFunc_2,
				  "bsr_2" : BsrFunc_2,
				  "div_2" : DivFunc_2,
				  "/_2" : DivFloatFunc_2,
				  "-_2" : SubFunc,
				  "-_1" : NegFunc,
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
				  "hd_1" : HdFunc_1,
				  "is_atom_1" : IsAtomFunc_1,
				  "length_1" : LengthFunc_1,
				  "list_to_integer_1" : ListToIntegerFunc_1,
				  "nif_error_1" : NifErrorFunc_1,
				  "not_1" : NotFunc_1,
				  "now_0" : NowFunc_0,
				  "++_2" : ListAppendFunc_2,
				  "--_2" : ListFilterFunc_2,
				  "or_2" : OrFunc_2,
				  #"put_2" : PutFunc_2,
				  "rem_2" : RemFunc_2,
				  "self_0" : SelfFunc_0,
				  "setelement_3" : SetElementFunc_3,
				  "spawn_1" : SpawnFunc_1,
				  "spawn_3" : SpawnFunc_3,
				  "tl_1" : TlFunc_1,
				  "trunc_1" : TruncFunc_1,
				  "tuple_size_1" : TupleSizeFunc_1}

	def initFuncDict(self):
		return self._func_dict	
