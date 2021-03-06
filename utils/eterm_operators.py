from pyrlang.interpreter.datatypes.number import W_AbstractIntObject, W_IntObject, W_BigIntObject, W_FloatObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject, W_StrListObject
from pyrlang.interpreter.datatypes.tuple import W_AbstractTupleObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from pyrlang.interpreter.datatypes.pid import W_PidObject
from pyrlang.interpreter.datatypes.inner import W_AddrObject
from pyrlang.interpreter.datatypes.closure import W_ClosureObject
from rpython.rlib import jit

def get_tuple_vals(v):
	assert isinstance(v, W_AbstractTupleObject)
	return v.elements()

@jit.unroll_safe
def get_list_contents(v):
	if isinstance(v, W_NilObject):
		return []
	else:
		assert isinstance(v, W_ListObject)
		res = []
		while True:
			res.append(v.head())
			if isinstance(v.tail(), W_NilObject):
				break
			v = v.tail()
		return res

def get_str_list_contents(v):
	res = get_list_contents(v)
	s = ""
	for e in res:
		s += chr(get_int_val(e))
	return s

def get_atom_val(v):
	assert isinstance(v, W_AtomObject)
	return v.get_str()

def get_int_val(v):
	assert isinstance(v, W_AbstractIntObject)
	return v.toint()

def get_float_val(v):
	assert isinstance(v, W_FloatObject)
	return v.tofloat()

def get_pid_contents(v):
	assert isinstance(v, W_PidObject)
	return (v.node_num, v.process_num, v.serial)

def get_addr_val(v):
	assert isinstance(v, W_AddrObject)
	return v.pc

@jit.unroll_safe
def build_list_object(object_lst):
	right = W_NilObject()
	length = len(object_lst)
	for i in range(0, length):
		right = W_ListObject(object_lst[length - i - 1], right)
	return right

@jit.unroll_safe
def build_strlist_object(object_lst):
	right = W_NilObject()
	length = len(object_lst)
	for i in range(0, length):
		right = W_StrListObject(object_lst[length - i - 1], right)
	return right

def build_strlist_object_from_string(s):
	return build_strlist_object([W_IntObject(ord(c)) for c in s])

def get_closure_fields(v):
	assert isinstance(v, W_ClosureObject)
	return (v.cp, v.pc, v.arity, v.free_variables())
