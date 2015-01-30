from pyrlang.interpreter.datatypes.number import W_IntObject, W_FloatObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject

def get_tuple_vals(v):
	assert isinstance(v, W_TupleObject)
	return v.vals

def get_list_contents(v):
	assert isinstance(v, W_ListObject)
	res = []
	while True:
		res.append(v.head())
		if isinstance(v.tail(), W_NilObject):
			break
		v = v.tail()
	return res

def get_atom_val(v):
	assert isinstance(v, W_AtomObject)
	return v.strval

def get_int_val(v):
	assert isinstance(v, W_IntObject)
	return v.intval
