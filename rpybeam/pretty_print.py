from beam_file import *
from pyrlang.interpreter.datatypes.number import W_IntObject, W_FloatObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from pyrlang.utils.eterm_operators import * 

def print_ImpT(impt, atomTable):
	impt_len = len(impt.entries)
	print "# Imports:"
	for i in range(0, impt_len):
		e = impt.entries[i]
		print "#      %s:%s/%d"%(atomTable[e.module-1], atomTable[e.function - 1], e.arity)
	print "#"

def print_ExpT(impt, atomTable):
	expt_len = len(impt.entries)
	print "# Exports:"
	for i in range(0, expt_len):
		e = impt.entries[i]
		print "#      %s/%d"%(atomTable[e.function-1], e.arity)
	print "#"

def print_Root(root):
	atoms = root.atomChunk.asList()
	print "# Module: %s"%(atoms[0])
	print "#"
	print_ExpT(root.expTChunk, atoms)
	print_ImpT(root.impTChunk, atoms)

def print_labelTable(lt):
	for i in range(0, len(lt)):
		print "L%d: #%d"%(i+1, lt[i])

def print_hex(s):
	res = ''
	for i in range(0, len(s)):
		res = "%s0x%x|"%(res, ord(s[i]))
	print res

def value_str(v):
	if isinstance(v, W_IntObject):
		return "%d"%(v.intval)
	elif isinstance(v, W_FloatObject):
		return "%f"%(v.floatval)
	elif isinstance(v, W_NilObject):
		return "Nil"
	elif isinstance(v, W_AtomObject):
		return v.strval
	elif isinstance(v, W_TupleObject):
		vals = [value_str(e) for e in v.vals]
		return "{%s}"%(", ".join(vals))
	elif isinstance(v, W_ListObject):
		s = []
		i = 0
		too_long = False
		while not isinstance(v.tail(), W_NilObject):
			s.append(value_str(v.head()))
			v = v.tail()
			i += 1
			if(i >= 30):
				too_long = True
				break
		if too_long:
			s.append("...")
		else:
			s.append(value_str(v.head()))
		return "[%s]"%(", ".join(s))

def print_value(v):
	print value_str(v)

def _stack_trace_tuple(v):
	l = get_tuple_vals(v)
	(module_atom, func_atom, arity, file_msg) = l
	(file_path_tuple, line_tuple) = get_list_contents(file_msg)
	path_atom = get_tuple_vals(file_path_tuple)[1]
	line = get_tuple_vals(line_tuple)[1]
	return (get_atom_val(module_atom),
			get_atom_val(func_atom),
			get_int_val(arity),
			get_atom_val(path_atom),
			get_int_val(line))

def error_message(v):
	(reason, stack_trace) = get_tuple_vals(v)
	st_list = get_list_contents(stack_trace)
	t1 = _stack_trace_tuple(st_list[0])

	s = "** exception error: %s\n"%get_atom_val(reason)
	s += "     in function  %s:%s/%d (%s, line %d)"%t1 
	if len(st_list) > 1:
		for i in range(1, len(st_list)):
			s += "\n     in call from %s:%s/%d (%s, line %d)"%_stack_trace_tuple(st_list[i])
	return s
