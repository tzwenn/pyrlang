from beam_file import *
from pyrlang.interpreter.datatypes.number import W_AbstractIntObject, W_IntObject, W_FloatObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject, W_StrListObject
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from pyrlang.interpreter.datatypes.pid import W_PidObject
from pyrlang.interpreter.atom_table import global_atom_table
from pyrlang.rpybeam.instruction import ListInstruction
from pyrlang.rpybeam import opcodes
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
	print "================ Label Table ==================="
	for i in range(0, len(lt)):
		print "L%d: #%d"%(i+1, lt[i])
	print "============= End of Label Talbe ==============="

def print_hex(s):
	res = ''
	for i in range(0, len(s)):
		res = "%s0x%x|"%(res, ord(s[i]))
	print res

def value_str(v):
	if isinstance(v, W_AbstractIntObject):
		return v.str()
	elif isinstance(v, W_FloatObject):
		return str(v.floatval)
	elif isinstance(v, W_NilObject):
		return "[]"
	elif isinstance(v, W_AtomObject):
		return v.get_str()
	elif isinstance(v, W_TupleObject):
		vals = get_tuple_vals(v)
		return "{%s}"%(", ".join([value_str(t) for t in vals]))
	elif isinstance(v, W_PidObject):
		(node_num, process_num, serial) = get_pid_contents(v)
		return "<%d.%d.%d>"%(node_num, process_num, serial)
	elif isinstance(v, W_StrListObject):
		s = []
		# when printing a list object, Pyrlang will firstly
		# tried to print it as a string, if failed, it will
		# print it as a normal list
		is_str = True
		while not isinstance(v, W_NilObject):
			head = v.head()
			if isinstance(head, W_IntObject):
				try:
					s.append(chr(get_int_val(head)))
					v = v.tail()
				except ValueError:
					is_str = False
					break
			else:
				is_str = False
				break
		if is_str:
			return '"' + ''.join(s) + '"'
		else:
			return list_str(v)
	elif isinstance(v, W_ListObject):
		return list_str(v)

def list_str(v):
	assert isinstance(v, W_ListObject)
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
	return "[%s]"%(",".join(s))

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

def instr_str(cp, instr):
	res = opcodes.opnames[instr.opcode] + " " + rands_to_str(cp, instr.args)
	if isinstance(instr, ListInstruction):
		res += ", [%s]"%", ".join(["(%s, L%d)"%(rand_to_str(cp, v), l) for (v,l) in instr.lst]) 
	return res

def rands_to_str(cp, rands):
	return ", ".join([rand_to_str(cp, arg) for arg in rands])

def rand_to_str(cp, rand):
	(tag, val) = rand
	if tag == opcodes.TAGX_LITERAL:
		return value_str(cp.lit_table[val])
	elif tag == opcodes.TAG_LITERAL:
		return str(val)
	elif tag == opcodes.TAG_INTEGER:
		return "#%d"%(cp.const_table[val].toint())
	elif tag == opcodes.TAG_ATOM:
		return global_atom_table.get_str_at(val)
	elif tag == opcodes.TAG_XREG:
		return "x(%d)"%val
	elif tag == opcodes.TAG_YREG:
		return "y(%d)"%val
	elif tag == opcodes.TAG_LABEL:
		return "L%d"%val
	elif tag == opcodes.TAGX_FLOATREG:
		return "f(%d)"%val
	else:
		return str(val)
