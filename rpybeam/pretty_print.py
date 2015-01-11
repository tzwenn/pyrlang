from beam_file import *
from pyrlang.interpreter.datatypes.number import W_IntObject, W_FloatObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject

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
		return "[%s]"%("|".join(s))

def print_value(v):
	print value_str(v)
