import sys
import ast
import pybeam
from pybeam import opcodes
from pybeam.code_construct import beam_code

sys.path.append("../")
# used for my MacBook
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')
# used for lab's MacPro
sys.path.append('/Users/huangruochen/src/python/pypy')
from pyrlang.lib.ModuleDict import module_dict

def get_named_type_from_body(name, type, obj):
	for node in obj.body:
		if isinstance(node, type) and node.name == name:
			return node

def get_first_type_stmt_from_body(type, obj):
	for node in obj.body:
		if isinstance(node, type):
			return node

if __name__ == '__main__':
	implemented_instrs = []
	s = open("interpreter/interp.py", "rb").read()
	c = ast.parse(s)
	process_def = get_named_type_from_body("Process", ast.ClassDef, c)
	execute_def = get_named_type_from_body("execute", ast.FunctionDef, process_def)
	dispatch_loop = get_first_type_stmt_from_body(ast.While, execute_def)
	dispatch_if = get_first_type_stmt_from_body(ast.If, dispatch_loop)
	while(True):
		cmp = dispatch_if.test
		implemented_instrs.append(cmp.comparators[0].attr)
		dispatch_if = dispatch_if.orelse[0]
		if not isinstance(dispatch_if, ast.If):
			break

	implemented_instrs_hex = [getattr(opcodes, name) for name in implemented_instrs]

	fname = sys.argv[1]
	b = pybeam.BeamFile(fname)
	
	(_,_,_,_,code) = b.code

	unimplemented_instrs = []
	for (op,operands) in beam_code.parse(code):
		if not op in implemented_instrs_hex:
			unimplemented_instrs.append(op)

	unimplemented_instrs = list(set(unimplemented_instrs))
			
	print "********************************************"
	print "unimplemented instruction:"
	for instr in unimplemented_instrs:
		print opcodes.opnames[instr].upper()

	print "unimplementation rate:"+"{0:.0f}%".format(float(len(unimplemented_instrs)) / len(implemented_instrs) * 100)

	unimplemented_modules = {} 
	unimplemented_module_functions = []
	for (module_name, function_name, arity) in b.imports:
		if module_name in module_dict:
			if not function_name + "_" + str(arity) in module_dict[module_name]._func_dict:
				unimplemented_module_functions.append("%s:%s/%d"%(module_name, function_name, arity))
		else:
			if module_name in unimplemented_modules:
				unimplemented_modules[module_name].append("%s:%s/%d"%(module_name, function_name, arity))
			else:
				unimplemented_modules[module_name] = ["%s:%s/%d"%(module_name, function_name, arity)]
	print "********************************************"
	print "unimplemented module(s):"
	for (mod, funcs) in unimplemented_modules.iteritems():
		print mod + " in:"
		for func in funcs:
			print "   " + func
		
	print "TOTAL: %d"%(len(unimplemented_modules))
	print "********************************************"
	print "unimplemented BIF(s):"
	for bif in unimplemented_module_functions:
		print bif
	print "TOTAL: %d"%(len(unimplemented_module_functions))
