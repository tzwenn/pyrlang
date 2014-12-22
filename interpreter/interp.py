import sys

from pyrlang.rpybeam import opcodes
from pyrlang.rpybeam.beam_code import CodeParser
from pyrlang.lib import ModuleDict
from register import X_Register, Y_Register
from pyrlang.interpreter.datatypes.number import W_IntObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject
from pyrlang.interpreter.datatypes.inner import W_AddrObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from rpython.rlib import jit

lib_module = ["erlang"]

def printable_loc(pc, code, cp):
	index = ord(code[pc])
	return str(pc) + " " + opcodes.opnames[index].upper()

driver = jit.JitDriver(greens = ['pc', 'code', 'cp'],
		reds = ['s_current_line', 's_atoms', 's_func_list', 's_self', 's_x_reg', 's_y_reg'],
		virtualizables = ['s_x_reg'],
		get_printable_location=printable_loc)

class BeamRunTime:
	def __init__(self, atoms, impTs):
		self.atoms = atoms
		self.func_list = []
		self.import_funcs(impTs)
		self.x_reg = X_Register()
		self.y_reg = Y_Register()
		self.current_line = -1

	def init_entry_arguments(self, arg_lst):
		for i in range(0, len(arg_lst)):
			self.x_reg.store(i, arg_lst[i])

	def import_funcs(self, impTs):
		for i in range(0, len(impTs)):
			entry = impTs[i];
			moduleName = self.atoms[entry[0] - 1]
			# TODO: add else branch to hold custom module
			if moduleName in lib_module:
				# we use function_arity to emulate function overload
				moduleEntity = ModuleDict.module_dict[moduleName]()
				function_name = "%s_%d"%(self.atoms[entry[1] - 1], entry[2])
				self.func_list.append(moduleEntity.searchFunc(function_name)())

	@jit.unroll_safe
	def execute(self, cp):
		pc = cp.entry_addr
		code = cp.str
		while(True):
			driver.jit_merge_point(pc = pc,
					code = code,
					cp = cp,
					s_current_line = self.current_line,
					s_atoms = self.atoms,
					s_func_list = self.func_list,
					s_self = self,
					s_x_reg = self.x_reg,
					s_y_reg = self.y_reg)
			#print printable_loc(code, pc)
			instr = ord(code[pc])
			pc = pc + 1
			#print "execute instr: %s"%(opcodes.opnames[instr])
			if instr == opcodes.CALL: # 4
				pc, arity = cp.parseInt(pc)
				pc, label = cp.parseInt(pc)
				pc = self.call(pc, cp, arity, label)

			elif instr == opcodes.CALL_LAST: #5
				pc, arity = cp.parseInt(pc)
				pc, label = cp.parseInt(pc)
				pc, n = cp.parseInt(pc)
				pc = self.call_last(cp, arity, label, n)

			elif instr == opcodes.CALL_ONLY: # 6
				pc, arity = cp.parseInt(pc)
				pc, label = cp.parseInt(pc)
				pc = self.call_only(cp, arity, label)
				driver.can_enter_jit(pc = pc,
						code = code,
						cp = cp,
						s_current_line = self.current_line,
						s_atoms = self.atoms,
						s_func_list = self.func_list,
						s_self = self, 
						s_x_reg = self.x_reg,
						s_y_reg = self.y_reg)

			elif instr == opcodes.ALLOCATE: # 12
				pc, stack_need = cp.parseInt(pc)
				pc, live = cp.parseInt(pc)
				self.allocate(stack_need, live)

			elif instr == opcodes.ALLOCATE_ZERO: # 14
				pc, stack_need = cp.parseInt(pc)
				pc, live = cp.parseInt(pc)
				self.allocate_zero(stack_need, live)

			elif instr == opcodes.TEST_HEAP: # 16
				pc, term1 = cp.parseBase(pc)
				pc, term2 = cp.parseBase(pc)
				self.test_heap(term1, term2)

			elif instr == opcodes.DEALLOCATE: # 18
				pc, n = cp.parseInt(pc)
				self.deallcate(n)

			elif instr == opcodes.K_RETURN: # 19
				if self.y_reg.is_empty():
					return self.x_reg.get(0)
				else:
					pc = self.y_reg.pop().addrval

			elif instr == opcodes.IS_LT: # 39
				pc, label = cp.parseInt(pc)
				pc, term1 = cp.parseBase(pc)
				pc, term2 = cp.parseBase(pc)
				pc = self.is_lt(pc, cp, label, term1, term2)

			elif instr == opcodes.IS_EQ_EXACT: # 43
				pc, next_addr = cp.parseInt(pc)
				pc, test_reg = cp.parseBase(pc)
				# TODO: maybe other data types here
				pc, value = cp.parseInt(pc)
				pc = self.is_eq_exact_int(pc, cp, next_addr, test_reg, value)

			elif instr == opcodes.IS_ATOM: # 48
				pc, label = cp.parseInt(pc)
				pc, test_v = cp.parseBase(pc)
				pc = self.is_atom(pc, cp, label, test_v)

			elif instr == opcodes.IS_NIL: # 52
				pc, label = cp.parseInt(pc)
				pc, test_v = cp.parseBase(pc)
				pc = self.is_nil(pc, cp, label, test_v)

			elif instr == opcodes.IS_NONEMPTY_LIST: # 56
				pc, label = cp.parseInt(pc)
				pc, test_v = cp.parseBase(pc)
				pc = self.is_nonempty_list(pc, cp, label, test_v)
				
			elif instr == opcodes.SELECT_VAL: # 59
				pc, reg = cp.parseBase(pc)  
				pc, label = cp.parseInt(pc)
				pc, sl = cp.parse_selectlist(pc)
				pc = self.select_val(cp, reg, label, sl)

			elif instr == opcodes.MOVE: # 64
				pc, source = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				self.move(source, dst_reg)

			elif instr == opcodes.GET_LIST: # 65
				pc, src_reg = cp.parseBase(pc)
				pc, head_reg = cp.parseBase(pc)
				pc, tail_reg = cp.parseBase(pc)
				self.get_list(src_reg, head_reg, tail_reg)

			elif instr == opcodes.PUT_LIST: # 69
				pc, head_reg = cp.parseBase(pc)
				pc, tail_reg = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				self.put_list(head_reg, tail_reg, dst_reg)

			elif instr == opcodes.GC_BIF2: # 125
				pc, fail = cp.parseInt(pc)
				pc, alive = cp.parseInt(pc)
				pc, bif_index = cp.parseInt(pc)
				pc, rand1 = cp.parseBase(pc)
				pc, rand2 = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				pc = self.gc_bif2(pc, fail, alive, bif_index, rand1, rand2, dst_reg)

			elif instr == opcodes.LINE: # 153
				pc, self.current_line = cp.parseInt(pc)

			else:
				raise Exception("Unimplemented opcode: %d"%(instr))

	def get_basic_value(self, pair):
		(tag, value) = pair
		if tag == opcodes.TAG_XREG or tag == opcodes.TAG_YREG:
			return self.fetch_basereg(pair)
		elif tag == opcodes.TAG_INTEGER:
			return W_IntObject(value)
		elif tag == opcodes.TAG_ATOM:
			if value == 0:
				return W_NilObject()
			else:
				return W_AtomObject(value)
		else:
			# TODO: take more care for else branch
			return W_IntObject(value)

	def fetch_basereg(self, regval):
		(tag, val) = regval
		if tag == opcodes.TAG_XREG:
			return self.x_reg.get(val)
		else:
			return self.y_reg.get(val)

	def store_basereg(self, dst_reg, res):
		(tag, val) = dst_reg
		#print "store value %d to reg %d(%d)"%(res.intval, tag, val)
		if tag == opcodes.TAG_XREG:
			self.x_reg.store(val, res)
		else:
			self.y_reg.store(val, res)

	def not_jump(self, pc, cp, label, test_v, class_name):
		v = self.get_basic_value(test_v)
		if isinstance(v, class_name):
			return pc
		else:
			return cp.label_to_addr(label)

########################################################################

	def call(self, pc, cp, arity, label):
		self.y_reg.push(W_AddrObject(pc))
		return cp.label_to_addr(label)

	def call_last(self, cp, arity, label, n):
		self.deallcate(n)
		return cp.label_to_addr(label)

	def call_only(self, cp, arity, label):
		return cp.label_to_addr(label)

	@jit.unroll_safe
	def allocate(self, stack_need, live):
		for i in range(0, stack_need):
			self.y_reg.push(None)
		
	@jit.unroll_safe
	def allocate_zero(self, stack_need, live):
		for i in range(0, stack_need):
			self.y_reg.push(W_IntObject(0))

	def test_heap(self, alloc, live):
		pass

	@jit.unroll_safe
	def deallcate(self, n):
		for i in range(0, n):
			self.y_reg.pop()
		
	def is_lt(self, pc, cp, label, v1, v2):
		int_v1 = self.get_basic_value(v1)
		int_v2 = self.get_basic_value(v2)
		if int_v1.lt(int_v2):
			return pc
		else:
			return cp.label_to_addr(label)

	def is_eq_exact_int(self, pc, cp, label, test_reg, value):
		if self.fetch_basereg(test_reg).intval != value:
			return cp.label_to_addr(label)
		else:
			return pc

	def is_atom(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_AtomObject)

	def is_nil(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_NilObject)
		#value = self.get_basic_value(test_v)
		#if not isinstance(value, W_NilObject):
			#cp.jump_label(label)

	def is_nonempty_list(self, pc, cp, label, test_v):
		value = self.get_basic_value(test_v)
		if isinstance(value, W_NilObject):
			return cp.label_to_addr(label)
		else:
			return pc

	@jit.unroll_safe
	def select_val(self, cp, val_reg, label, slist):
		val = self.fetch_basereg(val_reg)
		#print "select_val:"
		#print "atom: %d"%(val.index)
		for i in range(0, len(slist)):
			(v, l) = slist[i]
			if v == val.indexval:
				return cp.label_to_addr(l)
		return cp.label_to_addr(label)
		
	def move(self, source, dst_reg):
		self.store_basereg(dst_reg, self.get_basic_value(source))

	def get_list(self, src_reg, head_reg, tail_reg):
		lst = self.fetch_basereg(src_reg)
		self.store_basereg(head_reg, lst.head())
		self.store_basereg(tail_reg, lst.tail())

	def put_list(self, head_reg, tail_reg, dst_reg):
		head = self.fetch_basereg(head_reg)
		tail = self.fetch_basereg(tail_reg)
		res = W_ListObject(head, tail)
		self.store_basereg(dst_reg, res)

	def gc_bif2(self, pc, fail, alive, bif_index, rand1, rand2, dst_reg):
		# TODO: wrap them with try-catch to handle inner exception.
		tmp1 = self.get_basic_value(rand1)
		tmp2 = self.get_basic_value(rand2)
		#print "gc_bif2: opreate: %d, rand1: %d, rand2: %d"%(bif_index, tmp1.intval, tmp2.intval)
		res = self.func_list[bif_index].invoke([tmp1, tmp2])
		self.store_basereg(dst_reg, res)
		return pc
