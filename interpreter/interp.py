import sys

from pyrlang.rpybeam import opcodes
from pyrlang.lib import ModuleDict
from register import X_Register, Y_Register
from pyrlang.interpreter.datatypes.number import W_IntObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject
from pyrlang.interpreter.datatypes.inner import W_AddrObject
from pyrlang.interpreter.datatypes.atom import W_AtomObject

lib_module = ["erlang"]

class BeamRunTime:
	def __init__(self, cp, atoms, impTs):
		self.cp = cp # code parser
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

	def execute(self):
		while(True):
			instr = self.cp.parseInstr()
			#print "execute instr: %s"%(opcodes.opnames[instr])
			if instr == opcodes.CALL: # 4
				self.call(self.cp.parseInt(), self.cp.parseInt())

			elif instr == opcodes.CALL_LAST: #5
				self.call_last(self.cp.parseInt(), self.cp.parseInt(), self.cp.parseInt())

			elif instr == opcodes.CALL_ONLY: # 6
				self.call_only(self.cp.parseInt(), self.cp.parseInt())

			elif instr == opcodes.ALLOCATE: # 12
				self.allocate(self.cp.parseInt(), self.cp.parseInt())

			elif instr == opcodes.ALLOCATE_ZERO: # 14
				self.allocate_zero(self.cp.parseInt(), self.cp.parseInt())

			elif instr == opcodes.TEST_HEAP: # 16
				self.test_heap(self.cp.parseBase(), self.cp.parseBase())

			elif instr == opcodes.DEALLOCATE: # 18
				self.deallcate(self.cp.parseInt())

			elif instr == opcodes.K_RETURN: # 19
				if self.y_reg.is_empty():
					return self.x_reg.get(0)
				else:
					self.cp.jump_absolute(self.y_reg.pop().addrval)

			elif instr == opcodes.IS_LT: # 39
				self.is_lt(self.cp.parseInt(), self.cp.parseBase(), self.cp.parseBase())

			elif instr == opcodes.IS_EQ_EXACT: # 43
				next_addr = self.cp.parseInt()
				test_reg = self.cp.parseBase()
				# TODO: maybe other data types here
				value = self.cp.parseInt()
				self.is_eq_exact_int(next_addr, test_reg, value)

			elif instr == opcodes.IS_ATOM: # 48
				self.is_atom(self.cp.parseInt(), self.cp.parseBase())

			elif instr == opcodes.IS_NIL: # 52
				self.is_nil(self.cp.parseInt(), self.cp.parseBase())

			elif instr == opcodes.IS_NONEMPTY_LIST: # 56
				self.is_nonempty_list(self.cp.parseInt(), self.cp.parseBase())
				
			elif instr == opcodes.SELECT_VAL: # 59
				reg = self.cp.parseBase()  
				label = self.cp.parseInt()
				sl = self.cp.parse_selectlist()
				self.select_val(reg, label, sl)

			elif instr == opcodes.MOVE: # 64
				self.move(self.cp.parseBase(), self.cp.parseBase())

			elif instr == opcodes.GET_LIST: # 65
				self.get_list(self.cp.parseBase(), self.cp.parseBase(), self.cp.parseBase())

			elif instr == opcodes.PUT_LIST: # 69
				self.put_list(self.cp.parseBase(), self.cp.parseBase(), self.cp.parseBase())

			elif instr == opcodes.GC_BIF2: # 125
				bif_index = self.cp.parseInt()
				fail = self.cp.parseInt()
				alive = self.cp.parseInt()
				rand1 = self.cp.parseBase()
				rand2 = self.cp.parseBase()
				dst_reg = self.cp.parseBase()
				self.gc_bif2(bif_index, fail, alive, rand1, rand2, dst_reg)

			elif instr == opcodes.LINE: # 153
				self.current_line = self.cp.parseInt()

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

	def not_jump(self, label, test_v, class_name):
		v = self.get_basic_value(test_v)
		if not isinstance(v, class_name):
			self.cp.jump_label(label)

########################################################################

	def call(self, arity, label):
		self.y_reg.push(W_AddrObject(self.cp.offset))
		self.cp.jump_label(label)

	def call_last(self, arity, label, n):
		self.deallcate(n)
		self.cp.jump_label(label)

	def call_only(self, arity, label):
		self.cp.jump_label(label)

	def allocate(self, stack_need, live):
		for i in range(0, stack_need):
			self.y_reg.push(W_IntObject(-1))
		
	def allocate_zero(self, stack_need, live):
		for i in range(0, stack_need):
			self.y_reg.push(W_IntObject(0))

	def test_heap(self, alloc, live):
		pass

	def deallcate(self, n):
		for i in range(0, n):
			self.y_reg.pop()
		
	def is_lt(self, label, v1, v2):
		int_v1 = self.get_basic_value(v1)
		int_v2 = self.get_basic_value(v2)
		if not int_v1.lt(int_v2):
			self.cp.jump_label(label)

	def is_eq_exact_int(self, label, test_reg, value):
		if self.fetch_basereg(test_reg).intval != value:
			self.cp.jump_label(label)

	def is_atom(self, label, test_v):
		self.not_jump(label, test_v, W_AtomObject)

	def is_nil(self, label, test_v):
		self.not_jump(label, test_v, W_NilObject)
		#value = self.get_basic_value(test_v)
		#if not isinstance(value, W_NilObject):
			#self.cp.jump_label(label)

	def is_nonempty_list(self, label, test_v):
		value = self.get_basic_value(test_v)
		if isinstance(value, W_NilObject):
			self.cp.jump_label(label)

	def select_val(self, val_reg, label, slist):
		val = self.fetch_basereg(val_reg)
		#print "select_val:"
		#print "atom: %d"%(val.index)
		for i in range(0, len(slist)):
			(v, l) = slist[i]
			if v == val.indexval:
				self.cp.jump_label(l)
				return
		self.cp.jump_label(label)
		
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

	def gc_bif2(self, fail, alive, bif_index, rand1, rand2, dst_reg):
		# TODO: wrap them with try-catch to handle inner exception.
		tmp1 = self.get_basic_value(rand1)
		tmp2 = self.get_basic_value(rand2)
		#print "gc_bif2: opreate: %d, rand1: %d, rand2: %d"%(bif_index, tmp1.intval, tmp2.intval)
		res = self.func_list[bif_index].invoke([tmp1, tmp2])
		self.store_basereg(dst_reg, res)
