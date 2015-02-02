import sys

from pyrlang.rpybeam import opcodes
from pyrlang.rpybeam import pretty_print
from pyrlang.interpreter import fail_class
from pyrlang.rpybeam.beam_file import *
from pyrlang.interpreter.register import X_Register, Y_Register
from pyrlang.interpreter.cont_stack import ContinuationStack
from pyrlang.interpreter.datatypes.root import W_Root
from pyrlang.interpreter.datatypes.pid import W_PidObject
from pyrlang.interpreter.datatypes.number import W_IntObject, W_FloatObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.inner import W_AddrObject, W_CodeParserWrapperObject 
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from pyrlang.interpreter import constant
from pyrlang.utils.deque import MessageDeque
from pyrlang.lib.base import BaseBIF, BaseFakeFunc
from rpython.rlib import jit

def printable_loc(pc, cp):
	index = ord(cp.code[pc])
	return str(pc) + " " + opcodes.opnames[index].upper()

driver = jit.JitDriver(greens = ['pc', 'cp'],
		reds = ['reduction', 'single', 's_self', 's_x_reg', 's_y_reg'],
		virtualizables = ['s_x_reg'],
		get_printable_location=printable_loc)

class Process:
	def __init__(self, pid, scheduler, priority = constant.PRIORITY_NORMAL):
		self.x_reg = X_Register()
		self.y_reg = Y_Register()

		self.tuple_dst = constant.INVALID_REG
		self.tuple_arity = 0
		self.tuple_data = []

		self.pid = pid
		self.scheduler = scheduler
		self.priority = priority
		self.mail_box = MessageDeque()
		self.cont_stack = ContinuationStack()

	def init_entry_arguments(self, arg_lst):
		for i in range(0, len(arg_lst)):
			self.x_reg.store(i, arg_lst[i])

	@jit.unroll_safe
	def execute(self, cp, func_addr, single = False):
		pc = func_addr
		reduction = 2000

		while(True):
			driver.jit_merge_point(pc = pc,
					cp = cp,
					reduction = reduction,
					single = single, 
					s_self = self,
					s_x_reg = self.x_reg,
					s_y_reg = self.y_reg)
			#print pretty_print.value_str(self.pid) + ": " + printable_loc(pc, cp)
			instr = ord(cp.code[pc])
			pc = pc + 1
			#print "execute instr: %s"%(opcodes.opnames[instr])
			if instr == opcodes.LABEL: # 1
				pc, _ = cp.parseInt(pc)

			elif instr == opcodes.CALL: # 4
				pc, arity = cp.parseInt(pc)
				pc, label = cp.parseInt(pc)
				pc = self.call(pc, cp, arity, label)

			elif instr == opcodes.CALL_LAST: #5
				pc, arity = cp.parseInt(pc)
				pc, label = cp.parseInt(pc)
				pc, n = cp.parseInt(pc)
				pc = self.call_last(cp, arity, label, n)
				reduction -= 1
				if not single and reduction <= 0:
					break

			elif instr == opcodes.CALL_ONLY: # 6
				pc, arity = cp.parseInt(pc)
				pc, label = cp.parseInt(pc)
				pc = self.call_only(cp, arity, label)
				reduction -= 1
				if not single and reduction <= 0:
					break
				else:
					driver.can_enter_jit(pc = pc,
							cp = cp,
							reduction = reduction,
							single = single,
							s_self = self, 
							s_x_reg = self.x_reg,
							s_y_reg = self.y_reg)

			elif instr == opcodes.CALL_EXT: # 7
				pc, real_arity = cp.parseInt(pc)
				pc, (tag, header_index) = cp.parseBase(pc)
				if (tag == opcodes.TAG_LITERAL):
					entry = cp.import_header[header_index]
					cp, pc = self.call_ext(cp, pc, entry, real_arity)
				else:
					assert tag == opcodes.TAG_LABEL
					pc = self._call_ext_bif(pc, cp, header_index)
				reduction -= 1
				if not single and reduction <= 0:
					break

			elif instr == opcodes.CALL_EXT_LAST: # 8
				pc, real_arity = cp.parseInt(pc)
				pc, (tag, header_index) = cp.parseBase(pc)
				pc, dealloc = cp.parseInt(pc)
				if (tag == opcodes.TAG_LITERAL):
					entry = cp.import_header[header_index]
					cp, pc = self.call_ext_last(cp, pc, entry, real_arity, dealloc)
				else:
					assert tag == opcodes.TAG_LABEL
					pc = self._call_ext_bif(pc, cp, header_index)
					self.deallcate(dealloc)
					# calling a bif means the dispatch loop 
					# need a extra k_return semantics 
					if self.y_reg.is_empty():
						return (constant.STATE_TERMINATE, pc)
					else:
						(cp, pc) = self.k_return(cp)
				reduction -= 1
				if not single and reduction <= 0:
					break

			elif instr == opcodes.BIF0: # 9
				pc, bif_index = cp.parseInt(pc)
				pc, dst_reg = cp.parseBase(pc)
				self.bif0(cp, pc, bif_index, dst_reg)

			elif instr == opcodes.BIF1: # 10
				pc, fail = cp.parseInt(pc)
				pc, bif_index = cp.parseInt(pc)
				pc, rand1 = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				pc = self.bif1(cp, pc, fail, 
						cp.import_header[bif_index][1], rand1, dst_reg)
				reduction -= 1
				if not single and reduction <= 0:
					break

			elif instr == opcodes.BIF2: # 11
				pc, fail = cp.parseInt(pc)
				pc, bif_index = cp.parseInt(pc)
				pc, rand1 = cp.parseBase(pc)
				pc, rand2 = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				pc = self.bif2(cp, pc, fail, 
						cp.import_header[bif_index][1], rand1, rand2, dst_reg)
				reduction -= 1
				if not single and reduction <= 0:
					break

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

			elif instr == opcodes.INIT: # 17
				pc, dst_reg = cp.parseBase(pc)
				self.init(dst_reg)

			elif instr == opcodes.DEALLOCATE: # 18
				pc, n = cp.parseInt(pc)
				self.deallcate(n)

			elif instr == opcodes.K_RETURN: # 19
				if self.y_reg.is_empty():
					return (constant.STATE_TERMINATE, pc)
				else:
					(cp, pc) = self.k_return(cp)

			elif instr == opcodes.SEND: # 20
				self.send()

			elif instr == opcodes.REMOVE_MESSAGE: # 21
				self.remove_message()

			elif instr == opcodes.LOOP_REC: # 23
				pc, label = cp.parseInt(pc)
				pc, dst_reg = cp.parseBase(pc)
				pc = self.loop_rec(pc, cp, label, dst_reg)

			elif instr == opcodes.LOOP_REC_END: # 24
				pc, label = cp.parseInt(pc)
				pc = self.loop_rec_end(cp, label)

			elif instr == opcodes.WAIT: # 25
				pc, label = cp.parseInt(pc)
				pc = self.wait(cp, label)
				return (constant.STATE_HANG_UP, pc)

			elif instr == opcodes.IS_LT: # 39
				pc, label = cp.parseInt(pc)
				pc, term1 = cp.parseBase(pc)
				pc, term2 = cp.parseBase(pc)
				pc = self.is_lt(pc, cp, label, term1, term2)

			elif instr == opcodes.IS_EQ_EXACT: # 43
				pc, next_addr = cp.parseInt(pc)
				pc, test_reg = cp.parseBase(pc)
				# TODO: maybe other data types here
				pc, dst_reg  = cp.parseBase(pc)
				pc = self.is_eq_exact(pc, cp, next_addr, test_reg, dst_reg)

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

			elif instr == opcodes.IS_TUPLE: # 57
				pc, label = cp.parseInt(pc)
				pc, test_v = cp.parseBase(pc)
				pc = self.is_tuple(pc, cp, label, test_v)

			elif instr == opcodes.TEST_ARITY: # 58
				pc, label = cp.parseInt(pc)
				pc, src_reg = cp.parseBase(pc)
				pc, size = cp.parseInt(pc)
				pc = self.test_arity(pc, cp, label, src_reg, size)
				
			elif instr == opcodes.SELECT_VAL: # 59
				pc, reg = cp.parseBase(pc)  
				pc, label = cp.parseInt(pc)
				pc, sl = cp.parse_selectlist(pc)
				pc = self.select_val(cp, reg, label, sl)

			elif instr == opcodes.JUMP: # 61
				pc, label = cp.parseInt(pc)
				pc = self.jump(cp, label)

			elif instr == opcodes.K_CATCH: # 62
				pc, reg = cp.parseBase(pc)
				pc, label = cp.parseInt(pc)
				self.k_catch(cp, reg, label)

			elif instr == opcodes.CATCH_END: # 63
				pc, reg = cp.parseBase(pc)
				self.catch_end(pc, cp, reg)

			elif instr == opcodes.MOVE: # 64
				pc, source = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				self.move(cp, source, dst_reg)

			elif instr == opcodes.GET_LIST: # 65
				pc, src_reg = cp.parseBase(pc)
				pc, head_reg = cp.parseBase(pc)
				pc, tail_reg = cp.parseBase(pc)
				self.get_list(src_reg, head_reg, tail_reg)

			elif instr == opcodes.GET_TUPLE_ELEMENT: # 66
				pc, src_reg = cp.parseBase(pc)
				pc, index = cp.parseInt(pc)
				pc, dst_reg = cp.parseBase(pc)
				self.get_tuple_element(cp, src_reg, index, dst_reg)

			elif instr == opcodes.PUT_LIST: # 69
				pc, head_reg = cp.parseBase(pc)
				pc, tail_reg = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				self.put_list(head_reg, tail_reg, dst_reg)

			elif instr == opcodes.PUT_TUPLE: # 70
				pc, arity = cp.parseInt(pc)
				pc, dst_reg = cp.parseBase(pc)
				self.put_tuple(arity, dst_reg)

			elif instr == opcodes.PUT: # 71
				pc, src = cp.parseBase(pc)
				self.put(cp, src)

			elif instr == opcodes.CALL_FUN: # 75
				pc, arity = cp.parseInt(pc)
				pc = self.call_fun(pc, arity)

			elif instr == opcodes.CALL_EXT_ONLY: # 78
				pc, real_arity = cp.parseInt(pc)
				pc, (tag, header_index) = cp.parseBase(pc)
				if tag == opcodes.TAG_LITERAL:
					entry = cp.import_header[header_index]
					cp, pc = self.call_ext_only(cp, entry, real_arity)
				else:
					assert tag == opcodes.TAG_LABEL
					pc = self._call_ext_bif(pc, cp, header_index)

			elif instr == opcodes.MAKE_FUN2: # 103
				pc, index = cp.parseInt(pc)
				self.make_fun2(cp, index)

			elif instr == opcodes.GC_BIF2: # 125
				pc, fail = cp.parseInt(pc)
				pc, alive = cp.parseInt(pc)
				pc, bif_index = cp.parseInt(pc)
				pc, rand1 = cp.parseBase(pc)
				pc, rand2 = cp.parseBase(pc)
				pc, dst_reg = cp.parseBase(pc)
				pc = self.gc_bif2(cp, pc, fail, alive, 
						cp.import_header[bif_index][1], rand1, rand2, dst_reg)
				reduction -= 1
				if not single and reduction <= 0:
					break

			elif instr == opcodes.LINE: # 153
				pc, cp.current_line = cp.parseInt(pc)

			else:
				raise Exception("Unimplemented opcode: %d"%(instr))
		return (constant.STATE_SWITH, pc)

	def _send_by_pid(self, pid, msg):
		self.scheduler.send_by_pid(pid, msg)

	def go_to_next_message(self):
		self.mail_box.next()

	def reset_message_to_head(self):
		self.mail_box.reset_to_head()

	def current_message(self):
		return self.mail_box.get_current()

	def append_message(self, msg):
		return self.mail_box.append(msg)

	def remove_current_message(self):
		self.mail_box.remove_current()

	def get_basic_value(self, cp, pair):
		(tag, value) = pair
		if tag == opcodes.TAG_XREG or tag == opcodes.TAG_YREG:
			return self.fetch_basereg(pair)
		elif tag == opcodes.TAG_INTEGER:
			return W_IntObject(value)
		elif tag == opcodes.TAG_ATOM:
			if value == 0:
				return W_NilObject()
			else:
				# TODO: maybe bad performance, if so fix it.
				return W_AtomObject(cp.atoms[value-1])
		elif tag == opcodes.TAGX_LITERAL:
			t = cp.lit_table[value]
			return self.term_to_value(t)
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
		v = self.get_basic_value(cp, test_v)
		if isinstance(v, class_name):
			return pc
		else:
			return cp.label_to_addr(label)

	def term_to_value(self, t):
		if isinstance(t, NewFloatTerm):
			return W_FloatObject(t.floatval)
		elif isinstance(t, SmallIntegerTerm):
			return W_IntObject(t.val)
		elif isinstance(t, AtomTerm):
			return W_AtomObject(t.value)
		elif isinstance(t, SmallTupleTerm):
			o_lst = [self.term_to_value(e) for e in t.vals]
			return W_TupleObject(o_lst)
		elif isinstance(t, IntListTerm):
			lst = t.vals
			i_lst = []
			for intval in lst:
				i_lst.append(W_IntObject(intval))
			return self.build_list_object(i_lst)
		elif isinstance(t, AnyListTerm):
			lst = t.vals
			o_lst = []
			for t in lst:
				o_lst.append(self.term_to_value(t))
			return self.build_list_object(o_lst)
		else:
			W_IntObject(-999) # only used for type inference

	@jit.unroll_safe
	def build_list_object(self, object_lst):
		right = W_NilObject()
		length = len(object_lst)
		for i in range(0, length):
			right = W_ListObject(object_lst[length - i - 1], right)
		return right

	def exit(self, s):
		print s
		raise Exception()

	def fail(self, cp, pc, fclass, reason):
		#pretty_print.print_value(W_TupleObject([W_AtomObject('fail'),
			#W_AtomObject(fail_class.fail_names[fclass]),
			#reason]))
		if self.cont_stack.is_empty():
			if fclass == fail_class.THROWN:
				self.exit("not catch thrown exception")
			elif fclass == fail_class.EXIT:
				self.exit(pretty_print.value_str(reason))
			elif fclass == fail_class.ERROR:
				stack_trace = self.create_call_stack_info(cp, pc)
				res = W_TupleObject([reason, stack_trace])
				self.exit(pretty_print.error_message(res))
		else:
			(label, depth) = self.cont_stack.top()
			new_depth = self.y_reg.depth()
			self.deallcate(new_depth - depth)
			self.x_reg.store(0, None)
			self.x_reg.store(1, W_AtomObject(fail_class.fail_names[fclass]))
			self.x_reg.store(2, reason)
			return W_AddrObject(cp.label_to_addr(label))

	@jit.unroll_safe
	def create_call_stack_info(self, cp, pc):
		res = [self._one_call_stack_info(cp, pc)]
		_cp = cp
		i = 0
		while(i < self.y_reg.depth()):
			obj = self.y_reg.get(-(i+1))
			i += 1
			if isinstance(obj, W_AddrObject):
				res.append(self._one_call_stack_info(_cp, obj.addrval))
			elif isinstance(obj, W_CodeParserWrapperObject):
				_cp = obj.cp
				w_addr = self.y_reg.get(-(i+1))
				i += 1
				assert isinstance(w_addr, W_AddrObject)
				res.append(self._one_call_stack_info(_cp, w_addr.addrval))
		#res.reverse()
		return self.build_list_object(res)

	def _one_call_stack_info(self, cp, pc):
		(module_name, func_name, arity) = cp.find_func_def(pc)
		line_number = cp.find_current_line(pc)
		return W_TupleObject([W_AtomObject(module_name),
			W_AtomObject(func_name),
			W_IntObject(arity),
			W_ListObject(W_TupleObject([W_AtomObject('file'), 
				#FIXME: it actually should be a string type,
				# and it also influent the error_message function
				W_AtomObject(cp.file_name)]), 
				W_ListObject(W_TupleObject([W_AtomObject('line'), 
					W_IntObject(line_number)])))])

	def apply_bif(self, cp, pc, fail, bif_index, rands, dst_reg):
		# TODO: wrap them with try-catch to handle inner exception.
		args = [self.get_basic_value(cp, rand) for rand in rands]
		bif = cp.func_list[bif_index]
		assert isinstance(bif, BaseBIF)
		bif.set_caller(self)
		res = bif.invoke(args)
		self.store_basereg(dst_reg, res)
		return pc

	def _call_ext_only(self, cp, entry):
		module_index = entry[0]
		func_index = entry[1]
		mod = cp.import_mods[module_index]
		label = cp.export_header[func_index][2]
		func_addr = mod.label_to_addr(label)
		return mod, func_addr

	def _call_ext_bif(self, pc, cp, header_index):
		fake_bif = cp.func_list[header_index]
		assert isinstance(fake_bif, BaseFakeFunc)
		res = fake_bif.invoke(cp, pc, self)
		if isinstance(res, W_AddrObject):
			return res.addrval
		else:
			self.x_reg.store(0, res)
			return pc

	def _spawn(self, cp, pc, args, priority):
		pid = self.scheduler.create_pid()
		sub_process = Process(pid, self.scheduler, priority)
		sub_process.init_entry_arguments(args)
		self.scheduler.push_to_priority_queue((sub_process, cp, pc), priority)
		self.scheduler.process_pool[pid] = sub_process
		return pid

########################################################################

	def call(self, pc, cp, arity, label):
		self.y_reg.push(W_AddrObject(pc))
		return cp.label_to_addr(label)

	def call_last(self, cp, arity, label, n):
		self.deallcate(n)
		return cp.label_to_addr(label)

	def call_only(self, cp, arity, label):
		return cp.label_to_addr(label)

	def call_ext(self, cp, pc, entry, real_arity):
		# TODO: add some check for two arities
		self.y_reg.push(W_AddrObject(pc))
		self.y_reg.push(W_CodeParserWrapperObject(cp))
		return self._call_ext_only(cp, entry)

	# 8
	def call_ext_last(self, cp, pc, entry, real_arity, dealloc):
		self.deallcate(dealloc)
		return self._call_ext_only(cp, entry)

	# 9
	def bif0(self, cp, pc, bif_index, dst_reg):
		# bif0 doesn't have fail jump label, so we
		# never care the return value of apply_bif here
		self.apply_bif(cp, pc, -1, bif_index, [], dst_reg)

	# 10
	def bif1(self, cp, pc, fail, bif_index, rand, dst_reg):
		return self.apply_bif(cp, pc, fail, bif_index, [rand], dst_reg)

	# 11
	def bif2(self, cp, pc, fail, bif_index, rand1, rand2, dst_reg):
		return self.apply_bif(cp, pc, fail, bif_index, [rand1, rand2], dst_reg)

	# 12
	@jit.unroll_safe
	def allocate(self, stack_need, live):
		for i in range(0, stack_need):
			self.y_reg.push(None)
		
	@jit.unroll_safe
	def allocate_zero(self, stack_need, live):
		for i in range(0, stack_need):
			self.y_reg.push(W_IntObject(0))

	# 16
	def test_heap(self, alloc, live):
		pass

	# 17
	def init(self, dst_reg):
		self.store_basereg(dst_reg, None)

	# 18
	@jit.unroll_safe
	def deallcate(self, n):
		for i in range(0, n):
			self.y_reg.pop()

	# 19
	def k_return(self, cp):
		obj = self.y_reg.pop()
		if isinstance(obj, W_CodeParserWrapperObject):
			cp = obj.cp
			w_addr = self.y_reg.pop()
			assert isinstance(w_addr, W_AddrObject)
			pc = w_addr.addrval
			return (cp, pc)
		else:
			assert isinstance(obj, W_AddrObject)
			pc = obj.addrval
			return (cp, pc)

	# 20
	def send(self):
		dst = self.x_reg.get(0)
		msg = self.x_reg.get(1)
		if isinstance(dst, W_PidObject):
			self._send_by_pid(dst, msg)
			self.x_reg.store(0, msg)
		#FIXME: we need another function for sending by name!

	# 21
	def remove_message(self):
		self.remove_current_message()
		
	# 23
	def loop_rec(self, pc, cp, label, dst_reg):
		val = self.current_message()
		if val:
			self.store_basereg(dst_reg, val)
			return pc
		else:
			return self.jump(cp, label)

	# 24
	def loop_rec_end(self, cp, label):
		self.go_to_next_message()
		return self.jump(cp, label)

	# 25
	def wait(self, cp, label):
		self.reset_message_to_head()
		return self.jump(cp, label)

	# 39
	def is_lt(self, pc, cp, label, v1, v2):
		int_v1 = self.get_basic_value(cp, v1)
		int_v2 = self.get_basic_value(cp, v2)
		if int_v1.lt(int_v2):
			return pc
		else:
			return cp.label_to_addr(label)

	# 43
	def is_eq_exact(self, pc, cp, label, test_reg, dst_reg):
		test_v = self.get_basic_value(cp, test_reg)
		dst_v = self.get_basic_value(cp, dst_reg)
		#print "[IS_EQ_EXACT] comparing %s and %s..."%(pretty_print.value_str(test_v), pretty_print.value_str(dst_v))
		if test_v.is_equal(dst_v):
			#print "[IS_EQ_EXACT] result: 
			return pc
		else:
			return cp.label_to_addr(label)

	# 48
	def is_atom(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_AtomObject)

	def is_nil(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_NilObject)
		#value = self.get_basic_value(test_v)
		#if not isinstance(value, W_NilObject):
			#cp.jump_label(label)

	# 56
	def is_nonempty_list(self, pc, cp, label, test_v):
		value = self.get_basic_value(cp, test_v)
		if isinstance(value, W_NilObject):
			return cp.label_to_addr(label)
		else:
			return pc

	# 57
	def is_tuple(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_TupleObject)

	# 58
	def test_arity(self, pc, cp, label, src_reg, size):
		val = self.get_basic_value(cp, src_reg)
		if isinstance(val, W_TupleObject) and val.size() == size:
			return pc
		else:
			return self.jump(cp, label)

	# 59
	@jit.unroll_safe
	def select_val(self, cp, val_reg, label, slist):
		val = self.fetch_basereg(val_reg)
		assert isinstance(val, W_AtomObject)
		atom_str = val.strval
		#print "select_val:"
		#print "atom: %d"%(val.index)
		for i in range(0, len(slist)):
			(v, l) = slist[i]
			if cp.atoms[v-1] == atom_str:
				return cp.label_to_addr(l)
		return cp.label_to_addr(label)

	# 61
	def jump(self, cp, label):
		return cp.label_to_addr(label)

	# 62
	def k_catch(self, cp, reg, label):
		addr = cp.label_to_addr(label)
		self.store_basereg(reg, W_AddrObject(addr))
		self.cont_stack.push((label, self.y_reg.depth()))

	# 63
	def catch_end(self, pc, cp, reg):
		self.store_basereg(reg, None)
		(fail_addr, depth) = self.cont_stack.pop()
		x0 = self.x_reg.get(0)
		if not x0: # it means x0 is a none value
			x1 = self.x_reg.get(1)
			x2 = self.x_reg.get(2)
			assert isinstance(x1, W_AtomObject)
			atom_val = x1.strval
			if atom_val == fail_class.fail_names[fail_class.THROWN]:
				self.x_reg.store(0, x2)
			elif atom_val == fail_class.fail_names[fail_class.ERROR]:
				self.x_reg.store(0, W_TupleObject([x2, self.create_call_stack_info(cp, pc)]))
			else:
				self.x_reg.store(0, W_TupleObject([W_AtomObject('EXIT'), x2]))
		
	# 64
	def move(self, cp, source, dst_reg):
		self.store_basereg(dst_reg, self.get_basic_value(cp, source))

	# 65
	def get_list(self, src_reg, head_reg, tail_reg):
		lst = self.fetch_basereg(src_reg)
		assert isinstance(lst, W_ListObject)
		self.store_basereg(head_reg, lst.head())
		self.store_basereg(tail_reg, lst.tail())

	# 66
	def get_tuple_element(self, cp, src_reg, index, dst_reg):
		val = self.get_basic_value(cp, src_reg)
		assert isinstance(val, W_TupleObject)
		e = val.element(index)
		self.store_basereg(dst_reg, e)

	# 69
	def put_list(self, head_reg, tail_reg, dst_reg):
		head = self.fetch_basereg(head_reg)
		tail = self.fetch_basereg(tail_reg)
		res = W_ListObject(head, tail)
		self.store_basereg(dst_reg, res)

	# 70
	def put_tuple(self, arity, dst_reg):
		if arity == 0:
			self.store_basereg(dst_reg, W_TupleObject([]))
		else:
			self.tuple_dst = dst_reg
			self.tuple_arity = arity
			self.tuple_data = []

	# 71
	def put(self, cp, src):
		val = self.get_basic_value(cp, src)
		self.tuple_data.append(val)
		if self.tuple_arity == 1:
			dst_reg = self.tuple_dst
			self.store_basereg(dst_reg, W_TupleObject(self.tuple_data))

			# reset the tuple data area to default value
			self.tuple_dst = constant.INVALID_REG
			self.tuple_arity = 0
			self.tuple_data = []
		else:
			self.tuple_arity -= 1

	def call_fun(self, pc, arity):
		self.y_reg.push(W_AddrObject(pc))
		addr_obj = self.fetch_basereg((opcodes.TAG_XREG, arity))
		assert(isinstance(addr_obj, W_AddrObject))
		return addr_obj.addrval

	def call_ext_only(self, cp, entry, real_arity):
		return self._call_ext_only(cp, entry)

	def make_fun2(self, cp, index):
		label = cp.loc_table[index][2]
		self.store_basereg((opcodes.TAG_XREG, 0), W_AddrObject(cp.label_to_addr(label)))

	def gc_bif2(self, cp, pc, fail, alive, bif_index, rand1, rand2, dst_reg):
		# TODO: maybe we can help GC with alive?
		return self.bif2(cp, pc, fail, bif_index, rand1, rand2, dst_reg)
