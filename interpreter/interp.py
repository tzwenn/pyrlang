import sys

from pyrlang.rpybeam import opcodes
from pyrlang.rpybeam import pretty_print
from pyrlang.rpybeam.instruction import Instruction, ListInstruction, PatternMatchingInstruction, PatternMatchingListInstruction, LoopInstruction
from pyrlang.interpreter import fail_class
from pyrlang.rpybeam.beam_file import *
from pyrlang.interpreter.register import X_Register, Y_Register
from pyrlang.interpreter.cont_stack import ContinuationStack
from pyrlang.interpreter.atom_table import global_atom_table
from pyrlang.interpreter.datatypes.root import W_Root
from pyrlang.interpreter.datatypes.pid import W_PidObject
from pyrlang.interpreter.datatypes.number import W_AbstractIntObject, W_IntObject, W_FloatObject
from pyrlang.interpreter.datatypes.list import W_ListObject, W_NilObject, W_StrListObject
from pyrlang.interpreter.datatypes.tuple import W_TupleObject
from pyrlang.interpreter.datatypes.inner import W_AddrObject 
from pyrlang.interpreter.datatypes.atom import W_AtomObject
from pyrlang.interpreter.datatypes.closure import W_ClosureObject
from pyrlang.interpreter import constant
from pyrlang.interpreter import abstract_stack
from pyrlang.utils.deque import MessageDeque
from pyrlang.utils import eterm_operators
from pyrlang.lib.base import BaseBIF, BaseBIF0, BaseFakeFunc
from rpython.rlib import jit
from rpython.rlib.jit import hint

def printable_loc(pc, _call_pc, cp):
	instr = cp.instrs[pc]
	return "%d %s"%(pc, pretty_print.instr_str(cp, instr))

driver = jit.JitDriver(greens = ['pc', 'call_pc', 'cp'],
		reds = ['reduction', 
			#'init_stack_depth', 
			#'call_jit_lock', 
			'single', 's_self', 'x_reg', 'y_reg'],
		virtualizables = ['x_reg'],
		get_printable_location=printable_loc)

class Process:
	_immutable_fields_ = ['pid']
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
	def execute(self, cp, func_addr, single, reduction):
		pc = func_addr
		x_reg = self.x_reg
		#print "execute in reduction %d"%(reduction)
		reduction = hint(reduction, promote=True)
		single = hint(single, promote=True)
		self = hint(self, promote=True)

		#################################################
		#self.counter_n = 0 # use it for experiment counting, DON'T forget to discard it !!!
		#################################################

		#jump_pc = pc
		call_pc = pc
		#init_stack_depth = -1
		#call_jit_lock = False

		while(True):
			driver.jit_merge_point(pc = pc,
					call_pc = call_pc,
					cp = cp,
					#jump_pc = jump_pc,
					reduction = reduction,
					#init_stack_depth = init_stack_depth,
					#call_jit_lock = call_jit_lock,
					single = single, 
					s_self = self,
					x_reg = x_reg,
					y_reg = self.y_reg)
			#print pretty_print.value_str(self.pid) + ": [" + cp.file_name + "]" + printable_loc(pc, call_pc, cp) + " reduction: " + str(reduction)
			should_enter = False
			instr_obj = cp.instrs[pc]
			#call_pc = pc
			pc = pc + 1
			if isinstance(instr_obj, PatternMatchingListInstruction) or isinstance(instr_obj, PatternMatchingInstruction):
				should_enter = True

			instr = instr_obj.opcode
			depth = -1

			if instr == opcodes.LABEL: # 1
				pass

			elif instr == opcodes.CALL: # 4
				(arity, label) = instr_obj.arg_values()
				call_pc = pc - 1
				#is_two_state_match = call_pc == jump_pc
				frame = (cp, pc)
				pc = self.call(frame, arity, label)
				reduction -= 1
				if not single and reduction <= 0:
					break
				#else:
					#if not call_jit_lock:
						#should_enter = True
						#if init_stack_depth == -1:
							#init_stack_depth = self.y_reg.depth()-1
					# for testing
					#should_enter = True

			elif instr == opcodes.CALL_LAST: # 5
				(arity, label, n) = instr_obj.arg_values()
				pc = self.call_last(cp, arity, label, n)
				#reduction -= 1
				#if not single and reduction <= 0:
					#break

			elif instr == opcodes.CALL_ONLY: # 6
				(arity, label) = instr_obj.arg_values()
				#call_pc = pc
				pc = self.call_only(cp, arity, label)
				reduction -= 1
				if not single and reduction <= 0:
					break
				# for testing
				#else:
					#assert isinstance(instr_obj, LoopInstruction)
					#old_depth = instr_obj.depth
					#if self.y_reg.depth() == old_depth:
						#should_enter = True
					#else:
						#instr_obj.depth = self.y_reg.depth()

			elif instr == opcodes.CALL_EXT: # 7
				args = instr_obj.args
				real_arity = args[0][1]
				(tag, header_index) = args[1]
				if (tag == opcodes.TAG_LITERAL):
					entry = cp.import_header[header_index]
					call_pc = pc
					#is_two_state_match = call_pc == jump_pc
					frame = (cp, pc)
					cp, pc = self.call_ext(frame, entry, real_arity)
					reduction -= 1
					if not single and reduction <=0:
						break
					should_enter = True
				else:
					assert tag == opcodes.TAG_LABEL
					self.y_reg.push((cp, pc))
					cp, pc = self._call_ext_bif(pc, cp, header_index)
					# calling a bif means the dispatch loop 
					# need a extra k_return semantics 
					if self.y_reg.is_empty():
						return (constant.STATE_TERMINATE, pc, cp)
					else:
						(cp, pc) = self.k_return(cp)

			elif instr == opcodes.CALL_EXT_LAST: # 8
				args = instr_obj.args
				real_arity = args[0][1]
				(tag, header_index) = args[1]
				dealloc = args[2][1]
				if (tag == opcodes.TAG_LITERAL):
					entry = cp.import_header[header_index]
					cp, pc = self.call_ext_last(cp, pc, entry, real_arity, dealloc)
				else:
					assert tag == opcodes.TAG_LABEL
					cp, pc = self._call_ext_bif(pc, cp, header_index)
					self.deallocate(dealloc)
					# calling a bif means the dispatch loop 
					# need a extra k_return semantics 
					if self.y_reg.is_empty():
						return (constant.STATE_TERMINATE, pc, cp)
					else:
						(cp, pc) = self.k_return(cp)

			elif instr == opcodes.BIF0: # 9
				((_, bif_index), dst_reg) = instr_obj.args
				self.bif0(cp, pc, bif_index, dst_reg)

			elif instr == opcodes.BIF1: # 10
				args = instr_obj.args
				fail = args[0][1]
				bif_index = args[1][1]
				rand1 = args[2]
				dst_reg = args[3]
				pc = self.bif1(cp, pc, fail, 
						cp.import_header[bif_index][1], rand1, dst_reg)

			elif instr == opcodes.BIF2: # 11
				args = instr_obj.args
				fail = args[0][1]
				bif_index = args[1][1]
				rand1 = args[2]
				rand2 = args[3]
				dst_reg = args[4]
				pc = self.bif2(cp, pc, fail, 
						cp.import_header[bif_index][1], rand1, rand2, dst_reg)

			elif instr == opcodes.ALLOCATE: # 12
				(stack_need, live) = instr_obj.arg_values()
				self.allocate(stack_need, live)

			elif instr == opcodes.ALLOCATE_HEAP: # 13
				(stack_need, heap_need, live) = instr_obj.arg_values()
				self.allocate_heap(stack_need, heap_need, live)

			elif instr == opcodes.ALLOCATE_ZERO: # 14
				(stack_need, live) = instr_obj.arg_values()
				self.allocate_zero(stack_need, live)

			elif instr == opcodes.ALLOCATE_HEAP_ZERO: # 15
				(stack_need, heap_need, live) = instr_obj.arg_values()
				self.allocate_heap_zero(stack_need, heap_need, live)

			elif instr == opcodes.TEST_HEAP: # 16
				(term1, term2) = instr_obj.args
				self.test_heap(term1, term2)

			elif instr == opcodes.INIT: # 17
				dst_reg = instr_obj.args[0]
				self.init(dst_reg)

			elif instr == opcodes.DEALLOCATE: # 18
				(n,) = instr_obj.arg_values()
				self.deallocate(n)

			elif instr == opcodes.K_RETURN: # 19
				if self.y_reg.is_empty():
					return (constant.STATE_TERMINATE, pc, cp)
				else:
					call_pc = pc-1
					(cp, pc) = self.k_return(cp)
					# try to trace RETURN instruction, too
					should_enter = True
					#if call_jit_lock and init_stack_depth == self.y_reg.depth()+1:
						#init_stack_depth = -1
						#call_jit_lock = False
					#else:
						#call_jit_lock = True
					#print "return", "initial stack depth:%d"%init_stack_depth, "lock:%r"%call_jit_lock, "depth:%d"%self.y_reg.depth()

			elif instr == opcodes.SEND: # 20
				self.send()

			elif instr == opcodes.REMOVE_MESSAGE: # 21
				self.remove_message()

			elif instr == opcodes.LOOP_REC: # 23
				args = instr_obj.args
				label = args[0][1]
				dst_reg = args[1]
				pc = self.loop_rec(pc, cp, label, dst_reg)

			elif instr == opcodes.LOOP_REC_END: # 24
				(label,) = instr_obj.arg_values()
				pc = self.loop_rec_end(cp, label)

			elif instr == opcodes.WAIT: # 25
				(label,) = instr_obj.arg_values()
				pc = self.wait(cp, label)
				return (constant.STATE_HANG_UP, pc, cp)

			elif instr == opcodes.IS_LT: # 39
				((_, label), term1, term2) = instr_obj.args
				pc = self.is_lt(pc, cp, label, term1, term2)

			elif instr == opcodes.IS_GE: # 40
				((_, label), term1, term2) = instr_obj.args
				pc = self.is_ge(pc, cp, label, term1, term2)

			elif instr == opcodes.IS_EQ: # 41
				((_, label), term1, term2) = instr_obj.args
				pc = self.is_eq(pc, cp, label, term1, term2)

			elif instr == opcodes.IS_EQ_EXACT: # 43
				# TODO: maybe other data types here
				((_, next_addr), test_reg, dst_reg) = instr_obj.args
				pc = self.is_eq_exact(pc, cp, next_addr, test_reg, dst_reg)

			elif instr == opcodes.IS_INTEGER: # 45
				((_, label), test_v) = instr_obj.args
				pc = self.is_integer(pc, cp, label, test_v)

			elif instr == opcodes.IS_FLOAT: # 46
				((_, label), test_v) = instr_obj.args
				pc = self.is_float(pc, cp, label, test_v)

			elif instr == opcodes.IS_ATOM: # 48
				((_, label), test_v) = instr_obj.args
				pc = self.is_atom(pc, cp, label, test_v)

			elif instr == opcodes.IS_NIL: # 52
				((_, label), test_v) = instr_obj.args
				pc = self.is_nil(pc, cp, label, test_v)

			elif instr == opcodes.IS_LIST: # 55
				((_, label), test_v) = instr_obj.args
				pc = self.is_nil(pc, cp, label, test_v)

			elif instr == opcodes.IS_NONEMPTY_LIST: # 56
				((_, label), test_v) = instr_obj.args
				pc = self.is_nonempty_list(pc, cp, label, test_v)

			elif instr == opcodes.IS_TUPLE: # 57
				((_, label), test_v) = instr_obj.args
				pc = self.is_tuple(pc, cp, label, test_v)

			elif instr == opcodes.TEST_ARITY: # 58
				((_, label), src_reg, (_, size)) = instr_obj.args
				pc = self.test_arity(pc, cp, label, src_reg, size)
				
			elif instr == opcodes.SELECT_VAL: # 59
				assert isinstance(instr_obj, ListInstruction)
				(reg, (_, label)) = instr_obj.args
				sl = instr_obj.lst
				call_pc = pc
				pc = self.select_val(cp, reg, label, sl)

			elif instr == opcodes.JUMP: # 61
				(label, _) = instr_obj.arg_values()
				pc = self.jump(cp, label)

			elif instr == opcodes.K_CATCH: # 62
				(reg, (_, label)) = instr_obj.args
				self.k_catch(cp, reg, label)

			elif instr == opcodes.CATCH_END: # 63
				reg = instr_obj.args[0]
				self.catch_end(pc, cp, reg)

			elif instr == opcodes.MOVE: # 64
				(source, dst_reg) = instr_obj.args
				self.move(cp, source, dst_reg)

			elif instr == opcodes.GET_LIST: # 65
				(src_reg, head_reg, tail_reg) = instr_obj.args
				self.get_list(src_reg, head_reg, tail_reg)

			elif instr == opcodes.GET_TUPLE_ELEMENT: # 66
				(src_reg, (_, index), dst_reg) = instr_obj.args
				self.get_tuple_element(cp, src_reg, index, dst_reg)

			elif instr == opcodes.PUT_LIST: # 69
				(head_reg, tail_reg, dst_reg) = instr_obj.args
				self.put_list(cp, head_reg, tail_reg, dst_reg)

			elif instr == opcodes.PUT_TUPLE: # 70
				((_, arity), dst_reg) = instr_obj.args
				self.put_tuple(arity, dst_reg)

			elif instr == opcodes.PUT: # 71
				src = instr_obj.args[0]
				self.put(cp, src)

			elif instr == opcodes.BADMATCH: # 72
				(label,) = instr_obj.arg_values()
				pc = self.badmatch(pc, cp, label)

			elif instr == opcodes.IF_END: # 73
				pc = self.if_end(pc, cp)

			elif instr == opcodes.CALL_FUN: # 75
				(arity,) = instr_obj.arg_values()
				(cp, pc) = self.call_fun(pc, cp, arity)

			elif instr == opcodes.CALL_EXT_ONLY: # 78
				((_, real_arity), (tag, header_index)) = instr_obj.args
				if tag == opcodes.TAG_LITERAL:
					entry = cp.import_header[header_index]
					cp, pc = self.call_ext_only(cp, entry, real_arity)
				else:
					assert tag == opcodes.TAG_LABEL
					cp, pc = self._call_ext_bif(pc, cp, header_index)
					# calling a bif means the dispatch loop 
					# need a extra k_return semantics 
					if self.y_reg.is_empty():
						return (constant.STATE_TERMINATE, pc, cp)
					else:
						(cp, pc) = self.k_return(cp)

			elif instr == opcodes.MAKE_FUN2: # 103
				(index,) = instr_obj.arg_values()
				self.make_fun2(cp, index)

			elif instr == opcodes.IS_FUNCTION2: # 115
				((_, label), a1, a2) = instr_obj.args
				pc = self.is_function2(pc, cp, label, a1, a2)

			elif instr == opcodes.GC_BIF1: # 124
				((_,fail), (_,alive), (_,bif_index), rand1, dst_reg) = instr_obj.args
				pc = self.gc_bif1(cp, pc, fail, alive,
						cp.import_header[bif_index][1], rand1, dst_reg)

			elif instr == opcodes.GC_BIF2: # 125
				#################################################
				#self.counter_n += 1
				#################################################
				((_,fail), (_,alive), (_,bif_index), rand1, rand2, dst_reg) = instr_obj.args
				pc = self.gc_bif2(cp, pc, fail, alive, 
						cp.import_header[bif_index][1], rand1, rand2, dst_reg)
				#reduction -= 1
				#if not single and reduction <= 0:
					#break

			elif instr == opcodes.TRIM: # 136
				(n, remaining) = instr_obj.arg_values()
				self.trim(n, remaining)

			elif instr == opcodes.LINE: # 153
				pass
			else:
				pretty_print.print_value(self.create_call_stack_info(cp, pc))
				raise Exception("Unimplemented opcode: %d"%(instr))
			if should_enter:
				driver.can_enter_jit(pc = pc,
						call_pc = call_pc,
						cp = cp,
						reduction = reduction,
						#init_stack_depth = init_stack_depth,
						#call_jit_lock = call_jit_lock,
						#should_enter = should_enter, 
						single = single, 
						s_self = self,
						x_reg = x_reg,
						y_reg = self.y_reg)

		return (constant.STATE_SWITH, pc, cp)

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
			return cp.const_table[value]
		elif tag == opcodes.TAG_ATOM:
			if value == 0:
				return constant.CONST_NIL
			else:
				return global_atom_table.get_obj_at(value)
		elif tag == opcodes.TAGX_LITERAL:
			return cp.lit_table[value]
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

	def exit(self, s):
		print s
		raise Exception()

	# the args is used in erlang:error
	def fail(self, cp, pc, fclass, reason, args = None):
		if self.cont_stack.is_empty():
			if fclass == fail_class.THROWN:
				self.exit("not catch thrown exception")
			elif fclass == fail_class.EXIT:
				self.exit(pretty_print.value_str(reason))
			elif fclass == fail_class.ERROR:
				stack_trace = self.create_call_stack_info(cp, pc, args)
				res = W_TupleObject([reason, stack_trace])
				self.exit(pretty_print.error_message(res))
		else:
			(label, depth) = self.cont_stack.top()
			new_depth = self.y_reg.depth()
			self.deallocate(new_depth - depth)
			self.x_reg.store(0, None)
			self.x_reg.store(1, global_atom_table.get_obj_from_str(fail_class.fail_names[fclass]))
			self.x_reg.store(2, reason)
			return W_AddrObject(cp, cp.label_to_addr(label))

	@jit.unroll_safe
	def create_call_stack_info(self, cp, pc, args = None):
		res = [self._one_call_stack_info(cp, pc, args)]
		_cp = cp
		i = 0
		while(i < self.y_reg.depth()):
			obj = self.y_reg.get(-(i+1))
			i += 1
			assert isinstance(obj, W_AddrObject)
			res.append(self._one_call_stack_info(obj.cp, obj.pc))
		#res.reverse()
		return eterm_operators.build_list_object(res)

	def _one_call_stack_info(self, cp, pc, args = None):
		(module_name, func_name, arity) = cp.find_func_def(pc)
		line_number = cp.find_current_line(pc)
		if args:
			arg_part = args
		else:
			arg_part = W_IntObject(arity)
		return W_TupleObject([global_atom_table.get_obj_from_str(module_name),
			global_atom_table.get_obj_from_str(func_name),
			arg_part,
			W_ListObject(W_TupleObject([global_atom_table.get_obj_from_str('file'), 
				#FIXME: it actually should be a string type,
				# and it also influent the error_message function
				eterm_operators.build_strlist_object([W_IntObject(ord(c)) for c in cp.file_name])]), 
				W_ListObject(W_TupleObject([global_atom_table.get_obj_from_str('line'), 
					W_IntObject(line_number)])))])

	@jit.unroll_safe
	def apply_bif(self, cp, pc, fail, bif, rands, dst_reg):
		# TODO: wrap them with try-catch to handle inner exception.
		args = [self.get_basic_value(cp, rand) for rand in rands]
		assert isinstance(bif, BaseBIF)
		res = bif.invoke(args)
		self.store_basereg(dst_reg, res)
		return pc

	def _call_ext_only(self, cp, entry):
		module_index = entry[0]
		func_index = entry[1]
		mod = cp.import_mods[module_index]
		label = mod.export_header[func_index][2]
		func_addr = mod.label_to_addr(label)
		return mod, func_addr

	def _call_ext_bif(self, pc, cp, header_index):
		fake_bif = cp.func_list[header_index]
		assert isinstance(fake_bif, BaseFakeFunc)
		res = fake_bif.invoke(cp, pc, self)
		if isinstance(res, W_AddrObject):
			return res.cp, res.pc
		else:
			self.x_reg.store(0, res)
			return cp, pc

	def _spawn(self, cp, pc, args, priority):
		pid = self.scheduler.create_pid()
		sub_process = Process(pid, self.scheduler, priority)
		sub_process.init_entry_arguments(args)
		self.scheduler.push_to_priority_queue((sub_process, cp, pc), priority)
		self.scheduler.process_pool[pid] = sub_process
		return pid

########################################################################

	# 4
	def call(self, frame, arity, label):
		self.y_reg.push(frame)
		return frame[0].label_to_addr(label)

	# 5
	def call_last(self, cp, arity, label, n):
		self.deallocate(n)
		return cp.label_to_addr(label)

	# 6
	def call_only(self, cp, arity, label):
		return cp.label_to_addr(label)

	# 7
	def call_ext(self, frame, entry, real_arity):
		# TODO: add some check for two arities
		self.y_reg.push(frame)
		return self._call_ext_only(frame[0], entry)

	# 8
	def call_ext_last(self, cp, pc, entry, real_arity, dealloc):
		self.deallocate(dealloc)
		return self._call_ext_only(cp, entry)

	# 9
	def bif0(self, cp, pc, bif_index, dst_reg):
		# bif0 doesn't have fail jump label, so we
		# never care the return value of apply_bif here
		bif = cp.func_list[bif_index]
		assert isinstance(bif, BaseBIF0)
		bif.set_caller(self)
		self.apply_bif(cp, pc, -1, bif, [], dst_reg)

	# 10
	def bif1(self, cp, pc, fail, bif_index, rand, dst_reg):
		bif = cp.func_list[bif_index]
		return self.apply_bif(cp, pc, fail, bif, [rand], dst_reg)

	# 11
	def bif2(self, cp, pc, fail, bif_index, rand1, rand2, dst_reg):
		bif = cp.func_list[bif_index]
		return self.apply_bif(cp, pc, fail, bif, [rand1, rand2], dst_reg)

	# 12
	def allocate(self, stack_need, live):
		self.y_reg.allocate(stack_need)

	# 13
	def allocate_heap(self, stack_need, heap_need, live):
		self.y_reg.allocate(stack_need)
		
	# 14
	def allocate_zero(self, stack_need, live):
		self.y_reg.allocate(stack_need, constant.CONST_0)

	# 15
	def allocate_heap_zero(self, stack_need, heap_need, live):
		self.y_reg.allocate(stack_need, constant.CONST_0)

	# 16
	def test_heap(self, alloc, live):
		pass

	# 17
	def init(self, dst_reg):
		self.store_basereg(dst_reg, None)

	# 18
	@jit.unroll_safe
	def deallocate(self, n):
		self.y_reg.deallocate(n)

	# 19
	def k_return(self, cp):
		return self.y_reg.pop()

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

	# 40
	def is_ge(self, pc, cp, label, v1, v2):
		int_v1 = self.get_basic_value(cp, v1)
		int_v2 = self.get_basic_value(cp, v2)
		if not int_v1.lt(int_v2):
			return pc
		else:
			return cp.label_to_addr(label)

	# 41
	def is_eq(self, pc, cp, label, v1, v2):
		w_v1 = self.get_basic_value(cp, v1)
		w_v2 = self.get_basic_value(cp, v2)
		if isinstance(w_v1, W_AbstractIntObject) or isinstance(w_v1, W_FloatObject):
			if w_v1.is_rough_equal(w_v2):
				return pc
			else:
				return cp.label_to_addr(label)
		else:
			if w_v1.is_equal(w_v2):
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

	# 45
	def is_integer(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_AbstractIntObject)

	# 46
	def is_float(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_FloatObject)

	# 48
	def is_atom(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_AtomObject)

	# 52
	def is_nil(self, pc, cp, label, test_v):
		return self.not_jump(pc, cp, label, test_v, W_NilObject)
		#value = self.get_basic_value(test_v)
		#if not isinstance(value, W_NilObject):
			#cp.jump_label(label)

	# 55
	def is_list(self, pc, cp, label, test_v):
		value = self.get_basic_value(cp, test_v)
		if isinstance(value, W_ListObject) or isinstance(value, W_NilObject):
			return cp.label_to_addr(label)
		else:
			return pc

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
		val = self.get_basic_value(cp, val_reg)
		for i in range(0, len(slist)):
			((tag, v), l) = slist[i]
			if tag == opcodes.TAG_ATOM and val.is_equal(global_atom_table.get_obj_at(v)):
				return cp.label_to_addr(l)
			elif tag == opcodes.TAG_INTEGER and val.is_equal(cp.const_table[v]):
				return cp.label_to_addr(l)
		return cp.label_to_addr(label)

	# 61
	def jump(self, cp, label):
		return cp.label_to_addr(label)

	# 62
	def k_catch(self, cp, reg, label):
		addr = cp.label_to_addr(label)
		self.store_basereg(reg, W_AddrObject(cp, addr))
		self.cont_stack.push((label, self.y_reg.depth()))

	# 63
	def catch_end(self, pc, cp, reg):
		self.store_basereg(reg, None)
		(fail_addr, depth) = self.cont_stack.pop()
		x0 = self.x_reg.get(0)
		if not x0: # it means x0 is a none value
			x1 = self.x_reg.get(1)
			x2 = self.x_reg.get(2)
			atom_val = eterm_operators.get_atom_val(x1)
			if atom_val == fail_class.fail_names[fail_class.THROWN]:
				self.x_reg.store(0, x2)
			elif atom_val == fail_class.fail_names[fail_class.ERROR]:
				self.x_reg.store(0, W_TupleObject([x2, self.create_call_stack_info(cp, pc)]))
			else:
				self.x_reg.store(0, W_TupleObject([global_atom_table.get_obj_from_str('EXIT'), x2]))
		
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
	@jit.unroll_safe
	def put_list(self, cp, head_reg, tail_reg, dst_reg):
		head = self.get_basic_value(cp, head_reg)
		tail = self.get_basic_value(cp, tail_reg)
		tag = head_reg[0]
		tag = hint(tag, promote=True)
		if tag is opcodes.TAG_XREG or tag is opcodes.TAG_YREG:
			res = W_ListObject(head, tail)
		else:
			res = W_StrListObject(head, tail)
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

	# 72
	def badmatch(self, pc, cp, label):
		if label == 0:
			addr = self.fail(cp, pc, fail_class.ERROR, global_atom_table.get_obj_from_str('badmatch'))
			return eterm_operators.get_addr_val(addr)
		else:
			return self.jump(cp, label)

	# 73
	def if_end(self, pc, cp):
		addr = self.fail(cp, pc, fail_class.ERROR, global_atom_table.get_obj_from_str('if_clause'))
		return eterm_operators.get_addr_val(addr)

	# 75
	@jit.unroll_safe
	def call_fun(self, pc, cp, arity):
		self.y_reg.push((cp, pc))
		closure = self.fetch_basereg((opcodes.TAG_XREG, arity))
		(cp, addr, real_arity, fvs) = eterm_operators.get_closure_fields(closure)
		for i in range(len(fvs)):
			self.x_reg.store(arity + i, fvs[i])
		return cp, addr

	def call_ext_only(self, cp, entry, real_arity):
		return self._call_ext_only(cp, entry)

	# 103
	@jit.unroll_safe
	def make_fun2(self, cp, index):
		fun_entry = cp.fun_table[index]
		label = fun_entry.label_index
		addr = cp.label_to_addr(label)
		fvs = []
		for i in range(fun_entry.num_free):
			fvs.append(self.x_reg.get(i))
		closure_obj = W_ClosureObject(cp, addr, fun_entry.arity, fvs)
		self.store_basereg((opcodes.TAG_XREG, 0), closure_obj)

	# 115
	def is_function2(self, pc, cp, label, a1, a2):
		a1_v = self.get_basic_value(cp, a1) # the object of closure
		a2_v = self.get_basic_value(cp, a2) # the arity
		if isinstance(a1_v, W_ClosureObject):
			if a1_v.arity - len(a1_v.free_variables()) == eterm_operators.get_int_val(a2_v):
				return pc
		return cp.label_to_addr(label)

	# 124
	def gc_bif1(self, cp, pc, fail, alive, bif_index, rand1, dst_reg):
		return self.bif1(cp, pc, fail, bif_index, rand1, dst_reg)

	# 125
	def gc_bif2(self, cp, pc, fail, alive, bif_index, rand1, rand2, dst_reg):
		# TODO: maybe we can help GC with alive?
		return self.bif2(cp, pc, fail, bif_index, rand1, rand2, dst_reg)

	# 136
	def trim(self, n, remaining):
		self.deallocate(n)
