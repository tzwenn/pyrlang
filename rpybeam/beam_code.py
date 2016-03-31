from rpython.rlib.rstruct.runpack import runpack
from rpython.rlib import jit
from pyrlang.interpreter.mod_file_loader import ModFileLoader
from pyrlang.interpreter.atom_table import global_atom_table
from pyrlang.interpreter.datatypes.number import W_IntObject
from pyrlang.lib import ModuleDict
from pyrlang.rpybeam.beam_file import BeamRoot
from pyrlang.rpybeam.instruction import Instruction, ListInstruction, PatternMatchingListInstruction, PatternMatchingInstruction, LoopInstruction
import opcodes
import pretty_print
import time

class CodeParser:
	_immutable_fields_ = ['file_name', 'instrs[*]', 'import_header[*]', 
			'parent_cp', 'total_lines', 'labelTable[*]', 
			'_import_header[*]', 'lit_table[*]', 'loc_table[*]', 'mod_dict[*]',
			'fun_table[*]','import_mods[*]','func_list[*]', 'func_dict[*]', 'export_header[*]',
			'const_table[*]']

	def __init__(self, beam, name, parent_cp = None):
		self.file_name = name
		self.parent_cp = parent_cp
		self.code = beam.getCode()
		self.total_lines = -1
		self.labelTable = []


		# atom preprocess
		(self._import_header, self.export_header) = self.header_atom_preprocess(beam)

		self.lit_table = None
		self.loc_table = None
		self.fun_table = None
		if beam.litTChunk:
			self.lit_table = beam.litTChunk.asArray()[:]
		if beam.locTChunk:
			self.loc_table = beam.locTChunk.asArray()[:]
		if beam.funTChunk:
			self.fun_table = beam.funTChunk.asArray()[:]
			#print "loc_table:"
			#print self.loc_table
		#self.mod_dict = {} # mod atom index => import_mods' index

		# func_list: index => bif
		# func_dict: function name atom index => index in func_list (used for apply/1, apply/2, apply/3)
		# import_header: index => entry
		# import_mods: index => cp object
		# mod_dict: module atom index => index in import_mods
		(self.func_list, self.func_dict, self.import_header, self.import_mods, self.mod_dict) = self.import_BIF_and_module()
		(self.labelTable, self.instrs, self.const_table) = self.preprocess(beam)
		#print "\n".join([pretty_print.instr_str(self, instr) + ":  " + str(instr.__class__) for instr in self.instrs])
		#exit()
		#print [v.intval for v in self.const_table]

	def header_atom_preprocess(self, beam):
		atoms = beam.getAtomTable()
		self.name = atoms[0]
		global_atom_table.register_atoms(atoms)
		_import_header = list(beam.impTChunk.asArray())
		export_header = beam.expTChunk.asArray()[:]
		for i in range(len(_import_header)):
			(_mod, _fun, arity) = _import_header[i]
			mod = global_atom_table.search_index(atoms[_mod-1])
			fun = global_atom_table.search_index(atoms[_fun-1])
			_import_header[i] = (mod, fun, arity)
		for i in range(len(export_header)):
			(_fun, arity, label) = export_header[i]
			fun = global_atom_table.search_index(atoms[_fun-1])
			export_header[i] = (fun, arity, label)
		return _import_header, export_header

	@jit.unroll_safe
	def import_BIF_and_module(self):
		mfl = ModFileLoader()
		import_mods = []
		import_header = self._import_header[:]
		func_list = []
		func_dict = {}
		mod_dict = {}
		for i in range(0, len(import_header)):
			entry = import_header[i];
			module_atom_index = entry[0]
			func_atom_index = entry[1]
			arity = entry[2]
			moduleName = global_atom_table.get_str_at(module_atom_index)
			funcName = global_atom_table.get_str_at(func_atom_index)
			#print "prepare importing function: %s:%s/%d..." % (moduleName, funcName, arity)
			# BIF
			if moduleName in ModuleDict.module_dict and ModuleDict.is_bif_from_tuple(self.get_name_entry(entry)):
				# we use function_arity to emulate function overload
				moduleEntity = ModuleDict.module_dict[moduleName]()
				bif_name = ModuleDict.get_bif_name(funcName, arity)
				import_header[i] = (entry[0], len(func_list), entry[2])
				func_dict[func_atom_index] = len(func_list)
				func_list.append(moduleEntity.searchFunc(bif_name)())
			elif not moduleName == self.get_self_module_name():
				if module_atom_index not in mod_dict:
					#print "searching module [%s] by %s"%(moduleName, self.get_self_module_name())
					b = mfl.find(moduleName)
					mod_dict[module_atom_index] = len(import_mods)
					import_mods.append(CodeParser(b, moduleName + ".erl", self))
				mod_cp = import_mods[mod_dict[module_atom_index]]
				func_index = self.search_exports(funcName, 
						arity, mod_cp.export_header)
				import_header[i] = (mod_dict[module_atom_index], func_index, arity)
		return func_list[:], func_dict.copy(), import_header[:], import_mods[:], mod_dict.copy()

	def get_self_module_name(self):
		return self.name

	def get_module_cp_by_name(self, module_name):
		if module_name == self.get_self_module_name():
			return self
		else:
			mod_index = self.mod_dict[global_atom_table.search_index(module_name)]
			return self.import_mods[mod_index]

	def get_func_addr_by_name(self, func_name, arity):
		index = self.search_exports(func_name, arity, self.export_header)
		label = self.export_header[index][2]
		return self.label_to_addr(label)

	def get_name_entry(self, entry):
		return (global_atom_table.get_str_at(entry[0]),
				global_atom_table.get_str_at(entry[1]),
				entry[2])
					
	@jit.unroll_safe	
	def search_exports(self, func_name, arity, header):
		for i in range(0, len(header)):
			entry = header[i]
			if global_atom_table.get_str_at(entry[0]) == func_name and arity == entry[1]:
				return i
		else:
			raise Exception("Cannot found public function %s/%d"%(func_name, arity))

	def label_to_addr(self, label):
		#print "jump to label: %d"%(label)
		return self.labelTable[label-1]

	@jit.unroll_safe
	def _encode_hex(self, s):
		sum = 0
		for i in range(0, len(s)):
			hex_sum = ord(s[i])
			sum = sum << 8
			sum += hex_sum
		return sum

	@jit.unroll_safe
	def _next(self, pc, n=1):
		to_index = pc + n
		assert(pc >= 0)
		assert(to_index >= 0)
		return to_index, self.code[pc:to_index]

	def parseOne(self, pc):
		x = ord(self.code[pc])
		return pc+1, x

	def parseInstr(self, pc):
		return self.parseOne(pc)

	def parseTag(self, pc):
		pc, res = self._parseTag(pc)
		return pc+1, res

	def _parseTag(self, pc, tag = -1):
		if tag == -1:
			tag = ord(self.code[pc])
		if tag & 0x07 == opcodes.TAG_EXTENDED: # xxxxx111 actually
			return pc, (tag >> 4) + opcodes.TAGX_BASE
		else:
			return pc, tag & 0x07

	def isBaseTag(self, tag):
		return tag < opcodes.TAGX_BASE

	def parseInt(self, pc):
		pc, first = self.parseOne(pc)
		return self._parseInt(pc, first)

	def _parseInt(self, pc, first):
		pc, tag = self._parseTag(pc, first)
		#assert(tag < opcodes.TAGX_BASE)
		return self._createInt(pc, first)

	def build_atom(self, int_val):
		return hex(int_val << 4 | opcode.TAG_ATOM)

	def parseBase(self, pc):
		pc, first = self.parseOne(pc)
		pc, tag = self._parseTag(pc, first)
		if tag < opcodes.TAGX_BASE:
			pc, intval = self._parseInt(pc, first)
		else:
			pc, intval = self._parse_literal(pc)
		return pc, (tag, intval)

	def _createInt(self, pc, tag):
		if tag & 0x08: # xxxx1xxx actually, which means it need to care
			if tag & 0x10: # xxx1xxx actually, which means it's really a extended one
				if tag & 0xe0 == 0xe0:
					pc, tmp = self._createInt(pc, tag)
					length = tmp + (tag >> 5) + 2
					pc, tmp2 = self._next(pc, length)
					return pc, self._decode_bignit(tmp2)
				else:
					pc, tmp2 = self._next(pc, 2+(tag >> 5))
					return pc, self._decode_bignit(tmp2)
			else:
				pc, w = self.parseOne(pc)
				return pc, ((tag & 0xe0)<<3)|w
		else:
			return pc, tag >> 4

	def _decode_bignit(self, s):
		v = self._encode_hex(s)
		if ord(s[0]) > 0x80:
			return v-(1 << (len(s))*8)
		else:
			return v

	def parse_float(self, pc):
		pc, tag = self.parseTag(pc)
		assert(tag == opcodes.TAGX_FLOATLIT)
		return self._parse_float(pc)

	def _parse_float(self, pc):
		pc, tmp = self._next(pc, 4)
		return pc, runpack("f", tmp)

	def parse_floatreg(self, pc):
		pc, tag = self.parseTag(pc)
		assert(tag == opcodes.TAGX_FLOATREG)
		return self._parse_float(pc)

	def _parse_floatreg(self, pc):
		return self.parseInt(pc)

	def parse_literal(self, pc):
		pc, tag = self.parseTag(pc)
		assert(tag == opcodes.TAGX_LITERAL)
		return self._parse_literal(pc)

	def _parse_literal(self, pc):
		return self.parseInt(pc)

	def parse_alloclist(self, pc):
		pc, tag = self.parseTag(pc)
		assert(tag == opcodes.TAGX_ALLOCLIST)
		return self._parse_alloclist(pc)
	
	@jit.unroll_safe
	def _parse_alloclist(self, pc):
		pc, length = self.parseInt(pc)
		res = []
		for i in range(0, length):
			pc, tmp1 = self.parseBase(pc)
			pc, tmp2 = self.parseInt(pc)
			res.append((tmp1, tmp2))
		return pc, res

	def parse_selectlist(self, pc):
		#print "parse_selectlist:"
		#pretty_print.print_hex(self.code[self.offset:-1])
		#exit()
		pc, tag = self.parseTag(pc)
		assert(tag == opcodes.TAGX_SELECTLIST)
		return self._parse_selectlist(pc)

	@jit.unroll_safe
	def _parse_selectlist(self, pc):
		pc, length = self.parseInt(pc)
		res = []
		for i in range(0, length >> 1):
			pc, tmp1 = self.parseBase(pc)
			pc, tmp2 = self.parseInt(pc)
			res.append((tmp1, tmp2))
		return pc, res

	def createOne(self, val, tag):
		if val < 0x09:
			return [chr((val << 4) | tag)]
		else:
			return [chr(tag|0x08), chr(val)]

	@jit.unroll_safe
	def preprocess(self, beam):
		(instrs, const_table) = self.buildInstrs(beam)
		#for instr in instrs:
			#print instr.__class__, opcodes.opnames[instr.opcode]
		labelTable = []
		i = 0
		while i < len(instrs):
			instr_obj = instrs[i]
			#print "preprocess: " + opcodes.opnames[instr_obj.opcode]
			if instr_obj.opcode in [opcodes.CALL_EXT, 
					opcodes.CALL_EXT_ONLY, 
					opcodes.CALL_EXT_LAST]:
				args = instr_obj.args
				real_arity = args[0][1]
				header_index = args[1][1]
				entry = self._import_header[header_index]
				if ModuleDict.is_bif_from_tuple(self.get_name_entry(entry)):
					args[1] = (opcodes.TAG_LABEL, 
							self.import_header[header_index][1])
					instr_obj.args = args
			elif instr_obj.opcode == opcodes.LINE:
				(num,) = instr_obj.arg_values()
				if num > self.total_lines:
					self.total_lines = num
			elif instr_obj.opcode == opcodes.LABEL:
				labelTable.append(i)
			# put_tuple arity dst; put e1; put e2; ... => put_tuple dst [e1, e2, ...]
			elif instr_obj.opcode == opcodes.PUT_TUPLE:
				tuple_args = []
				size = instr_obj.args[0][1]
				for j in range(size):
					idx = i+1
					put_instr = instrs[idx]
					assert put_instr.opcode == opcodes.PUT
					tmp_arg = put_instr.args[0]
					tuple_args.append(tmp_arg)
					del instrs[idx]
				instrs[i] = Instruction(opcodes.PUT_TUPLE, [instr_obj.args[1]] + tuple_args[:])
			i += 1
		return labelTable[:], instrs[:], const_table[:]

	# try to find the function definition of code at pc
	@jit.unroll_safe
	def find_func_def(self, pc):
		while pc != 0:
			instr = self.instrs[0]
			if instr.opcode == opcodes.FUNC_INFO:
				(module_index, func_name_index, arity) = instr.arg_values()
				return (global_atom_table.get_str_at(module_index),
						global_atom_table.get_str_at(func_name_index),
						arity)
			else:
				pc -= 1
		raise Exception("cannot find function definition from address %d"%(pc))

	@jit.unroll_safe
	def find_label_from_address(self, addr):
		for label in range(len(self.labelTable)):
			if addr < self.labelTable[label]:
				return label
		return len(self.labelTable)

	@jit.unroll_safe
	def find_current_line(self, pc):
		while pc > 0:
			instr = self.instrs[pc]
			if instr.opcode == opcodes.LINE:
				(num,) = instr.arg_values()
				return num
			else:
				pc -= 0
		raise Exception("cannot find line number from address %d"%(pc))

		#while(pc < len(self.code)):
			#pc, instr = self.parseInstr(pc)
			#if instr == opcodes.LINE:
				#pc, line_number = self.parseInt(pc)
				#if line_number == 0: # it means we are in the last line
					#return self.total_lines
				#else:
					#return line_number - 1
			#else:
				#pc = self.discard_operands(instr, pc)
		## the last part of module file should always be built-in module info
		## function, so it's reasonable to return a line 0 for them
		#return 0  

	#@jit.unroll_safe
	#def discard_operands(self, instr, pc):
		#arity = opcodes.arity[instr]
		#for i in range(0, arity):
			#pc, first = self.parseOne(pc)
			#pc, tag = self._parseTag(pc, first)
			##print first
			##print tag
			#if self.isBaseTag(tag):
				#pc, _ = self._parseInt(pc, first)
			#elif tag == opcodes.TAGX_FLOATREG:
				#pc, _ = self._parse_floatreg(pc)
			#elif tag == opcodes.TAGX_SELECTLIST:
				#pc, _ = self._parse_selectlist(pc)
			#elif tag == opcodes.TAGX_FLOATREG:
				#pc, _ = self._parse_floatreg(pc)
			#elif tag == opcodes.TAGX_ALLOCLIST:
				#pc, _ = self._parse_alloclist(pc)
			#elif tag == opcodes.TAGX_LITERAL:
				#pc, _ = self._parse_literal(pc)
			#else:
				#pretty_print.print_hex(self.code)
				#raise Exception("Unknown TAG: %d at position:%d"%(tag, pc-1))
		#return pc

	@jit.unroll_safe
	def buildInstrs(self, beam):
		pc = 0
		instrs = []
		const_table = []
		atoms = beam.getAtomTable()
		#current_label = 0
		while (pc < len(self.code)):
			pc, instr = self.parseInstr(pc)
			arity = opcodes.arity[instr]
			args = []
			lst_field = None
			for i in range(0,arity):
				pc, first = self.parseOne(pc)
				pc, tag = self._parseTag(pc, first)
				if self.isBaseTag(tag):
					pc, val = self._parseInt(pc, first)
					if tag == opcodes.TAG_INTEGER:
						val = self._check_const_table(const_table, val)
					elif tag == opcodes.TAG_ATOM and not val == 0:
						val = global_atom_table.search_index(atoms[val-1])
					args.append((tag, val))
				elif tag == opcodes.TAGX_FLOATLIT:
					pc, val = self._parse_floatreg(pc)
					args.append((tag, val))
				elif tag == opcodes.TAGX_SELECTLIST:
					pc, lst_field = self._parse_selectlist(pc)
					for i in range(len(lst_field)):
						((tag, val), label) = lst_field[i]
						if tag == opcodes.TAG_INTEGER:
							val = self._check_const_table(const_table, val)
						elif tag == opcodes.TAG_ATOM and not val == 0:
							val = global_atom_table.search_index(atoms[val-1])
						lst_field[i] = ((tag, val), label)
				elif tag == opcodes.TAGX_FLOATREG:
					pc, val = self._parse_floatreg(pc)
					args.append((tag, val))
				elif tag == opcodes.TAGX_ALLOCLIST:
					pc, lst_field = self._parse_alloclist(pc)
				elif tag == opcodes.TAGX_LITERAL:
					pc, val  = self._parse_literal(pc)
					args.append((tag, val))
				else:
					pretty_print.print_hex(self.code)
					raise Exception("Unknown TAG: %d at position:%d"%(tag, pc-1))
			if instr in opcodes.loop_instrs:
				instrs.append(LoopInstruction(instr, args[:]))
			elif lst_field:
			#if lst_field:
				instrs.append(ListInstruction(instr, args[:], lst_field))
				#if instr in opcodes.possible_pattern_matches and (not self.check_error_label(current_label, args)):
					#instrs.append(PatternMatchingListInstruction(instr, args[:], lst_field))
				#else:
					#instrs.append(ListInstruction(instr, args[:], lst_field))
			else:
				instrs.append(Instruction(instr, args[:]))
				#if instr in opcodes.possible_pattern_matches and (not self.check_error_label(current_label, args)):
					#instrs.append(PatternMatchingInstruction(instr, args[:]))
				#else:
					#if instr == opcodes.LABEL:
						#current_label = args[0][1]
					#instrs.append(Instruction(instr, args[:]))
		return self.mark_pattern_instrs(instrs), [W_IntObject(v) for v in const_table]

	def check_error_label(self, next_label, args):
		for arg in args:
			(tag, val) = arg
			if tag == opcodes.TAG_LABEL and val < next_label:
				return True
		return False

	def mark_pattern_instrs(self, instrs):
		need_mark = False
		import sys
		next_label = sys.maxint
		for i in range(len(instrs)-1, -1, -1):
			instr_obj = instrs[i]
			# always mark conditional instructions before a RETURN instruction
			if instr_obj.opcode == opcodes.K_RETURN:
				need_mark = True
			elif instr_obj.opcode == opcodes.LABEL:
				next_label = instr_obj.arg_values()[0]
				#need_mark = False
                        #elif instr_obj.opcode in opcodes.loop_instrs:
                                #need_mark = False
			elif need_mark:
				if instr_obj.opcode in opcodes.possible_pattern_matches:
					if not self.check_error_label(next_label, instr_obj.args):
						need_mark = False
						if isinstance(instr_obj, ListInstruction):
							instrs[i] = PatternMatchingListInstruction(instr_obj.opcode,
									instr_obj.args[:], instr_obj.lst[:])
						else:
							instrs[i] = PatternMatchingInstruction(instr_obj.opcode,
									instr_obj.args[:])
		return instrs[:]


	def _check_const_table(self, const_table, val):
		in_const_table = False
		for j in range(len(const_table)):
			if val == const_table[j]:
				val = j
				in_const_table = True
				break
		if not in_const_table:
			const_table.append(val)
			val = len(const_table) - 1
		return val
