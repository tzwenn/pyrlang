from rpython.rlib.rstruct.runpack import runpack
from rpython.rlib import jit
from pyrlang.interpreter.mod_file_loader import ModFileLoader
from pyrlang.interpreter.atom_table import global_atom_table
from pyrlang.interpreter.datatypes.number import W_IntObject
from pyrlang.lib import ModuleDict
from pyrlang.rpybeam.beam_file import BeamRoot
from pyrlang.rpybeam.instruction import Instruction, ListInstruction
import opcodes
import pretty_print
import time

class CodeParser:
	_immutable_fields_ = ['file_name', 'instrs[*]', 'import_header[*]', 
			'parent_cp', 'total_lines', 'labelTable[*]', 
			'_import_header[*]', 'lit_table[*]', 'loc_table[*]', 'mod_dict[*]',
			'fun_table[*]','import_mods[*]','func_list[*]', 'export_header[*]',
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

		(self.func_list, self.import_header, self.import_mods, self.mod_dict) = self.import_BIF_and_module()
		(self.labelTable, self.instrs, self.const_table) = self.preprocess(beam)
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
		return func_list[:],import_header[:], import_mods[:], mod_dict.copy()

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
		return -1

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
		assert(tag < opcodes.TAGX_BASE)
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
		labelTable = []
		for i in range(len(instrs)):
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
		return labelTable[:], instrs[:], const_table[:]

		# first pass
		#while(pc < len(self.code)):
			#pc, instr = self.parseInstr(pc)
			#if instr in [opcodes.CALL_EXT, opcodes.CALL_EXT_ONLY, opcodes.CALL_EXT_LAST]:
				#pc, real_arity = self.parseInt(pc)
				#pc_begin = pc
				#pc, header_index = self.parseInt(pc)
				#pc_end = pc
				#entry = self._import_header[header_index]
				#if ModuleDict.is_bif_from_tuple(self.get_name_entry(entry)):
					## at this time, the func index are already replaced
					#replace_list.append((pc_begin, pc_end - pc_begin,
							#self.import_header[header_index][1]))
			#elif instr == opcodes.LINE:
				#pc, num = self.parseInt(pc)
				#if num > self.total_lines:
					#self.total_lines = num
			#else:
				#pc = self.discard_operands(instr, pc)
		#code_list = list(self.code)

		#global_offset = 0
		#for (addr, word_len, index) in replace_list:
			## [Notice] this is not a real label, it's just a cheat
			## that tell interpreter it's actually a bif rather 
			## than a module function
			#e_lst = self.createOne(index, opcodes.TAG_LABEL)
			##print "replace %d(size:%d) with %s" % (addr, word_len, str(e_lst))
			#if len(e_lst) == word_len:
				#for i in range(len(e_lst)):
					#code_list[addr + i + global_offset] = e_lst[i]
			#elif len(e_lst) > word_len:
				#assert word_len == 1 and len(e_lst) == 2
				#code_list[addr + global_offset] = e_lst[0]
				#insert_pos = addr + global_offset + 1
				#assert insert_pos >= 0
				#code_list.insert(insert_pos, e_lst[1])
				#global_offset += 1
			#else:
				##print code_list[addr - 10:]
				#assert len(e_lst) == 1 and word_len == 2
				#code_list[addr + global_offset] = e_lst[0]
				#del code_list[addr + global_offset + 1]
				#global_offset -= 1
				##print code_list[addr - 10:]
		#self.code = ''.join(code_list)

		## second pass, to build the label table
		## note we cannot do it in first pass because
		## the replacement of fake bif may change
		## the layout of the code.
		#pc = 0
		#while(pc < len(self.code)):
			#pc, instr = self.parseInstr(pc)
			##print "   " + str(pc - 1) + "[" + opcodes.opnames[instr].upper() + "]"
			#if instr == opcodes.LABEL:
				#pc, num = self.parseInt(pc)
				#self.labelTable.append(pc)
				##print "L" + str(num) + ":" + str(pc)
			#else:
				##print "[%d]"%(pc) + opcodes.opnames[instr].upper()
				#pc = self.discard_operands(instr, pc)

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

		#str_len = len(self.code)
		#while(pc < str_len):
			#pc, instr = self.parseInstr(pc)
			#if instr == opcodes.LABEL:
				#pc, label_num = self.parseInt(pc)
				#return self.find_func_def_from_label(label_num - 3)
			#else:
				#pc = self.discard_operands(instr, pc)
		#return self.find_func_def_from_label(len(self.labelTable) - 3)

	# find some address within the range of label table,
	# begined with 1
	@jit.unroll_safe
	def find_label_from_address(self, addr):
		for label in range(len(self.labelTable)):
			if addr < self.labelTable[label]:
				return label
		return len(self.labelTable)

	# label index should begin with 0
	#@jit.unroll_safe
	#def find_func_def_from_label(self, label_index):
		#for l_idx in range(label_index, len(self.labelTable)):
			#pc = self.labelTable[l_idx]
			#pc, instr = self.parseInstr(pc)
			#if instr == opcodes.LINE:
				#pc = self.discard_operands(instr, pc)
				#pc, instr = self.parseInstr(pc)
				#if instr == opcodes.FUNC_INFO:
					#pc, module_index = self.parseInt(pc)
					#pc, func_name_index = self.parseInt(pc)
					#pc, arity = self.parseInt(pc)
					#return (self.atoms[module_index - 1], 
							#self.atoms[func_name_index - 1],
							#arity)
		#raise Exception("can not found function definition from label:%d"%(label_index + 1))

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
					pc, val = self._parseInt(pc, first)
					args.append((tag, val))
				elif tag == opcodes.TAGX_ALLOCLIST:
					pc, lst_field = self._parse_alloclist(pc)
				elif tag == opcodes.TAGX_LITERAL:
					pc, val  = self._parse_literal(pc)
					args.append((tag, val))
				else:
					pretty_print.print_hex(self.code)
					raise Exception("Unknown TAG: %d at position:%d"%(tag, pc-1))
			if lst_field:
				instrs.append(ListInstruction(instr, args[:], lst_field))
			else:
				instrs.append(Instruction(instr, args[:]))
		return instrs[:], [W_IntObject(v) for v in const_table]

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
