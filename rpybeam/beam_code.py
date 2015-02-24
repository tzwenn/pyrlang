from rpython.rlib.rstruct.runpack import runpack
from rpython.rlib import jit
from pyrlang.interpreter.mod_file_loader import ModFileLoader
from pyrlang.lib import ModuleDict
from pyrlang.rpybeam.beam_file import BeamRoot
import opcodes
import pretty_print

class CodeParser:
	_immutable_fields_ = ['file_name', 'parent_cp', 'code', 'total_lines', 'atoms[*]', 'labelTable', 'bif_map',
			'import_header', '_import_header', 'lit_table', 'loc_table', 'fun_table', 'func_list', 
			'import_mods', 'export_header', 'export_funcs_list']

	def __init__(self, beam, name, parent_cp = None):
		self.file_name = name
		self.parent_cp = parent_cp
		self.code = beam.getCode()
		self.current_line = -1
		self.total_lines = -1
		self.current_label = -1
		self.labelTable = []
		self.bif_map = {}
		self.atoms = beam.getAtomTable()
		# the import_header will be rewrite so we need another copy to refer the original version at something
		self._import_header = list(beam.impTChunk.asArray())
		self.import_header = list(beam.impTChunk.asArray()) 
		self.lit_table = None
		self.loc_table = None
		self.fun_table = None
		if beam.litTChunk:
			self.lit_table = beam.litTChunk.asArray()
		if beam.locTChunk:
			self.loc_table = beam.locTChunk.asArray()
		if beam.funTChunk:
			self.fun_table = beam.funTChunk.asArray()
			#print "loc_table:"
			#print self.loc_table
		self.func_list = []
		self.import_mods = [] # list of CodeParser to represent import module
		self.mod_dict = {} # mod atom index => import_mods' index

		self.export_header = beam.expTChunk.asArray()
		self.export_funcs_list = []
		self.import_BIF_and_module()
		self.preprocess(0)
		#pretty_print.print_labelTable(self.labelTable)

	def import_BIF_and_module(self):
		mfl = ModFileLoader()
		for i in range(0, len(self.import_header)):
			entry = self.import_header[i];
			module_atom_index = entry[0] - 1
			func_atom_index = entry[1] - 1
			arity = entry[2]
			moduleName = self.atoms[module_atom_index]
			funcName = self.atoms[func_atom_index]
			#print "prepare importing function: %s:%s/%d..." % (moduleName, funcName, arity)
			# BIF
			if moduleName in ModuleDict.module_dict and ModuleDict.is_bif_from_tuple(self.get_name_entry(entry)):
				# we use function_arity to emulate function overload
				moduleEntity = ModuleDict.module_dict[moduleName]()
				bif_name = ModuleDict.get_bif_name(funcName, arity)
				#print "mod: %s: func_list: %d => %s"%(self.atoms[0], len(self.func_list), bif_name)
				self.import_header[i] = (entry[0], len(self.func_list), entry[2])
				if moduleName in self.bif_map:
					self.bif_map[moduleName][bif_name] = len(self.func_list)
				else:
					self.bif_map[moduleName] = {bif_name: len(self.func_list)}
				self.func_list.append(moduleEntity.searchFunc(bif_name)())
			elif not moduleName == self.get_self_module_name():
				if module_atom_index not in self.mod_dict:
					#print "searching module [%s] by %s"%(moduleName, self.get_self_module_name())
					b = mfl.find(moduleName)
					self.mod_dict[module_atom_index] = len(self.import_mods)
					self.import_mods.append(CodeParser(b, moduleName + ".erl", self))
				mod_cp = self.import_mods[self.mod_dict[module_atom_index]]
				func_index = self.search_exports(funcName, 
						arity, mod_cp.export_header, mod_cp.atoms)
				self.import_header[i] = (self.mod_dict[module_atom_index], func_index, arity)

	def get_self_module_name(self):
		return self.atoms[0]

	def get_module_cp_by_name(self, module_name):
		if module_name == self.get_self_module_name():
			return self
		else:
			mod_index = self.mod_dict[self.atoms.index(module_name)]
			return self.import_mods[mod_index]

	def get_func_addr_by_name(self, func_name, arity):
		index = self.search_exports(func_name, arity, self.export_header, self.atoms)
		label = self.export_header[index][2]
		return self.label_to_addr(label)

	def get_name_entry(self, entry):
		return (self.atoms[entry[0] - 1],
				self.atoms[entry[1] - 1],
				entry[2])
					
	def search_exports(self, func_name, arity, header, atoms):
		for i in range(0, len(header)):
			entry = header[i]
			if atoms[entry[0] - 1] == func_name and arity == entry[1]:
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
		return pc, self.parseInt(pc)

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
			pc, tmp1 = self.parseInt(pc)
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

	##
	# @brief Two pass pre-process for CodeParser,
	# @param pc
	#
	# @return 
	@jit.unroll_safe
	def preprocess(self, pc):
		#print "entry_func:%s"%(self.entry_func)
		#print("atoms:")
		#print self.atoms
		#print "entry_arity:%d"%(self.entry_arity)

		#print "self module: %s" % (self.atoms[0])
		#print self.export_funcs_dict
		#print "########"
		replace_list = []
		# first pass
		while(pc < len(self.code)):
			pc, instr = self.parseInstr(pc)
			if instr in [opcodes.CALL_EXT, opcodes.CALL_EXT_ONLY, opcodes.CALL_EXT_LAST]:
				pc, real_arity = self.parseInt(pc)
				pc_begin = pc
				pc, header_index = self.parseInt(pc)
				pc_end = pc
				entry = self._import_header[header_index]
				if ModuleDict.is_bif_from_tuple(self.get_name_entry(entry)):
					# at this time, the func index are already replaced
					replace_list.append((pc_begin, pc_end - pc_begin,
							self.import_header[header_index][1]))
			elif instr == opcodes.LINE:
				pc, num = self.parseInt(pc)
				if num > self.total_lines:
					self.total_lines = num
			else:
				pc = self.discard_operands(instr, pc)
		code_list = list(self.code)

		global_offset = 0
		for (addr, word_len, index) in replace_list:
			# [Notice] this is not a real label, it's just a cheat
			# that tell interpreter it's actually a bif rather 
			# than a module function
			e_lst = self.createOne(index, opcodes.TAG_LABEL)
			#print "replace %d(size:%d) with %s" % (addr, word_len, str(e_lst))
			if len(e_lst) == word_len:
				for i in range(len(e_lst)):
					code_list[addr + i + global_offset] = e_lst[i]
			elif len(e_lst) > word_len:
				assert word_len == 1 and len(e_lst) == 2
				code_list[addr + global_offset] = e_lst[0]
				insert_pos = addr + global_offset + 1
				assert insert_pos >= 0
				code_list.insert(insert_pos, e_lst[1])
				global_offset += 1
			else:
				#print code_list[addr - 10:]
				assert len(e_lst) == 1 and word_len == 2
				code_list[addr + global_offset] = e_lst[0]
				del code_list[addr + global_offset + 1]
				global_offset -= 1
				#print code_list[addr - 10:]
		self.code = ''.join(code_list)

		# second pass, to build the label table
		# note we cannot do it in first pass because
		# the replacement of fake bif may change
		# the layout of the code.
		pc = 0
		while(pc < len(self.code)):
			pc, instr = self.parseInstr(pc)
			#print "   " + str(pc - 1) + "[" + opcodes.opnames[instr].upper() + "]"
			if instr == opcodes.LABEL:
				pc, num = self.parseInt(pc)
				self.labelTable.append(pc)
				#print "L" + str(num) + ":" + str(pc)
			else:
				#print "[%d]"%(pc) + opcodes.opnames[instr].upper()
				pc = self.discard_operands(instr, pc)

	@jit.unroll_safe
	def find_func_def(self, pc):
		str_len = len(self.code)
		while(pc < str_len):
			pc, instr = self.parseInstr(pc)
			if instr == opcodes.LABEL:
				pc, label_num = self.parseInt(pc)
				return self.find_func_def_from_label(label_num - 3)
			else:
				pc = self.discard_operands(instr, pc)
		return self.find_func_def_from_label(len(self.labelTable) - 3)

	# find some address within the range of label table,
	# begined with 1
	@jit.unroll_safe
	def find_label_from_address(self, addr):
		for label in range(len(self.labelTable)):
			if addr < self.labelTable[label]:
				return label
		return len(self.labelTable)

	def find_func_def_from_addr(self, addr):
		return self.find_func_def_from_label(self.find_label_from_address(addr) - 2)

	# label index should begin with 0
	@jit.unroll_safe
	def find_func_def_from_label(self, label_index):
		for l_idx in range(label_index, len(self.labelTable)):
			pc = self.labelTable[l_idx]
			pc, instr = self.parseInstr(pc)
			if instr == opcodes.LINE:
				pc = self.discard_operands(instr, pc)
				pc, instr = self.parseInstr(pc)
				if instr == opcodes.FUNC_INFO:
					pc, module_index = self.parseInt(pc)
					pc, func_name_index = self.parseInt(pc)
					pc, arity = self.parseInt(pc)
					return (self.atoms[module_index - 1], 
							self.atoms[func_name_index - 1],
							arity)
		raise Exception("can not found function definition from label:%d"%(label_index + 1))

	@jit.unroll_safe
	def find_current_line(self, pc):
		while(pc < len(self.code)):
			pc, instr = self.parseInstr(pc)
			if instr == opcodes.LINE:
				pc, line_number = self.parseInt(pc)
				if line_number == 0: # it means we are in the last line
					return self.total_lines
				else:
					return line_number - 1
			else:
				pc = self.discard_operands(instr, pc)
		# the last part of module file should always be built-in module info
		# function, so it's reasonable to return a line 0 for them
		return 0  

	@jit.unroll_safe
	def discard_operands(self, instr, pc):
		arity = opcodes.arity[instr]
		for i in range(0, arity):
			pc, first = self.parseOne(pc)
			pc, tag = self._parseTag(pc, first)
			#print first
			#print tag
			if self.isBaseTag(tag):
				pc, _ = self._parseInt(pc, first)
			elif tag == opcodes.TAGX_FLOATREG:
				pc, _ = self._parse_floatreg(pc)
			elif tag == opcodes.TAGX_SELECTLIST:
				pc, _ = self._parse_selectlist(pc)
			elif tag == opcodes.TAGX_FLOATREG:
				pc, _ = self._parse_floatreg(pc)
			elif tag == opcodes.TAGX_ALLOCLIST:
				pc, _ = self._parse_alloclist(pc)
			elif tag == opcodes.TAGX_LITERAL:
				pc, _ = self._parse_literal(pc)
			else:
				pretty_print.print_hex(self.code)
				raise Exception("Unknown TAG: %d at position:%d"%(tag, pc-1))
		return pc
