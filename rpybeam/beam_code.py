from rpython.rlib.rstruct.runpack import runpack
from rpython.rlib import jit
from pyrlang.interpreter.mod_file_loader import ModFileLoader
from pyrlang.lib import ModuleDict
from pyrlang.rpybeam.beam_file import BeamRoot
import opcodes
import pretty_print

lib_module = ["erlang"]

class CodeParser:
	_immutable_fields_ = ['atoms[*]', 'labelTable']

	def __init__(self, beam):
		self.position_key = 0
		self.str = beam.getCode()
		self.current_line = -1
		self.labelTable = []
		self.atoms = beam.getAtomTable()
		self.import_header = list(beam.impTChunk.asArray())
		if beam.litTChunk:
			self.lit_table = beam.litTChunk.asArray()
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
		custom_mod_entries = []
		for i in range(0, len(self.import_header)):
			entry = self.import_header[i];
			module_atom_index = entry[0] - 1
			func_atom_index = entry[1] - 1
			arity = entry[2]
			moduleName = self.atoms[module_atom_index]
			funcName = self.atoms[func_atom_index]
			# BIF
			if moduleName in lib_module:
				# we use function_arity to emulate function overload
				moduleEntity = ModuleDict.module_dict[moduleName]()
				bif_name = "%s_%d"%(funcName, arity)
				#print "mod: %s: func_list: %d => %s"%(self.atoms[0], len(self.func_list), bif_name)
				self.import_header[i] = (entry[0], len(self.func_list), entry[2])
				self.func_list.append(moduleEntity.searchFunc(bif_name)())
			else:
				if module_atom_index not in custom_mod_entries:
					b = mfl.find(moduleName)
					self.mod_dict[module_atom_index] = len(self.import_mods)
					self.import_mods.append(CodeParser(b))
				mod_cp = self.import_mods[self.mod_dict[module_atom_index]]
				func_index = self.search_exports(funcName, 
						arity, mod_cp.export_header, mod_cp.atoms)
				self.import_header[i] = (self.mod_dict[module_atom_index], func_index, arity)
					
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
		unit = []
		for i in range(0, len(s)):
			hex_num = ord(s[i])
			unit.append(str((0xf0 & hex_num) >> 4))
			unit.append(str(0x0f & hex_num))
		return ''.join(unit)

	@jit.unroll_safe
	def _next(self, pc, n=1):
		to_index = pc + n
		assert(pc >= 0)
		assert(to_index >= 0)
		return to_index, self.str[pc:to_index]

	def parseOne(self, pc):
		x = ord(self.str[pc])
		return pc+1, x

	def parseInstr(self, pc):
		return self.parseOne(pc)

	def parseTag(self, pc):
		pc, res = self._parseTag(pc)
		return pc+1, res

	def _parseTag(self, pc, tag = -1):
		if tag == -1:
			tag = ord(self.str[pc])
		if tag & 0x07 == opcodes.TAG_EXTENDED:
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
		if tag & 0x08:
			if tag & 0x10:
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
		v = int(self._encode_hex(s), 16)
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
		#pretty_print.print_hex(self.str[self.offset:-1])
		#exit()
		pc, tag = self.parseTag(pc)
		assert(tag == opcodes.TAGX_SELECTLIST)
		return self._parse_selectlist(pc)

	@jit.unroll_safe
	def _parse_selectlist(self, pc):
		pc, length = self.parseInt(pc)
		res = []
		for i in range(0, length >> 1):
			pc, tmp1 = self.parseInt(pc)
			pc, tmp2 = self.parseInt(pc)
			res.append((tmp1, tmp2))
		return pc, res

	##
	# @brief One pass pre-process for CodeParser,
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
		while(True):
			pc, instr = self.parseInstr(pc)
			if instr == opcodes.LABEL:
				pc, _ = self.parseInt(pc)
				self.labelTable.append(pc)

			else:
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
						pretty_print.print_hex(self.str)
						raise Exception("Unknown TAG: %d at position:%d"%(tag, pc-1))
			if(pc >= len(self.str)):
					break
