from rpython.rlib.rstruct.runpack import runpack
from rpython.rlib import jit
from pyrlang.interpreter.mod_file_loader import ModFileLoader
from pyrlang.lib import ModuleDict
import opcodes
import pretty_print

lib_module = ["erlang"]

class CodeParser:
	_immutable_fields_ = ['atoms[*]', 'labelTable']

	def __init__(self, beam, imported_funcs):
		self.position_key = 0
		self.str = beam.getCode()
		self.current_line = -1
		self.labelTable = []
		self.atoms = beam.getAtomTable()
		self.import_header = list(beam.impTChunk.asArray())
		self.func_list = []
		self.import_mods = [] # list of CodeParser to represent import module
		self.mod_dict = {} # mod atom index => import_mods' index
		self.imported_funcs_dict = imported_funcs # func_name => arity => imported_func_list's index
		self.imported_funcs_list = []
		self.import_BIF_and_module(self.import_header)
		self.preprocess(0)
		#pretty_print.print_labelTable(self.labelTable)

	def import_BIF_and_module(self, impTs):
		mfl = ModFileLoader()
		custom_mod_entries = {}
		for i in range(0, len(impTs)):
			entry = impTs[i];
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
				if module_atom_index in custom_mod_entries:
					if funcName in custom_mod_entries[module_atom_index]:
						custom_mod_entries[module_atom_index][funcName][arity] = 0
					else:
						custom_mod_entries[module_atom_index][funcName] = {arity:0}
				else:
					custom_mod_entries[module_atom_index] = {funcName:{arity:-1}}
		for (mod,funcs) in custom_mod_entries.items():
			# search module file (*.beam) 
			b = mfl.find(self.atoms[mod])
			self.mod_dict[mod] = len(self.import_mods)
			self.import_mods.append(CodeParser(b, funcs))

	def label_to_addr(self, label):
		print "jump to label: %d"%(label)
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
		pc, intval = self._parseInt(pc, first)
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
		return pc, self.parseInt(pc)

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
	# We should 
	# 1. create label table, (label_index => label_address)
	# 2. rewriting call_ext access (replace the module_atom_index with our import_mods list index,
	#    replace the func_atom_index and arity with imported_funcs index for every importing module,     
	# 3. create a imported_funcs table for every imported funcs. (index => func_address)
	#
	# @param pc
	#
	# @return 
	@jit.unroll_safe
	def preprocess(self, pc):
		#print "entry_func:%s"%(self.entry_func)
		#print("atoms:")
		#print self.atoms
		#print "entry_arity:%d"%(self.entry_arity)

		# we only want to go one pass to replace extern module
		# and function index, but we cannot modify the 'str' 
		# field since it's a RPython string, so we just firstly
		# store all the indices like:
		# replace address => (module index, function index)
		# as lastly replace where it occurs in 'str' by converting
		# into list, and then converting back to RPython string.
		#replace_table = {}  

		#print "self module: %s" % (self.atoms[0])
		#print self.imported_funcs_dict
		#print "########"
		while(True):
			pc, instr = self.parseInstr(pc)
			if instr == opcodes.LABEL:
				pc, _ = self.parseInt(pc)
				self.labelTable.append(pc)
			elif instr == opcodes.FUNC_INFO:
				pc, tag1 = self.parseOne(pc)
				pc, module = self._parseInt(pc, tag1)
				pc, tag2 = self.parseOne(pc)
				pc, func_index = self._parseInt(pc, tag2)
				pc, tag3 = self.parseOne(pc)
				pc, arity = self._parseInt(pc, tag3) 

				func_name = self.atoms[func_index-1]
				## build imported_funcs_dict table
				if module == 1: 
					# module == 1 means a function belong to itself
					if func_name in self.imported_funcs_dict:
						if arity in self.imported_funcs_dict[func_name]:
							self.imported_funcs_dict[func_name][arity] = len(self.imported_funcs_list) 
							self.imported_funcs_list.append(pc + 2)
			elif instr in [opcodes.CALL_EXT, opcodes.CALL_EXT_ONLY]:
				# we need to replace it with list index to achieve
				# random access.
				pc, read_arity = self.parseInt(pc)
				pc, import_index = self.parseInt(pc)
				import_entry = self.import_header[import_index]
				module_atom_index = import_entry[0]
				#print "module_atom_index:%d" % (module_atom_index)
				#print "name:%s" % (self.atoms[module_atom_index-1])
				#print self.atoms
				if self.atoms[module_atom_index-1] not in lib_module:
					module_index = self.mod_dict[module_atom_index-1]
					func_atom_index = import_entry[1]
					require_arity = import_entry[2]
					func_name = self.atoms[func_atom_index-1]
					#print self.import_mods[module_index].imported_funcs_dict
					func_index = self.import_mods[module_index].imported_funcs_dict[func_name][require_arity]

					#print "import_index:" + import_index
					#print "mod_dict:" + self.mod_dict
					#print "module_index:" + module_index
					#print "func_atom_index:" + func_atom_index

					self.import_header[import_index] = (module_index, func_index, require_arity)
					#replace_table[pc - 3] = (module_index, func_index)

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
		#code_list = list(self.str)
		#for (addr, replace) in replace_table.items():
			#code_list[addr] = self.build_atom(replace[0])
			#code_list[addr+1] = self.build_atom(replace[1])
		#self.str = ''.join(code_list)
