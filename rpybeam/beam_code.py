from rpython.rlib.rstruct.runpack import runpack
from rpython.rlib import jit
import opcodes
import pretty_print

class CodeParser:
	_immutable_fields_ = ['str', 'atoms[*]', 'entry_arity', 'labelTable']
	def __init__(self, s, atomTable, entry_func = "start", entry_arity = 0):
		self.str = s
		self.labelTable = []
		self.entry_func = entry_func
		self.atoms = atomTable
		self.entry_addr = -1
		self.entry_arity = entry_arity
		self.labelTable = self.createLabelTable(0)
		#pretty_print.print_labelTable(self.labelTable)
		if self.entry_arity == -1:
			raise Exception("entry function %s/%d not found!"%(entry_func, entry_arity))

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

	def createLabelTable(self, pc):
		#print "entry_func:%s"%(self.entry_func)
		#print("atoms:")
		#print self.atoms
		#print "entry_arity:%d"%(self.entry_arity)
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
				if module == 1:
					if self.atoms[func_index-1] == self.entry_func:
						if arity == self.entry_arity:
							self.entry_addr = pc + 2 
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
		return self.labelTable
