from rpython.rlib.rstruct.runpack import runpack
import opcodes
import pretty_print

class CodeParser:
	def __init__(self, s):
		self.offset = 0
		self.str = s
		self.labelTable = []

	def _ord(self, s):
		return ord(s[0])

	def _next(self, n=1):
		tmp = []
		for i in range(0, n):
			tmp.append(self.str[i + self.offset])
		self.offset += n
		return ''.join(tmp)

	def parseInstr(self):
		return self._ord(self._next())

	def parseTag(self):
		self.offset += 1
		return self._parseTag()

	def _parseTag(self):
		tag = ord(self.str[self.offset])
		if tag & 0x07 == opcodes.TAG_EXTENDED:
			return tag >> 4 + opcodes.TAGX_BASE
		else:
			return tag & 0x07

	def isBaseTag(self, tag):
		return type >= opcodes.TAGX_BASE

	def parseInt(self):
		tag = self.parseTag()
		return self._parseInt(tag)

	def _parseInt(self,tag):
		assert(self.isBaseTag(tag))
		return self._createInt(tag)

	def _createInt(self, tag):
		if tag & 0x08:
			if tag & 0x10:
				if tag & 0xe0 == 0xe0:
					length = self._createInt(tag) + (tag >> 5) + 2
					return self._decode_bignit(self._next(length))
				else:
					return self._decode_bignit(self._next(2+(tag >> 5)))
			else:
				w = self._ord(self._next())
				return ((tag & 0xe0)<<3)|w
		else:
			return tag >> 4

	def _decode_bignit(self, s):
		v = int(s, 16)
		if ord(s[0]) > 0x80:
			return v-(1 << (len(s))*8)
		else:
			return v

	def parse_float(self):
		assert(self.parseTag() == opcodes.TAGX_FLOATLIT)
		return self._parse_float()

	def _parse_float(self):
		return runpack("f", self._next(4))

	def parse_floatreg(self):
		assert(self.parseTag() == opcodes.TAGX_FLOATREG)
		return self._parse_float()

	def _parse_floatreg(self):
		return self.parseInt()

	def parse_literal(self):
		assert(self.parseTag() == opcodes.TAGX_LITERAL)
		return self._parse_literal()

	def _parse_literal(self):
		return self.parseInt()

	def parse_alloclist(self):
		assert(self.parseTag() == opcodes.TAGX_ALLOCLIST)
		return self._parse_alloclist()
	
	def _parse_alloclist(self):
		length = self.parseInt()
		res = []
		for i in range(0, length):
			res.append((self.parseInt(), self.parseInt()))
		return res

	def parse_selectlist(self):
		assert(self.parseTag() == opcodes.TAGX_SELECTLIST)
		return self.parse_selectlist()

	def _parse_selectlist(self):
		length = self.parseInt()
		res = []
		# not support!
		raise Exception("Selectlist is not supported!")
		return res

	def createLabelTable(self):
		while(True):
			instr = self.parseInstr()
			print "INSTR: %d"%(instr)
			if instr == opcodes.LABEL:
				self.parseInt()
				self.labelTable.append(self.offset)
			else:
				arity = opcodes.arity[instr]
				for i in range(0, arity):
					tag = self.parseTag()
					if self.isBaseTag(tag):
						self._parseInt(tag)
					elif tag == opcodes.TAGX_FLOATREG:
						self._parse_floatreg()
					elif tag == opcodes.TAGX_SELECTLIST:
						self._parse_selectlist()
					elif tag == opcodes.TAGX_FLOATREG:
						self._parse_floatreg()
					elif tag == opcodes.TAGX_ALLOCLIST:
						self._parse_alloclist()
					elif tag == opcodes.TAGX_LITERAL:
						self._parse_literal()
					else:
						pretty_print.print_hex(self.str)
						raise Exception("Unknown TAG: %d at position:%d"%(tag, self.offset-1))
			if(self.offset >= len(self.str)):
					break
		return self.labelTable
