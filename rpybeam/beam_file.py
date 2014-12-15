from rpython.rlib.rStringIO import RStringIO
from rpython.rlib import rzlib
from rpython.rlib.rstruct.runpack import runpack
from pretty_print import *
#from beam_code import BeamInstr

class BaseNode:
	def __init__(self, stream):
		self.readlen = 0
		self.parse(stream)

	def parse(self, stream):
		pass

	def readString(self, stream, length = 4):
		self.readlen += length
		return self._readString(stream, length)
		
	def _readString(self, stream, length):
		res = []
		for i in range(0, length):
			res.append(runpack("s", stream.read(1)))
		return ''.join(res)

	def readInt4(self, stream):
		self.readlen += 4
		c1, c2, c3, c4 = runpack("4B", stream.read(4))
		return (c1 << 24) + (c2 << 16) + (c3 << 8) + c4

	def readUCInt(self, stream):
		self.readlen += 1
		return runpack("B", stream.read(1))

	def readAny(self, stream, length):
		self.readlen += length
		return stream.read(length)

class BeamRoot(BaseNode):
	def __init__(self, fstream):
		# ugly!!!
		stream = RStringIO()
		stream.write(fstream.read())
		stream.seek(0)
		BaseNode.__init__(self, stream)

	def parse(self, stream):
		assert(self.readString(stream) == 'FOR1')
		self.size = self.readInt4(stream)
		assert(self.readString(stream) == 'BEAM')
		self.dispatchChunk(stream)

	def getCode(self):
		return self.codeChunk.codes

	def getAtomTable(self):
		return self.atomChunk.asList()

	def dispatchChunk(self, stream):
		while(True):
			flag = self._readString(stream, 4)
			if flag == 'Atom':
				self.atomChunk = AtomChunk(stream)
			elif flag == 'Code':
				self.codeChunk = CodeChunk(stream)
			elif flag == 'StrT':
				self.strTChunk = StrTChunk(stream)
			elif flag == 'ImpT':
				self.impTChunk = ImpTChunk(stream)
			elif flag == 'ExpT':
				self.expTChunk = ExpTChunk(stream)
			elif flag == 'LocT':
				self.locTChunk = LocTChunk(stream)
			elif flag == 'LitT':
				self.litTChunk = LitTChunk(stream)
			else:
				break
		#else:
			#print flag
			#print len(flag)
			#print stream.read(10)

class Chunk(BaseNode):
	def __init__(self, stream):
		self.readlen = 4
		self.size = self.readInt4(stream)
		self.parse(stream)

	def discardRemain(self, stream):
		n = self.readlen % 4
		if n > 0:
			self.readAny(stream, 4 - n)

class AtomChunk(Chunk):
	def parse(self, stream):
		self.atomList = []
		self.count = self.readInt4(stream)
		for i in range(0, self.count):
			a = Atom(stream)
			self.atomList.append(a)
			self.readlen += a.readlen
		self.discardRemain(stream)

	def asList(self):
		res = []
		for a in self.atomList:
			res.append(a.name)
		return res

class Atom(BaseNode):
	def parse(self, stream):
		size = self.readUCInt(stream)
		self.name = self.readString(stream, size)

class CodeChunk(Chunk):
	def parse(self, stream):
		self.info_size = self.readInt4(stream)
		self.version = self.readInt4(stream)
		self.opcode_max = self.readInt4(stream)
		self.labels = self.readInt4(stream)
		self.function_count = self.readInt4(stream)

		self.readAny(stream, self.info_size - 16)
		self.codes = self.readAny(stream, self.size - self.info_size - 4)
		self.discardRemain(stream)

class StrTChunk(Chunk):
	def parse(self, stream):
		if self.size:
			self.strlen = self.readUCInt(stream)
			self.str = self.readString(stream, self.strlen)
			self.discardRemain(stream)
		else:
			self.strlen = 0
			self.str = ''

class ImpTChunk(Chunk):
	def parse(self, stream):
		self.count = self.readInt4(stream)
		self.entries = []
		for i in range(0, self.count):
			e = ImpTEntry(stream)
			self.readlen += e.readlen
			self.entries.append(e)

	def asArray(self):
		res = []
		for i in range(0, len(self.entries)):
			res.append(self.entries[i].asArray())
		return res

class ImpTEntry(BaseNode):
	def parse(self, stream):
		self.module = self.readInt4(stream)
		self.function = self.readInt4(stream)
		self.arity = self.readInt4(stream)

	def asArray(self):
		return (self.module, self.function, self.arity)

class ExpTChunk(Chunk):
	def parse(self, stream):
		self.count = self.readInt4(stream)
		self.entries = []
		for i in range(0, self.count):
			e = InnerEntry(stream)
			self.readlen += e.readlen
			self.entries.append(e)

class InnerEntry(BaseNode):
	def parse(self, stream):
		self.function = self.readInt4(stream)
		self.arity = self.readInt4(stream)
		self.label = self.readInt4(stream)

class LocTChunk(Chunk):
	def parse(self, stream):
		self.count = self.readInt4(stream)
		self.entries = []
		for i in range(0, self.count):
			e = InnerEntry(stream)
			self.readlen += e.readlen
			self.entries.append(e)

class LitTChunk(Chunk):
	def parse(self, stream):
		self.len_uncompressed = self.readInt4(stream)

		odata = self.readAny(stream, self.size - 4)
		
		# ugly
		data = self.decompress(odata)
		substream = RStringIO()
		substream.write(data)
		substream.seek(0)

		self.count = self.readInt4(substream)
		self.term_list = []
		for i in range(0, self.count):
			size = self.readInt4(substream)
			assert(self.readUCInt(substream) == 0x83)
			tag = self.readUCInt(substream)
			if tag == 70: # new_float
				self.term_list.append(NewFloatTerm(substream, size))
		self.discardRemain(stream)

	def decompress(self,s):
		stream = rzlib.inflateInit()
		bytes, finished, unused = rzlib.decompress(stream, s)
		return bytes

#class AttrChunk(Chunk):
	#def parse(self, stream):
		#self.

class Term(BaseNode):
	def __init__(self, stream, size):
		self.readlen = 0
		self.size = size
		#print self.readUCInt(stream)
		self.parse(stream)

	def readBFloat(self, stream):
		self.readlen += 8
		return runpack(">d", stream.read(8))

class NewFloatTerm(Term):
	def parse(self, stream):
		self.floatval = self.readBFloat(stream)
