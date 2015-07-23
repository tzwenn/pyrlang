from rpython.rlib import jit
from pyrlang.interpreter.datatypes.number import W_FloatObject
from pyrlang.rpybeam import pretty_print

class AbstractRegister:
	def get(self, n):
		pass

	def store(self, n, val):
		pass

max_x_reg_size = 128 

class X_Register(AbstractRegister):
	_virtualizable_ = ['regs[*]', 'floats[*]']

	def __init__(self):
		self = jit.hint(self, fresh_virtualizable=True, access_directly=True)
		self.regs = [None] * max_x_reg_size
		self.floats = [None] * max_x_reg_size

	def get(self, n):
		assert(n >= 0)
		assert(n < max_x_reg_size)
		return self.regs[n]

	def store(self, n, val):
		assert(n >= 0)
		assert(n < max_x_reg_size)
		self.regs[n] = val

	def get_float(self, n):
		assert(n >= 0)
		assert(n < max_x_reg_size)
		return self.floats[n]

	def store_float(self, n, val):
		assert(n >= 0)
		assert(n < max_x_reg_size)
		assert isinstance(val, W_FloatObject)
		self.floats[n] = val

	def print_content(self):
		print "Normal:"
		for i,v in enumerate(self.regs):
			if v:
				print i, pretty_print.value_str(v)
		print "Floats:"
		for i, f in enumerate(self.floats):
			if f:
				print i, pretty_print.value_str(f)

#class Y_Register(AbstractRegister):
	##_virtualizable_ = ['regs[*]']

	#def __init__(self):
		##self = jit.hint(self, fresh_virtualizable=True, access_directly=True)
		#self.regs = []

	#def get(self, n):
		#return self.regs[-(n+1)]

	#def store(self, n, val):
		#self.regs[-(n+1)] = val

	#def pop(self):
		#return self.regs.pop()

	#def push(self, value):
		#self.regs.append(value)

	#def is_empty(self):
		#return len(self.regs) == 0

	#def depth(self):
		#return len(self.regs)

#class Abstract_Node:
	#_immutable_fields_ = ['val']
	#pass

#class Stack_Node(Abstract_Node):
	#def __init__(self, val, next):
		#self.val = val
		#self.next = next

#class Empty_Node(Abstract_Node):
	#pass

#class Y_Register(AbstractRegister):
	#def __init__(self):
		#self.current_node = Empty_Node()

	#def pop(self):
		#res = self.current_node
		#self.current_node = res.next
		#return res.val

	#def push(self, val):
		#self.current_node = Stack_Node(val, self.current_node)

	#def is_empty(self):
		#return isinstance(self.current_node, Empty_Node)

	#@jit.unroll_safe
	#def get(self, n):
		#node = self.current_node
		#for i in range(n+1):
			#assert isinstance(node, Stack_Node)
			#if i == n:
				#return node.val
			#node = node.next
		#raise IndexError

	#@jit.unroll_safe
	#def store(self, n, val):
		#if n == 0:
			#next = self.current_node.next
			#self.current_node = Stack_Node(val, next)
			#return
		#node = self.current_node
		#for i in range(n+1):
			#assert isinstance(node, Stack_Node)
			#if i == n - 1:
				#pre_node = node
				#current_node = node.next
				#assert isinstance(current_node, Stack_Node)
				#next_node = current_node.next
				#pre_node.next = Stack_Node(val, next_node)
				#return
			#node = node.next
		#raise IndexError

	#def depth(self):
		#i = 0
		#node = self.current_node
		#while isinstance(node, Stack_Node):
			#i += 1
			#node = node.next
		#return i

init_y_reg_size = 8

class Y_Register(AbstractRegister):
	def __init__(self):
		self.vals = [None] * init_y_reg_size
		self.addrs = [(None, 0)] * init_y_reg_size
		self.val_top = -1
		self.val_size = jit.hint(init_y_reg_size, promote=True)

		self.addr_top = -1
		self.addr_size = jit.hint(init_y_reg_size, promote=True)

	@jit.unroll_safe
	def allocate(self, n, init_val = None):
		self.val_top += n
		while self.val_top >= self.val_size:
			self.vals = self.vals + [None] * self.val_size
			self.val_size = self.val_size << 1
		if init_val:
			index = self.val_top
			for i in range(n):
				self.vals[index] = init_val
				index -= 1

	def deallocate(self, n):
		self.val_top -= n

	def push(self, val): # (cp, pc)
		self.addr_top += 1
		if self.addr_top >= self.addr_size:
			self.addrs = self.addrs + [(None, 0)] * self.addr_size
			self.addr_size = self.addr_size << 1
		self.addrs[self.addr_top] = val

	def pop(self):
		val = self.addrs[self.addr_top]
		self.addr_top -= 1
		return val

	def get(self,n):
		idx = self.val_top - n
		assert idx >= 0
		return self.vals[idx]

	def is_empty(self):
		return self.addr_top == -1

	def depth(self):
		return self.addr_top + 1

	def store(self, n, val):
		self.vals[self.val_top - n] = val

	def print_content(self):
		print "Y register"
		for (i,v) in enumerate(self.vals):
			if v:
				print i, pretty_print.value_str(v)
