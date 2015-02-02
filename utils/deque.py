from link_list import LinkList
# we use this class just because RPython 
# not supporting colloctions.deque
class Deque:
	def __init__(self):
		self.list1 = []
		self.list2 = []

	def append(self, x):
		self.list2.append(x)

	def pop(self):
		if len(self.list1) == 0:
			self.list1 = self.list2
			self.list2 = []
			self.list1.reverse()
		return self.list1.pop()

	def element(self, n):
		len1 = len(self.list1)
		if len1 == 0:
			return self.list2[n]
		else:
			if n > len1 - 1:
				return self.list2[n - len1]
			else:
				return self.list1[-(n+1)]

	def length(self):
		return len(self.list1) + len(self.list2)

	def empty(self):
		return len(self.list1) == 0 and len(self.list2) == 0

class MessageDeque(LinkList):
	def __init__(self, init_list = []):
		LinkList.__init__(self, init_list)
		self.pre_node = None
		self.current_position = 0

	def get_current(self):
		if self.pre_node:
			if self.pre_node.get_next():
				return self.pre_node.get_next().get_val()
			else:
				self.reset_to_head()
				return None
		else:
			if self.node:
				return self.node.get_val()
			else:
				return None

	def next(self):
		self.current_position += 1
		if self.pre_node:
			self.pre_node = self.pre_node.get_next()
		else:
			self.pre_node = self.node

	def remove_current(self):
		self.delete(self.current_position)

	def reset_to_head(self):
		self.current_position = 0
		self.pre_node = None
