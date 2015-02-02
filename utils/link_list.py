class Node:
	def __init__(self, val, next):
		self.next = next
		self.val = val

	def get_next(self):
		return self.next

	def get_val(self):
		return self.val

	def set_next(self, node):
		if node:
			assert isinstance(node, Node)
		self.next = node

class LinkList:
	def __init__(self, init_list = []):
		self.node = None
		pre_node = None
		for e in init_list:
			node = Node(e, None)
			if pre_node:
				pre_node.set_next(node)
			else:
				self.node = node
			pre_node = node

	def _get(self, n):
		node = self.node
		for i in range(0, n + 1):
			if node:
				if i == n:
					return node
				else:
					node = node.get_next()
			else:
				raise Exception("index %d out of link list range"%(n))

	def get(self, n):
		return self._get(n).get_val()

	def _check_next(self, node, n):
		if node and node.get_next():
			pass
		else:
			raise Exception("index %d out of link list range"%(n))

	def delete(self, n):
		if n == 0:
			if self.node and self.node.get_next():
				self.node = self.node.get_next()
			else:
				self.node = None
		else:
			pre_node = self._get(n - 1)
			self._check_next(pre_node, n)
			pre_node.set_next(pre_node.get_next().get_next())

	def to_list(self):
		res = []
		node = self.node
		while node:
			res.append(node.get_val())
			node = node.get_next()
		return res

	def append(self, val):
		node = self.node
		if node:
			while(node.get_next()):
				node = node.get_next()
			node.set_next(Node(val, None))
		else:
			self.node = Node(val, None)
