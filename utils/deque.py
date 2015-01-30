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

	def empty(self):
		return len(self.list1) == 0 and len(self.list2) == 0
