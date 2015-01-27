class ContinuationStack:
	def __init__(self):
		self.vals = []

	def top(self):
		return self.vals[-1]

	def pop(self):
		return self.vals.pop()

	def push(self, v):
		self.vals.append(v)

	def is_empty(self):
		return len(self.vals) == 0
