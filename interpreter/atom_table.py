from pyrlang.interpreter.datatypes.atom import W_AtomObject

class Atom_Table:
	def __init__(self):
		self._str_table = ['nil']
		self._obj_table = [W_AtomObject(0)]
		self.TRUE_ATOM = self.get_obj_at(self.register_str('true'))
		self.FALSE_ATOM = self.get_obj_at(self.register_str('false'))

	def search_index(self, s):
		for i in range(len(self._str_table)):
			if self.get_str_at(i) == s:
				return i
		return -1

	def get_obj_at(self, idx):
		return self._obj_table[idx]

	def get_str_at(self, idx):
		return self._str_table[idx]

	def register_str(self, s):
		idx = self.search_index(s)
		if idx == -1:
			new_idx = len(self._str_table)
			self._str_table.append(s)
			self._obj_table.append(W_AtomObject(new_idx))
			return new_idx
		else:
			return idx

	def register_atoms(self, atoms):
		for s in atoms:
			self.register_str(s)

	def get_obj_from_str(self, s):
		idx = self.register_str(s)
		return self._obj_table[idx]

global_atom_table = Atom_Table()
