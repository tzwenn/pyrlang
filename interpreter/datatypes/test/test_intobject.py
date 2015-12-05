from pyrlang.interpreter.datatypes.number import W_IntObject

class TestW_IntObject:
	def test_str(self):
		x = 12345
		obj1 = W_IntObject(x)
		result = obj1.str()
		assert result == str(x)
