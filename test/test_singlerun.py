import pytest
from pyrlang.interpreter.datatypes.number import *
from pyrlang.utils.app import App
from pyrlang.rpybeam import pretty_print 

@pytest.mark.parametrize("filename,entry,args,expected", [
	("test_beam/closure.beam", "test", [5], 6),
	("test_beam/closure2.beam", "test", [5,3], 2),
	("test_beam/fib.beam", "test", [5], 8),
	("test_beam/test_map.beam", "test", [], "[4]"),
	])

class Test_SingleRun:

	def _run_once(self, filename, func_name, arglst):
		app = App(["", "-s", filename, func_name] + [str(e) for e in arglst])
		return pretty_print.value_str(app.launch())

	def test_eval(self, filename, entry, args, expected):
		assert self._run_once(filename, entry, args) == str(expected)
