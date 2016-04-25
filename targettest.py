import sys
# used for my MacBook
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')
# used for lab's MacPro
sys.path.append('/home/huangruochen/src/python/pypy')
sys.path.append('../')
#print sys.path
from pyrlang.utils.app import App
from rpython.jit.codewriter.policy import JitPolicy
from rpython.rlib.jit import JitHookInterface

class MyJitIface(JitHookInterface):
	##
	# @brief JIT abort callback function
	#
	# @param reason: int, defined in rlib.jit.Counter's scope, such as Counter.ABORT_TOO_LONG
	# @param jitdriver: rlib.jit.JitDriver
	# @param greenkey: list of jit.metainterp.history.Const
	# @param greenkey_repr: string
	# @param logops : jit.metainterp.logger.LogOperations
	# @param operations : list of jit.metainterp.resoperation.AbstractResOp
	def on_abort(self, reason, jitdriver, greenkey, greenkey_repr, logops, operations):
		pass

def main(argv):
	app = App(argv)
	app.launch()
	return 0

def jitpolicy(driver):
	iface = MyJitIface()
	return JitPolicy(iface)

def target(driver, args):
	return main, None

if __name__ == '__main__':
	main(sys.argv)
