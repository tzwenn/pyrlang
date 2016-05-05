import sys
# used for my MacBook
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')
# used for lab's MacPro
sys.path.append('/home/huangruochen/src/python/pypy')
sys.path.append('../')
#print sys.path
from rpython.jit.codewriter.policy import JitPolicy
from pyrlang.utils.app import App
from pyrlang.utils.observeriface import MyJitIface

def main(argv):
	app = App(argv)
	app.launch()
	return 0

#def jitpolicy(driver):
	#iface = MyJitIface()
	#return JitPolicy(iface)

def target(driver, args):
	return main, None

if __name__ == '__main__':
	main(sys.argv)
