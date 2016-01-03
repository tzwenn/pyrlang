import sys
# used for my MacBook
sys.path.append('/Users/kiwakachen/src/python/pypy-zh/pypy')
# used for lab's MacPro
sys.path.append('/Users/huangruochen/src/python/pypy-commit/pypy')
sys.path.append('../')
#print sys.path
from pyrlang.utils.app import App

def main(argv):
	app = App(argv)
	app.launch()
	return 0

def target(driver, args):
	return main, None

if __name__ == '__main__':
	main(sys.argv)
