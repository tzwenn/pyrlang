import sys
sys.path.append('/Users/sven/Desktop/rpython/pypy/')
sys.path.append('../')

from pyrlang.utils.app import App

def main(argv):
	app = App(argv)
	app.launch()
	return 0


def target(driver, args):
	return main, None

if __name__ == '__main__':
	main(sys.argv)
