import sys
from lib.container import Container, ListContainer, LazyContainer

def testContainer():
    c = Container(x=5)
    c.y = 8
    c.z = 9
    c.w = 10
    c.foo = 5
    
    print (c)

def main(argv):
	testContainer()

def target(driver, args):
	main, None

if __name__ == '__main__':
	main(sys.argv)
