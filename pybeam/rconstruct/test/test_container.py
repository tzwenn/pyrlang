from rconstruct.lib.container import *

def testContainer():
    c = Container(x=5)
    c.y = 8
    c.z = 9
    c.w = 10
    c.foo = 5
    
    print (c)
