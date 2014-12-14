from root import W_Root

class W_IntObject(W_Root):
    def __init__(self, intval):
        assert(isinstance(intval, int))
        self.intval = intval

    def add(self, other):
        if not isinstance(other, W_IntObject):
            raise Exception("wrong type")
        return W_IntObject(self.intval + other.intval)

    def lt(self, other): 
        if not isinstance(other, W_IntObject):
            raise Exception("wrong type")
        return W_IntObject(self.intval < other.intval)

    def is_true(self):
        return self.intval != 0

    def str(self):
        return str(self.intval)


class W_FloatObject(W_Root):
    def __init__(self, floatval):
        assert(isinstance(floatval, float))
        self.floatval = floatval

    def add(self, other):
        if not isinstance(other, W_FloatObject):
            raise Exception("wrong type")
        return W_FloatObject(self.floatval + other.floatval)

    def lt(self, other): 
        if not isinstance(other, W_FloatObject):
            raise Exception("wrong type")
        return W_IntObject(self.floatval < other.floatval)

    def str(self):
        return str(self.floatval)


