# instantiate a list subclass?

class Foo(list):

    def __new__(cls, n):
        return super(Foo, cls).__new__(cls, [0] * n)
    def __init__(self, args):
        print 'init', args

print Foo(3)
