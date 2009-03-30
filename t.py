#!/usr/bin/python

class push(object):

    class __metaclass__(type):
        def __repr__(self):
            return 'push'


class vector(object):

    class __metaclass__(type):
        def __repr__(self):
            return 'vector'


class dot(object):

    class __metaclass__(type):
        def __repr__(self):
            return 'dot'


class cons(object):

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __iter__(self):
        yield self.car
        cdr = self.cdr
        if cdr is not None:
            if not isinstance(cdr, cons):
                raise Exception()
            for x in cdr:
                yield x

    def __repr__(self):
        def inrep(p):
            s = sep = ''
            while p is not None:
                s += sep + repr(p.car)
                p = p.cdr
                if p is not None and not isinstance(p, cons):
                    s += ' . ' + repr(p)
                    break
                sep = ' '
            return s
        return '(%s)' % inrep(self)


input = '(a(bc))'
# input = '(a.b)'
output = [None, None, 'c', push, 'b', push, push, 'a', push, push]

out = None
for token in input:
    if token == '(':
        out = cons(list, out)
    elif token == 'V':
        out = cons(vector, out)
    elif token == ')':
        out = cons(None, out)
    elif token == '.':
        out = cons(dot, out)
    else:
        out = cons(push, out)
        out = cons(token, out)

# assert list(out) == output
print out

stack = None
for action in out:
#    print 'action=%r stack=%r' % (action, stack)
    if action == push:
        next = stack.cdr
        new = cons(stack.car, next.car)
        stack = cons(new, next.cdr)
    elif action == list:
        next = stack.cdr
        if isinstance(next, cons):
            new = cons(stack.car, next.car)
            stack = cons(new, next.cdr)
    elif action == dot:
        stack.car = stack.car.car
    else:
        stack = cons(action, stack)
# print 'stack=%r' % stack
assert stack.cdr is None
print 'stack.car=%r' % stack.car
# assert repr(stack.car) == "('a' ('b' 'c'))"
