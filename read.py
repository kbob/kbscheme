#!/usr/bin/python


class EOF(object):
    pass


class Done(Exception):
    pass


class sym(str):
    def __repr__(self):
        return super(sym, self).__str__()


class cons(object):
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr
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


class Action(object):

    ACCEPT, SCAN, POP, DISCARD = actions = range(4)

    def __init__(self, func):
        self.func = func

    def __call__(self, *args):
        return self.func(*args)

    def __repr__(self):
        return self.func.func_name


@Action
def accept(value, frame):
    return Action.ACCEPT, value


@Action
def sequence(value, frame):
#    print 'sequence(value=%r, frame=%r)' % (value, frame)
    if frame[1]:
        f = frame[1]
        while f.cdr:
            f = f.cdr
        f.cdr = cons(value, None)
    else:
        frame[1] = cons(value, None)
    return Action.SCAN, frame[1]


@Action
def dot(value, frame):
    print 'dot(value=%r, frame=%r)' % (value, frame)
    return Action.SCAN, frame[1]


@Action
def quote(value, frame):
#    print 'quote(value=%r, frame=%r)' % (value, frame)
    return Action.POP, cons(sym('quote'), cons(value, None))


@Action
def comment(value, frame):
    return Action.DISCARD, None


def yylex(s):
    if s:
        return s.pop(0)
    return EOF


def trhi(token, value, stack):
    srep = ' '.join(repr(s) for s in stack)
    print 'high token=%r value=%r stack=%s' % (token, value, srep)


def trlow(action, value, stack):
    srep = ' '.join(repr(s) for s in stack)
    print 'low action=%r value=%r stack=%s' % (action, value, srep)


def gpush(f):
    global stack
    stack = [[lambda *g: f(*g), None]] + stack

# More grammar:
# other abbrevs
# [...]
# #(...) #vu8(...)
# dot
# distinguish EXACT_NUMBER from SIMPLE
# error handling
# keep pointers to both ends of list.

# Things pushed: sequence, quote, comment.
# Things popped: )
# Actions: ACCEPT DISCARD POP SCAN

# Roots:
#   stack
#   head of current level
#   tail of current level

def read(s):
    stack = [[accept, None]]
    value = None
    while True:
        token = yylex(s)
#        trhi(token, value, stack)
	assert token != EOF
        if token == '(':
            stack = [[sequence, None]] + stack
            value = None
            continue
        elif token == ')':
            assert stack
            stack = stack[1:]
        elif token == '.':
            stack = [[dot, None]] + stack
        elif token == "'":
            stack = [[quote, None]] + stack
            continue
        elif token == ';':
            stack = [[comment, None]] + stack
            continue
        elif token in 'abcd':
            value = sym(token)
        else:
            assert False, 'illegal token %r' % token
        while True:
            action, value = stack[0][0](value, stack[0])
#            trlow(action, value, stack)
            assert action in Action.actions
            if action == Action.ACCEPT:
                return value
            elif action == Action.POP:
                stack = stack[1:]
            elif action == Action.DISCARD:
                stack = stack[1:]
                value = stack[0][1]
                break
            elif action == Action.SCAN:
                break

class Context(object):

    def __init__(self, enders, acceptable, more_acceptable):
        self.enders = set(enders)
        self.acceptable = set(acceptable)
        self.more = set(more_acceptable)

    # def copy(self):
    #     return Context(self.enders, self.acceptable, self.more)


top            = Context(EOF, '([VBASN;', '')
list           = Context(')', '([VBASN;', '.')
alt_list       = Context(']', '([VBASN;', '.')
vector         = Context(')', '([VBASN;', '')
bytevec        = Context(')', 'N', '')

contexts = {
    '(': Context(')', '([VBASN;', '.'),
    '[': Context(']', '([VBASN;', '.'),
    'V': Context(')', '([VBASN;', ''),
    'B': Context(')', 'N', ''),
}


prefices = set('A;')


def read(s):
    root = None
    context_stack = [Context(EOF, '([VBASN;', '')]
    store = ['root']
    while True:
        token, yylval = yylex()
        current = context_stack[-1]
        if token in current.enders:
            value = current.make()
            context_stack.pop()
            store(value)
        elif token in current.acceptable:
            current.acceptable |= current.more
            if token in 'SN':
                store(yylval)
                while prefix_stack:
                    yylval = prefix_stack.pop()(yylval)
                    if yylval:
            elif token in contexts:
                context_stack.push(contexts[token])
            elif token in prefices:
                prefix_stack.push(yylval)
        else:
            raise SyntaxError('unexpected %s' % token)


class Read(object):

    def __init__(self, instream):
        self.s = instream

    def next(self):
        self.root = None
        self.stack = [self.accept]
        self.store = 'self.root'
        while True:
            token, yylval = yylex(s)
            if token == EOF:
                raise Syntax('unexpected EOF')
            if token in self.pushes:
                self.stack.push(self.pushes[token])
            elif token == ']':
                if not self.stack:
                    raise Syntax('extra ]')
                if self.stack[-1] != self.altlist:
                    raise Syntax('exprected ), got ]')
            elif token == ')':
                if not self.stack:
                    raise Syntax('extra )')
                if self.stack[-1] == self.altlist:
                    raise Syntax('expected ], got )')
            elif token == '.':
                if self.stack[-1] == self.altlist

    def list(self):
        

    pushes = {
        '(': self.list,
        '[': self.altlist,
        'V': self.vector,
        'B': self.bytevector,
        'A': self.abbrev,
        ';': self.comment,
    }



print repr(cons(sym('a'), sym('b')))

tvs = [
    ['a',          'a'],
    ['()',         'None'],
    ['(a)',        '(a)'],
    ['(ab)',       '(a b)'],
    ['(a.b)',	   '(a . b)'],
    ['(ab(cd))',   '(a b (c d))'],
    ["'a",         '(quote a)'],
    ["'(ab)",      '(quote (a b))'],
    ["(a'bc)",     '(a (quote b) c)'],
    [';ab',        'b'],
    ['(;a)',       'None'],
    ['(a;b)',      '(a)'],
    ['(a;bc)',     '(a c)'],
    [';(a(b))(c)', '(c)'],
    ]

for input, expected in tvs:
    actual = repr(read(list(input)))
    print 'read(%r) => %r' % (input, actual)
    assert actual == expected
#    print
