#!/usr/bin/python

"""Minimal Scheme interpreter.

   There is no reader; you have to build Scheme expressions using
   Python.  This primarily exists to demonstrate a non-recursive
   Scheme evaluator which could support
   call-with-current-continuation, aka call/cc.
"""

import pprint


# prerequisites for Scheme evaluator


class Nil:

    def __repr__(self):
        return '()'

    def __nonzero__(self):
        raise NotImplementedError

Nil = Nil()                             # singleton


class Pair:

    def __init__(self, car, cdr):
        self._car, self._cdr = car, cdr

    def __repr__(self):
        def pieces(p):
            while is_pair(p):
                yield repr(p.car())
                p = p.cdr()
            if not is_null(p):
                yield '.'
                yield repr(p)
        return '(%s)' % ' '.join(pieces(self))

    def car(self):
        return self._car

    def cdr(self):
        return self._cdr

    def setcar(self, p):
        self._car = p

    def setcdr(self, p):
        self._cdr = p

def cons(car, cdr):
    return Pair(car, cdr)


class Symbol(object):

    symbols = {}

    def __new__(cls, name):
        if name not in cls.symbols:
            cls.symbols[name] = super(Symbol, cls).__new__(cls)
        return cls.symbols[name]
        
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name


class Env:
    def __init__(self, parent):
        self.parent = parent
        self.bindings = {}
    def __repr__(self):
        def ancestors(env):
            while not is_null(env):
                yield 'base' if env is base_env else repr(env.bindings)
                env = env.parent
        return '<Env %s>' % ' => '.join(ancestors(self))
    def bind(self, sym, value):
        assert sym not in self.bindings
        self.bindings[sym] = value
    def lookup(self, sym):
        assert is_symbol(sym)
        if sym in self.bindings:
            return self.bindings[sym]
        return self.parent and self.parent.lookup(sym)


class Procedure(object):
    def __init__(self, name, body, args, env, is_special_form=False):
        self.name = name
        self.body = body
        self.args = args
        self.env = env
        self.is_special = is_special_form
    def __call__(self, **kwargs):
        return self.func(**kwargs)
    def __repr__(self):
        return '<Procedure %s%r>' % (self.name, self.args)
    def is_special_form(self):
        return self.is_special
    def is_primitive(self):
        return callable(self.body)


class Frame:

    """A continuation frame.
    
       Scheme's call 'stack' is actually a graph of continuation
       frames.  Each frame points at its parent (caller) but because a
       frame may outlive its return, it can't be freed in strict LIFO
       order like stack elements.
    """

    def __init__(self, parent, cont, **kwargs):
        self.parent = parent
        self.cont = cont
        self.regs = kwargs
    def __repr__(self):
        cont = Nil if is_null(self.cont) else self.cont.func_name
        return ('<Frame cont=%(cont)r\n'
                '       regs=%(regs)r>' % dict(cont=cont, regs=self.regs))


def is_null(exp):
    return exp == Nil

def is_number(exp):
    return isinstance(exp, int)

def is_boolean(exp):
    return isinstance(exp, bool)

def is_symbol(exp):
    return isinstance(exp, Symbol)

def is_pair(exp):
    return isinstance(exp, Pair)

def is_procedure(exp):
    return isinstance(exp, Procedure)


def is_self_evaluating(exp):
    return is_null(exp) or is_number(exp) or is_boolean(exp)
    # return is_null(exp) or is_number(exp) or is_boolean(exp)

def is_simple_case(exp):
    return is_self_evaluating(exp) or is_symbol(exp)

def simple_value(exp, env):
    if is_self_evaluating(exp):
        return exp
    if is_symbol(exp):
        return env.lookup(exp)
    raise Exception('not simple')

def RETURN(val):
    global f
    f = f.parent
    f.regs['val'] = val
    return f

def GOTO(target, target_args):
    global f
    f = Frame(f.parent, target, **target_args)
    return f

def CALL_THEN_GOTO(callee, callee_args, target, target_args):
    global f
    f = Frame(f.parent, target, **target_args)
    f = Frame(f, callee, **callee_args)
    return f

def RAISE(condition):
    raise NotImplementedError


# begin Scheme evaluator


def eval_application(proc, args, env):
    body = proc.body
    if proc.is_primitive():
        return GOTO(body, {'args': args, 'env': env})
    new_env = Env(proc.env)
    formals = proc.args
    actuals = args
    while not is_null(formals) or not is_null(actuals):
        if is_null(formals):
            rest = actuals
            break
        if is_null(actuals):
            return RAISE('not enough args')
        new_env.bind(formals.car(), actuals.car())
        formals = formals.cdr()
        actuals = actuals.cdr()
    if is_null(body):
        return RETURN(Nil)
    return GOTO(eval_sequence, {'seq': body, 'env': new_env})
    

def eval(exp, env):
    if is_simple_case(exp):
	return RETURN(simple_value(exp, env))
    if not is_pair(exp):
        return RAISE('&syntax')
    proc = exp.car()
    actuals = exp.cdr()
    return CALL_THEN_GOTO(eval, {'exp': proc, 'env': env},
                          accum_operator, {'args': actuals, 'env': env})


def accum_operator(val, args, env):
    proc = val
    assert is_procedure(proc)
    if proc.is_special_form() or is_null(args):
        return eval_application(proc, args, env)
    first_actual = args.car()
    rest_args = args.cdr()
    return CALL_THEN_GOTO(eval, {'exp': first_actual, 'env': env},
                          accum_arg, {'seq': rest_args,
                                      'proc': proc,
                                      'args': Nil,
                                      'nxarg': Nil,
                                      'env': env})


def accum_arg(seq, val, proc, args, nxarg, env):
    p = cons(val, Nil)
    if is_null(args):
        args = p
    else:
        nxarg.setcdr(p)
    nxarg = p
    if is_null(seq):
        return eval_application(proc, args, env)
    exp = seq.car()
    seq = seq.cdr()
    return CALL_THEN_GOTO(eval, {'exp': exp, 'env': env},
                          accum_arg, {'seq': seq,
                                      'proc': proc,
                                      'args': args,
                                      'nxarg': nxarg,
                                      'env': env})


def eval_sequence(seq, env, val=Nil):
    first = seq.car()
    rest = seq.cdr()
    if is_null(rest):
        return GOTO(eval, {'exp': first, 'env': env})
    return CALL_THEN_GOTO(eval, {'exp': first, 'env': env},
                          eval_sequence, {'seq': rest, 'env': env})


# end Scheme evaluator


base_env = Env(Nil)

def bind_proc(name, is_special_form=False):
    def wrap(func):
        global base_env
        p = Procedure(name,
                      func,
                      Nil,
                      base_env,
                      is_special_form=is_special_form)
        base_env.bind(Symbol(name), p)
        return p
    return wrap


# primitives


@bind_proc('lambda', is_special_form=True)
def p_lambda(args, env):
    params = args.car()
    body = args.cdr()
    return RETURN(Procedure('<lambda>', body, params, env))


@bind_proc('+')
def p_add(args, env):
    n = 0
    while not is_null(args):
        n += args.car()
        args = args.cdr()
    return RETURN(n)


@bind_proc('return_3', is_special_form=True)
def p_return_3(args, env):
    return RETURN(3)

# main


def do_eval(exp):
    global base_env
    global f
    f = Frame(Nil, Nil)
    f = Frame(f, eval, exp=exp, env=base_env)
    # print
    # pprint.pprint(f)
    while f.cont != Nil:
        f = f.cont(**f.regs)
        # pprint.pprint(f)
    return f.regs['val']


def test_eval(rep, expected, exp):
    assert repr(exp) == rep, 'repr %r != %s' % (exp, rep)
    val = do_eval(exp)
    if callable(expected):
        assert expected(val), 'eval %r failed' % exp
    else:
        assert val == expected, 'eval %r: %r != %r' % (exp, val, expected)
    print 'OK %r => %r' % (exp, val)


def L(*args):
    """list."""
    return cons(args[0], L(*args[1:])) if args else Nil


base_env.bind(Symbol('x'), 3)
lammie = Symbol('lambda')
plus = Symbol('+')
x = Symbol('x')

test_eval('123', 123, 123)
test_eval('True', True, True)
test_eval('()', Nil, Nil)
test_eval('x', 3, x)

# (+) => 0
test_eval('(+)', 0, L(plus))

# (+ 3) => 3
test_eval('(+ 3)', 3, L(plus, 3))

# (+ 3 4) => 7
test_eval('(+ 3 4)', 7, L(plus, 3, 4))
# assert do_eval(cons(plus, cons(3, cons(4, Nil)))) == 7

# (+ (+ 1 2) (+ 3 4)) => 10
test_eval('(+ (+ 1 2) (+ 3 4))', 10,
          L(plus, L(plus, 1, 2), L(plus, 3, 4)))

# (lambda ()) => <Procedure>
test_eval('(lambda ())', is_procedure, L(lammie, L()))

# ((lambda ())) => ()
test_eval('((lambda ()))', Nil, L(L(lammie, L())))

# ((lambda (x)) 4) => ()
test_eval('((lambda (x)) 4)', Nil, L(L(lammie, L(x)), 4))

# ((lambda (x) (+ x 3)) 4) => 7
test_eval('((lambda (x) (+ x 3)) 4)', 7,
          L(L(lammie, L(x), L(plus, x, 3)), 4))

# ((lambda (x) (+ x x)) 4) => 8
test_eval('((lambda (x) (+ x x)) 4)', 8,
          L(L(lammie, L(x), L(plus, x, x)), 4))

# ((lambda (x) (+) (+ x 3)) 4) => 7
test_eval('((lambda (x) (+) (+ x 3)) 4)', 7,
          L(L(lammie, L(x), L(plus), L(plus, x, 3)), 4))

# (return_3) => 3
test_eval('(return_3)', 3, L(Symbol('return_3')))
