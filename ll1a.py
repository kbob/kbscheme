#!/usr/bin/python
# -*- coding: utf-8 -*-

import collections
import itertools
import pprint
import re


# Nonterminals
#   d datum
#   i sequence (list interior)
#   e elements (vector interior)
#   b bytes (bytevector interior)

# Terminals
#   N number
#   S simple
#   A abbrev
#   X comment
#   V begin_vector
#   B begin_bytevector
#   ( ) . [ ] themselves


grammar = (
    'd=N',
    'd=S',
    'd=(i)',
    'd=[i]',
    'd=Ve)',
    'd=Bb)',
    'd=Ad',

    'i=dj',
    'i=',
    
    'j=dj',
    'j=.d',
    'j=',

    'e=de',
    'e=',

    'b=Nb',
    'b=',
    )


symbols = set(''.join(grammar).replace('=', '$'))
nonterminals = set(r[0] for r in grammar)
terminals = symbols - nonterminals
start_symbol = grammar[0][0]


Fi = {
    'd=N'  : 'N',
    'd=S'  : 'S',
    'd=(i)': '(',
    'd=[i]': '[',
    'd=Ve)': 'V',
    'd=Bb)': 'B',
    'd=Ad' : 'A',

    'i=dj' : 'NS([VBA',
    'i='   : 'ε',
    
    'j=dj' : 'NS([VBA',
    'j=.d' : '.',
    'j='   : 'ε',

    'e=de' : 'NS([VBA',
    'e='   : 'ε',

    'b=Nb' : 'N',
    'b='   : 'ε',
    }
assert set(Fi) <= set(grammar), 'Fi extra %r' % (set(Fi) - set(grammar))
assert set(Fi) == set(grammar), 'Fi missing %r' % (set(grammar) - set(Fi))


Fo = {
    'd': 'NS([VBA]).$',
    'i': '])',
    'j': '])',
    'e': ')',
    'b': ')',
    }
assert set(Fo) <= nonterminals, 'Fo extra %r' % (set(Fo) - nonterminals)
assert set(Fo) == nonterminals, 'Fo missing %r' % (nonterminals - set(Fo))


def make_parsing_table():
    def entry_rules(A, a):
        for g in grammar:
            if g[0] == A:
                if a in Fi[g]:
                    yield g
                if 'ε' in Fi[g] and a in Fo[A]:
                    yield g
    def entry(A, a):
        rules = list(entry_rules(A, a))
        assert len(rules) <= 1, 'grammar is not LL(1)'
        return rules and rules[0] or None
    T = dict((n, (dict((t, entry(n, t))
                       for t in terminals)))
             for n in nonterminals)
    return T


T = make_parsing_table()
if False:
    def tk(t):
        return (t == '$', t)
    def pt(n, t):
        z = T[n][t]
        if z is None:
            return '-'
        for i, rule in enumerate(grammar):
            if rule == z:
                return i
    tt = list(sorted(terminals, key=tk))
    print '      %s' % (''.join('%-3s' % t for t in tt))
    print '    ' + '---' * len(tt)
    for n in sorted(nonterminals):
        print '%3c : %s' % (n, ''.join('%-3s' % pt(n, t) for t in tt))
    exit()


red_map = {
    'd=(i)':  'OpenList',
    'd=[i]':  'OpenList',
    'd=Ve)':  'OpenVector',
    'd=Bb)':  'OpenByteVector',
    'd=Ad':   'Abbrev',

    'i=':     'CloseList',

    'j=.d':   'DotClose',
    'j=':     'CloseList',

    'e=':     'CloseVector',

    'b=':     'CloseByteVector',
    }
assert set(red_map) <= set(grammar), \
       'red_map extra %r' % (set(red_map) - set(grammar))


class nil(object):

    class __metaclass__(type):
        def __repr__(self):
            return '()'

        def __nonzero__(self):
            return False


class sym(str):

    def __repr__(self):
        return super(sym, self).__str__()


class cons(object):

    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __iter__(self):
        yield self.car
        cdr = self.cdr
        if cdr is not nil:
            assert isinstance(cdr, cons)
            for x in cdr:
                yield x

    def __repr__(self):
        def inrep(p):
            s = sep = ''
            while p is not nil:
                s += sep + repr(p.car)
                p = p.cdr
                if p is not nil and not isinstance(p, cons):
                    s += ' . ' + repr(p)
                    break
                sep = ' '
            return s
        return '(%s)' % inrep(self)


class Vector(list):

    def __repr__(self):
        return '#(%s)' % ' '.join(repr(i) for i in self)


class ByteVector(list):

    def __init__(self, ll):
        assert all(isinstance(e, int) for e in ll)
        assert all(0 <= e < 256 for e in ll)
        super(ByteVector, self).__init__(ll)

    def __repr__(self):
        return '#vu8(%s)' % ' '.join(repr(i) for i in self)


class EOF(object):

    class __metaclass__(type):
        def __repr__(self):
            return 'EOF'


class Syntax(Exception):

    def __repr__(self):
        return '&syntax'


tokmap = {
    '1': 'N',
    '2': 'N',
    '3': 'N',
    'a': 'S',
    'b': 'S',
    'c': 'S',
    'd': 'S',
    "'": 'A',
    }


def yylex(input):
    global yylval
    yylval = None
    if input:
        token = input.pop(0)
        if token in 'abcd':
            yylval = sym(token)
        if token == "'":
            yylval = sym('quote')
        if token in '123':
            yylval = int(token)
        token = tokmap.get(token, token)
        return token
    else:
        return '$'


def parse(T, input):
    ostack = []
    stack = ['$', start_symbol]
    input = list(input)
    tok = yylex(input)
    while True:
        i = stack[-1]
        rule = T.get(i, {}).get(tok)
        if rule:
            stack[-1:] = rule[:1:-1]  # pop; push reversed RHS
            red = red_map.get(rule, '')
            if red:
                ostack.append(red)
        else:
            sym = stack.pop()
            if sym == '$':
                break
            if sym != tok:
                raise Syntax()
            if yylval is not None:
                ostack.append(yylval)
            tok = yylex(input)
    return ostack


def build(actions):
    vstack = []
    reg = nil
    for action in reversed(actions):
        if action == 'CloseList':
            vstack.append(reg)
            reg = nil
        elif action == 'DotClose':
            vstack.append(reg.cdr)
            reg = reg.car
        elif action == 'Abbrev':
            reg = cons(cons(reg.car, cons(reg.cdr.car, nil)), reg.cdr.cdr)
        elif action == 'CloseVector':
            vstack.append(reg)
            reg = nil
        elif action == 'CloseByteVector':
            vstack.append(reg)
            reg = nil
        elif action == 'OpenList':
            parent = vstack.pop()
            reg = cons(reg, parent)
        elif action == 'OpenVector':
            reg = Vector(reg)
            parent = vstack.pop()
            reg = cons(reg, parent)
        elif action == 'OpenByteVector':
            reg = ByteVector(reg)
            parent = vstack.pop()
            reg = cons(reg, parent)
        else:
            reg = cons(action, reg)
    assert len(vstack) == 0
    if reg:
        assert reg.cdr is nil
        return reg.car
    return EOF


def read(input):
    try:
        return build(parse(T, list(input)))
    except Syntax, ex:
        return ex


tests = [
    ['',              'EOF'],
    ['a',             'a'],
    ['1',             '1'],
    ['()',            '()'],
    ['(1)',           '(1)'],
    ['(a)',           '(a)'],
    ['(ab)',          '(a b)'],
    ['(1b)',          '(1 b)'],
    ['(a2)',          '(a 2)'],
    ['(a.b)',         '(a . b)'],
    ['((a.b))',       '((a . b))'],
    ['(a(b.c))',      '(a (b . c))'],
    ['((a.b)c)',      '((a . b) c)'],
    ['((a.b).(c.d))', '((a . b) c . d)'],
    ['(ab(cd))',      '(a b (c d))'],
    ['((ab)cd)',      '((a b) c d)'],
    ['[]',            '()'],
    ['[1]',            '(1)'],
    ['([])',          '(())'],
    ['[[][][]]',      '(() () ())'],
    ['([(a)])',       '(((a)))'],
    ['[([a])]',       '(((a)))'],
    ['[ab[cd]]',      '(a b (c d))'],
    ['Vabc)',         '#(a b c)'],
    ['Va2c)',         '#(a 2 c)'],
    ['VaVbc))',       '#(a #(b c))'],
    ['B123)',         '#vu8(1 2 3)'],
    ["'a",            '(quote a)'],
    ["'(ab)",         '(quote (a b))'],
    ["V'a'(ab))",     '#((quote a) (quote (a b)))'],
    ["('abc)",        '((quote a) b c)'],
    ["(a'bc)",        '(a (quote b) c)'],
    ["(ab'c)",        '(a b (quote c))'],
    ["'''a",          '(quote (quote (quote a)))'],
    [';a',            'EOF'],
    [';ab',           'b'],
    ['(;a)',          '()'],
    ['(a;b)',         '(a)'],
    ['(a;bc)',        '(a c)'],
    [';(a(b))(c)',    '(c)'],
    [';(a(b))',       'EOF'],
    [')',             '&syntax'],
    ["'",             '&syntax'],
    ['(',             '&syntax'],
    ['(a',            '&syntax'],
    ['( . )',         '&syntax'],
    ['(a . )',        '&syntax'],
    ['( . a)',        '&syntax'],
    ['[)',            '&syntax'],
    ['(]',            '&syntax'],
    ['V]',            '&syntax'],
    ['B]',            '&syntax'],
    ['Ba)',           '&syntax'],
    ["';a",           '&syntax']
    ]


for input, expected in tests:
    if ';' in input or 'EOF' in expected:
        print '  skipping %s' % input
        continue
    print 'testing %s' % input
    actual = read(input)
    assert repr(actual) == expected, 'got %r, expected %s' % (actual, expected)
