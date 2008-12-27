#!/usr/bin/python
# -*- coding: utf-8 -*-

import collections
import itertools
import pprint
import re


grammar = (
    'p=xp',
    'p=d',
    'p=',
    'd=s',
    'd=c',
    's=N',
    's=S',
    'c=(i)',
    'c=[i]',
    'c=Ve)',
    'c=Bb)',
    'c=Ad',
    'i=di',
    'i=xi',
    'i=dDd',
    'i=',
    'e=de',
    'e=xe',
    'e=',
    'b=Nb',
    'b=xb',
    'b=',
    'x=Cd',
    )
grammar = ('Z=p',) + grammar


tokmap = {
    '1': 'N',
    '2': 'N',
    '3': 'N',
    'a': 'S',
    'b': 'S',
    'c': 'S',
    'd': 'S',
    '.': 'D',
    "'": 'A',
    ';': 'C',
    }


red_map = {
    'p=d':   'ACCEPT',
    'p=':    'EOF',
    'c=(i)': '$$ = $2',
    'c=[i]': '$$ = $2',
    'c=Ve)': '$$ = Vector($2)',
    'c=Bb)': '$$ = ByteVector($2)',
    'c=Ad':  '$$ = abbrev($1, $2)',
    'i=di':  '$$ = Cons($1, $2)',
    'i=xi':  '$$ = $2',
    'i=dDd': '$$ = Cons($1, $3)',
    'i=':    '$$ = Nil',
    'e=de':  '$$ = Cons($1, $2)',
    'e=xe':  '$$ = $2',
    'e=':    '$$ = Nil',
    'b=Nb':  '$$ = Cons($1, $2)',
    'b=xb':  '$$ = $2',
    'b=':    '$$ = Nil',
    }


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

    def __len__(self):
        if isinstance(self.cdr, cons):
            return 1 + len(self.cdr)
        else:
            return 1 + (self.cdr is not nil)

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

    def __new__(cls, ll):
        return super(Vector, cls).__new__(cls)

    def __init__(self, ll):
        while ll:
            self.append(ll.car)
            ll = ll.cdr

    def __repr__(self):
        return '#(%s)' % ' '.join(repr(i) for i in self)


class ByteVector(list):

    def __new__(cls, ll):
        return super(ByteVector, cls).__new__(cls)

    def __init__(self, ll):
        while ll:
            assert isinstance(ll.car, int)
            self.append(ll.car)
            ll = ll.cdr

    def __repr__(self):
        return '#vu8(%s)' % ' '.join(repr(i) for i in self)


def yylex(input):
    global yylval
    yylval = nil
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
        

def parse(input):

    def act(production):
        global yypval
        action = red_map.get(production, '$$ = $1')
        # print 'action %r' % action
        ix = len(vstack) + drop
        if action == '$$ = Nil':
            value = nil
        elif action == '$$ = $1':
            value = vstack[ix]
        elif action == '$$ = $2':
            value = vstack[ix + 1]
        elif action == '$$ = Vector($2)':
            value = Vector(vstack[ix + 1])
        elif action == '$$ = ByteVector($2)':
            value = ByteVector(vstack[ix + 1])
        elif action == '$$ = abbrev($1, $2)':
            value = cons(vstack[ix], cons(vstack[ix + 1], nil))
        elif action == '$$ = Cons($1, $2)':
            value = cons(vstack[ix], vstack[ix + 1])
        elif action == '$$ = Cons($1, $3)':
            value = cons(vstack[ix], vstack[ix + 2])
        elif action == 'ACCEPT':
            yypval = vstack[-1]
            return 'ACCEPT'
        elif action == 'EOF':
            yypval = 'EOF'
            return 'EOF'
        else:
            assert False, 'unknown action "%s"' % action
        return value
            
    stack = [0]
    vstack = []
    token = yylex(input)
    while True:
        action = actions[stack[-1]][token]
        v, k = action[0], int(action[1:])
        if v == 's':
            vstack.append(yylval)
            NS = goto[stack[-1]][token]
            stack.append(NS)
            token = yylex(input)
        elif v == 'r':
            drop = 2 - len(grammar[k])
            value = act(grammar[k])
            if value == 'ACCEPT':
                return
            elif value == 'EOF':
                return
            if drop < 0:
                stack = stack[:drop]
                vstack = vstack[:drop]
            vstack.append(value)
            NS = goto[stack[-1]][grammar[k][0]]
            stack.append(NS)


def all_items():
    for p in grammar:
        lhs, rhs = p.split('=')
        for i in range(len(rhs) + 1):
            yield '%s=%s.%s' % (lhs, rhs[:i], rhs[i:])


class Item(str):

    def __repr__(self):
        def idx():
            for i, it in enumerate(all_items()):
                if self == it:
                    return i
        def ex0(s):
            return ''.join(' %c' % c for c in s)
        def ex1(s):
            return ''.join('%c ' % c for c in s)
        lhs, rhs = self.split('=')
        r0, r1 = rhs.split('.')
        return '(%d) %s ::= %s^%s' % (idx(), lhs, ex0(r0), ex1(r1))
        

def enumerate_item_set(item_set):
    return ItemSet(item_set).enumerate()


class ItemSet(set):

    def __str__(self):
        return ' '.join(str(i) for i, it in self.enumerate())

    def __repr__(self):
        def ex0(s):
            return ''.join(' %c' % c for c in s)
        def ex1(s):
            return ''.join('%c ' % c for c in s)
        def rit(i, item):
            lhs, rhs = item.split('=')
            r0, r1 = rhs.split('.')
            return '%2d   %s ::= %s^%s' % (i, lhs, ex0(r0), ex1(r1))
        return '\n'.join(rit(i, it) for i, it in self.enumerate())

    def enumerate(self):
        def filter(pair):
            return pair[1] in self
        return itertools.ifilter(filter, enumerate(all_items()))


def XXXclose(item_set):
    closure = ItemSet(item_set)
    next_syms = set()
    items = list(item_set)
    while items:
        item = items.pop()
        m = re.search(r'\.(.)', item)
        if m:
            sym = m.group(1)
            if sym in nonterminals and sym not in next_syms:
                next_syms.add(sym)
                for rule in grammar:
                    if rule.startswith(sym):
                        new_item = rule.replace('=', '=.')
                        items.append(new_item)
                        closure.add(new_item)
    # print 'closure(%s) => %s' % (ItemSet(item_set), closure)
    return closure

def close(set):
    print 'close [%s]' % ItemSet(set)
    def item_number(n):
        for i, item in enumerate(all_items()):
            if i == n:
                return item
    unprocessed = [item in set for item in all_items()]
    done = False
    while not done:
        done = True
        for i in (ii for ii, iin in enumerate(unprocessed) if iin):
            unprocessed[i] = False
            item = item_number(i)
            #print 'closing %r' % Item(item)
            m = re.search(r'\.(.)', item)
            if m:
                nextsym = m.group(1)
                if nextsym in nonterminals:
                    #print 'nextsym = %s' % nextsym
                    prefix = '%c=.' % nextsym
                    for j, item2 in enumerate(all_items()):
                        #print 'j=%r, item2=%r' % (j, item2)
                        if item2.startswith(prefix):
                            #print 'adding %r' % Item(item2)
                            set.add(item2)
                            unprocessed[j] = True
                            done = False
    print 'close returns [%s]' % ItemSet(set)
    #print
    return set


def iskey(item):
    for i, it in enumerate(all_items()):
        if item == it:
            return i
    raise Exception

def advance(item_set):
    new_set = collections.defaultdict(set)
    for item in sorted(item_set, key=iskey):
        m = re.search(r'\.(.)', item)
        if not m:
            continue
        sym = m.group(1)
        new_item = re.sub(r'\.(.)', r'\1.', item)
        new_set[sym].add(new_item)
    for sym in sorted(new_set):
        new_set[sym] = close(new_set[sym])
    # print 'advance(%s) => %s\n' % (item_set, new_set)
    return new_set


# # XXX old version
# def transition_table(grammar):
#     def find_state(state):
#         try:
#             return states.index(state)
#         except ValueError:
#             row = len(states)
#             states.append(state)
#             rows.append(row)
#             tt.append({})
#             return row
#     item_0 = grammar[0].replace('=', '=.')
#     states = [close(set((item_0,)))]
#     tt = [{}]
#     rows = [0]
#     while rows:
#         row = rows.pop(0)
#         next_states = advance(states[row])
#         d = {}
#         # for sym, state in next_states.iteritems():
#         for sym in sorted(next_states, key=lambda s: next_states[s]):
#             state = next_states[sym]
#             # print 'sym', sym
#             # print 'state', state
#             next_row = find_state(state)
#             tt[row][sym] = next_row
#     return states, tt


def transition_table(grammar):
    set_0 = set([grammar[0].replace('=', '=.')])
    set_0 = close(set_0)
    item_sets = [set_0]
    tt = [{}]
    for i, iset in enumerate(item_sets):
        for j in range(256):
            if chr(j) in symbols:
                symbol = chr(j)
                # print 'symbol %s' % symbol
                nset = set()
                for k, item in enumerate_item_set(iset):
                    if '.%s' % symbol in item:
                        nset.add(re.sub(r'\.(.)', r'\1.', item))
                if not nset:
                    continue
                nset = close(nset)
                try:
                    nitems = item_sets.index(nset)
                except ValueError:
                    nitems = len(item_sets)
                    item_sets.append(nset)
                    tt.append({})
                tt[i][symbol] = nitems
    return item_sets, tt


def action_table(states, tt):
    def terminal_columns(row):
        return dict((sym, 's%d' % row[sym]) for sym in row if sym in terminals)
    def reduce_states(state):
        for item in state:
            if item.endswith('.'):
                m = lgrammar.index(item[:-1])
                if m:
                    yield 'r%d' % m
    def actions(state, row):
        a = terminal_columns(row)
        if accept_item in state:
            a['$'] = 'a0'
        for reduction in reduce_states(state):
            for sym in terminals | set('$'):
                if sym not in a:
                    a[sym] = reduction
        return a
    accept_item = grammar[0] + '.'
    lgrammar = list(grammar)
    return [actions(state, row) for state, row in zip(states, tt)]


def goto_table(tt):
    def nonterminal_columns(row):
        # return dict((sym, row[sym]) for sym in row if sym in nonterminals)
        return dict((sym, row[sym]) for sym in row)
    return [nonterminal_columns(row) for row in tt]


def pretty_print_states(states):
    for i, s in enumerate(states):
        print 'set %d' % i
        print re.compile(r'^', re.M).sub('    ', repr(ItemSet(s)))
        print


def pretty_print_transitions(tt):
    def format_row(row):
        return ' '.join('%2s' % row.get(s, '-') for s in symlist)

    symlist = sorted(symbols)
    print 'transitions'
    print '          %s' % '  '.join(symlist)
    print
    for i, row in enumerate(tt):
        print '    %2d   %s' % (i, format_row(row))
    print

nonterminals = set(r[0] for r in grammar)
# print 'nonterminals', nonterminals
terminals = set(ch for g in grammar for ch in g[2:] if ch not in nonterminals)
symbols = nonterminals | terminals | set('$')

states, tt = transition_table(grammar)
pretty_print_states(states)
# print 'transitions'; pprint.pprint(tt)
pretty_print_transitions(tt)
actions = action_table(states, tt)
# print 'actions'; pprint.pprint(actions)
goto = goto_table(tt)
# print 'goto ', ; pprint.pprint(goto)

def read(input):
    parse(list(input))
    return yypval


tvs = [
    ['',           "'EOF'"],
    ['a',          'a'],
    ['1',          '1'],
    ['()',         '()'],
    ['(a)',        '(a)'],
    ['(ab)',       '(a b)'],
    ['(a.b)',      '(a . b)'],
    ['(ab(cd))',   '(a b (c d))'],
    ['[]',         '()'],
    ['([])',       '(())'],
    ['[[][][]]',   '(() () ())'],
    ['([(a)])',    '(((a)))'],
    ['[([a])]',    '(((a)))'],
    ['[ab[cd]]',   '(a b (c d))'],
    ['Vabc)',      '#(a b c)'],
    ['VaVbc))',    '#(a #(b c))'],
    ['B123)',      '#vu8(1 2 3)'],
    ["'a",         '(quote a)'],
    ["'(ab)",      '(quote (a b))'],
    ["(a'bc)",     '(a (quote b) c)'],
    ["'''a",       '(quote (quote (quote a)))'],
    [';a',         "'EOF'"],
    [';ab',        'b'],
    ['(;a)',       '()'],
    ['(a;b)',      '(a)'],
    ['(a;bc)',     '(a c)'],
    [';(a(b))(c)', '(c)'],
    [';(a(b))',    "'EOF'"],
    ]

for input, expected in tvs:
    # print 'read(%s)' % input
    actual = repr(read(list(input)))
    print 'read(%r) => %r' % (input, actual)
    assert actual == expected, 'expected "%s", got "%s"' % (expected, actual)
    # print
