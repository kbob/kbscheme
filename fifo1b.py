#!/usr/bin/python
# -*- coding: utf-8 -*-

import collections
import itertools


# Nonterminals
#   p program
#   d datum
#   i sequence (list interior)
#   e elements (vector interior)
#   b bytes (bytevector interior)
#   x comment
#   y comments

# Terminals
#   N number
#   S simple
#   A abbrev
#   X comment
#   V begin_vector
#   B begin_bytevector
#   ( ) . ; [ ] themselves


grammar = (
    'p=xp',
    'p=d',
    'p=',

    'd=N',
    'd=S',
    'd=(i)',
    'd=[i]',
    'd=Ve)',
    'd=Bb)',
    'd=Ad',

    'i=dj',
    'i=xi',
    'i=',
    
    'j=dj',
    'j=xj',
    'j=.ydy',
    'j=',

    'e=de',
    'e=xe',
    'e=',

    'b=Nb',
    'b=xb',
    'b=',

    'x=;d',

    'y=xy',
    'y=',
    )


def test():
    Fo = {                          # XXX hand-constructed follow set.
        'p': '$',
        'd': 'NS([VBA]).;$',
        'i': '])',
        'j': '])',
        'e': ')',
        'b': ')',
        'x': 'NS([VBA]).;$',
        'y': 'NS([VBA])',
        }
    assert all(s in nonterminals for s in Fo)
    for s in nonterminals:
        def fmt(f):
            return ''.join(sorted(f))
        # print 's=%r Fo=%r follow=%r' % (s, fmt(Fo[s]), fmt(follow[s]))
        assert set(Fo[s]) == follow[s], 'Fo[%r] got %r, expected %r' % (s, fmt(follow[s]), fmt(Fo[s]))


symbols = set(''.join(grammar).replace('=', '$'))
nonterminals = set(g[0] for g in grammar)
terminals = symbols - nonterminals
start_symbol = grammar[0][0]


def setrep(set):
    def setfu(set):
        if '-' in set:
            yield u'\u03b5'
        for mem in sorted(set):
            if mem != '-':
                yield mem
    return ''.join(setfu(set))


def pretty_print_set(label, set, key=None):
    print '%s = {' % label[:2]
    w = max(len(i) for i in set) + 3
    for x in sorted(set, key=key):
        print "    %-*s '%s'," % (w, "'%s':" % x, setrep(set[x]))
    print '}\n\n'
    

def make_sym_first():
    sym_first = collections.defaultdict(set)
    for t in terminals:
        sym_first[t].add(t)
    for n in nonterminals:
        if '%s=' % n in grammar:
            sym_first[n].add('-')
    done = False
    while not done:
        done = True
        for g in grammar:
            x = g[0]
            for y in g[2:]:
                if not sym_first[x] >= sym_first[y]:
                    sym_first[x] |= sym_first[y]
                    done = False
                if '-' not in sym_first[y]:
                    break
    # pretty_print_set('sym_first', sym_first)
    return sym_first


def first(w):
    f = set('-')
    for y in w:
        f |= sym_first[y]
        if '-' not in sym_first[y]:
            f -= set('-')
            break
    # print u'first(%s) => {%s}' % (w or u'\u03b5', ' '.join(sorted(f)))
    return f


def make_follow():
    follow = collections.defaultdict(set)
    follow[start_symbol] = set('$')
    for g in grammar:
        for i in range(3, len(g)):
            b, beta = g[i - 1], g[i:]
            if b in nonterminals:
                follow[b] |= first(beta) - set('-')
    done = False
    while not done:
        done = True
        for g in grammar:
            for i in reversed(range(2, len(g))):
                a, b, beta = g[0], g[i], g[i+1:]
                if '-' not in first(beta):
                    break
                if b in nonterminals:
                    if not follow[b] >= follow[a]:
                        done = False
                        follow[b] |= follow[a]
    def keyfunc(sym):
        for i, rule in enumerate(grammar):
            if rule.startswith(sym):
                return i
    pretty_print_set('Follow', follow, key=keyfunc)
    return follow


sym_first = make_sym_first()

first_dict = dict((rule, first(rule[2:])) for rule in grammar)
pretty_print_set('First', first_dict,
                 key=lambda rule: list(grammar).index(rule))

follow = make_follow()
test()

