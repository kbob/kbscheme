#!/usr/bin/python
# -*- coding: utf-8 -*-

import collections
import itertools
import pprint
import re


grammar = (
#    'a=p',
#    'p=xp',
    'p=d',
    'p=',
#    'd=s',
#    'd=c',
#    's=N',
#    's=S',
#    'c=(i)',
#    'c=[i]',
#    'c=Ve)',
#    'c=Bb)',
#    'c=Ad',
    'd=N',
    'd=S',
    'd=(i)',
    'd=[i]',
    'd=Ve)',
    'd=Bb)',
    'd=Ad',

    'i=di',
#    'i=xi',
    'i=dDd',
    'i=',
    'e=de',
#    'e=xe',
    'e=',
    'b=Nb',
#    'b=xb',
    'b=',
#    'x=Cd',
    )


red_map = {
    'p=d':   'ACCEPT',
    'p=':    'EOF',
    'c=(i)': '$$ = $2',
    'c=[i]': '$$ = $2',
    'c=Ve)': '$$ = Vector($2)',
    'c=Bb)': '$$ = ByteVector($2)',
    'c=Ad':  '$$ = abbrev($1, $2)',
    'i=di':  '$$ = Cons($1, $2)',
#    'i=xi':  '$$ = $2',
    'i=dDd': '$$ = Cons($1, $3)',
    'i=':    '$$ = Nil',
    'e=de':  '$$ = Cons($1, $2)',
#    'e=xe':  '$$ = $2',
    'e=':    '$$ = Nil',
    'b=Nb':  '$$ = Cons($1, $2)',
#    'b=xb':  '$$ = $2',
    'b=':    '$$ = Nil',
    }


Fi = {
    'a=p'  : 'NS([VB-',
    'p=xp' : 'C',
    'p=d'  : 'NS([VB',
    'p='   : '-',
#    'd=s'  : 'NS',
#    'd=c'  : '([VB',
#    's=N'  : 'N',
#    's=S'  : 'S',
#    'c=(i)': '(',
#    'c=[i]': '[',
#    'c=Ve)': 'V',
#    'c=Bb)': 'B',
#    'c=Ad' : 'A',
    'd=N'  : 'N',
    'd=S'  : 'S',
    'd=(i)': '(',
    'd=[i]': '[',
    'd=Ve)': 'V',
    'd=Bb)': 'B',
    'd=Ad' : 'A',
    'i=di' : 'NS([VB',
    'i=xi' : 'C',
    'i=dDd': 'NS([VB',
    'i='   : '-',
    'e=de' : 'NS([VB',
    'e=xe' : 'C',
    'e='   : '-',
    'b=Nb' : 'N',
    'b=xb' : 'C',
    'b='   : '-',
    'x=Cd' : 'C',
    }


Fo = {
    'a': '$',
    'p': 'NS([VBAC$',
    'd': 'NS([VBAC])D$',
    's': 'NS([VBAC])D$',
    'c': 'NS([VBAC])D$',
    'i': 'NS([VBAC])D',
    'e': 'NS([VBAC)',
    'b': 'NC)',
    'x': '$NS([VBAC)D',
    }


def make_parsing_table():
    nonterminals = set(r[0] for r in grammar)
    symbols = set(''.join(grammar).replace('=', '$'))
    terminals = symbols - nonterminals
    # def ps(name, value):
    #     print name, ''.join(sorted(value)), len(value)
    # ps('terminals', terminals)
    # ps('nonterminals', nonterminals)
    # ps('symbols', symbols)
    def entry_rules(A, a):
        for g in grammar:
            print 'g', g
            if a in Fi[g]:
                yield g
            if '-' in Fi[g] and A in Fo[A]:
                yield g
    def entry(A, a):
        rules = list(entry_rules(A, a))
        if len(rules) > 1:
            print 'A=%r a=%r rules=%r' % (A, a, rules)
        assert len(rules) <= 1, 'grammar is not LL(1)'
        return rules and rules[0] or None
    T = [[entry(n, t) for t in terminals] for n in nonterminals]
    print 'T'
    pprint.pprint(T)


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


make_parsing_table()
