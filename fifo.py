#!/usr/bin/python
# -*- coding: utf-8 -*-

import collections
import itertools


grammar = (
    'a=p',
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

def test():
    Fo = {                          # XXX hand-constructed follow set.
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
    for s in nonterminals:
        def fmt(f):
            return ''.join(sorted(f))
        print 's=%r Fo=%r follow=%r' % (s, fmt(Fo[s]), fmt(follow[s]))
        assert set(Fo[s]) == follow[s]


XXXgrammar = (
    'e=tE',
    'E=+tE',
    'E=',
    't=fT',
    'T=*fT',
    'T=',
    'f=(e)',
    'f=I',
    )

def XXXtest():
    assert first('e') == first('t') == first('f') == set('(I')
    assert first('E') == set('+-')
    assert first('T') == set('*-')
    assert follow['e'] == follow['E'] == set(')$')
    assert follow['t'] == follow['T'] == set('+)$')
    assert follow['f'] == set('+*)$')


symbols = set(''.join(grammar).replace('=', '$'))
nonterminals = set(g[0] for g in grammar)
terminals = symbols - nonterminals


def setrep(set):
    def setfu(set):
        if '-' in set:
            yield u'\u03b5'
        for mem in sorted(set):
            if mem != '-':
                yield mem
    return '{%s}' % ' '.join(setfu(set))

def pretty_print_set(label, set):
    print label
    for x in sorted(set):
        print '    %s: %s' % (x, setrep(set[x])[1:-1])
    print


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
    pretty_print_set('sym_first', sym_first)
    return sym_first


def first(w):
    f = set('-')
    for y in w:
        f |= sym_first[y]
        if '-' not in sym_first[y]:
            f -= set('-')
            break
    # print u'first(%s) => {%s}' % (w or u'\u03b5', ', '.join(sorted(f)))
    return f


def make_follow():
    follow = collections.defaultdict(set)
    follow[grammar[0][0]] = set('$')
    pretty_print_set('follow 1', follow)
    for g in grammar:
        for i in range(3, len(g)):
            b, beta = g[i - 1], g[i:]
            if b in nonterminals:
                print u'2: follow(%s) |= first(%s) = %s (except \u03b5)' % (b, beta or u'\u03b5', setrep(first(beta)))
                follow[b] |= first(beta) - set('-')
    pretty_print_set('follow 2', follow)
    done = False
    while not done:
        done = True
        print 'closing follow'
        for g in grammar:
            for i in reversed(range(2, len(g))):
                a, b, beta = g[0], g[i], g[i+1:]
                if '-' not in first(beta):
                    break
                # else:
                #     print u'%s =*=> \u03b5' % (beta or u'\u03b5')
                if b in nonterminals:
                    print '3: follow(%s) |= follow(%s) = %s' % (b, a, setrep(follow[a]))
                    if not follow[b] >= follow[a]:
                        done = False
                        follow[b] |= follow[a]
    pretty_print_set('follow 3', follow)
    return follow


sym_first = make_sym_first()
follow = make_follow()
test()

