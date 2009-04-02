#!/usr/bin/python
# -*- coding: utf-8 -*-

import collections
import itertools


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


def test():
    Fo = {                          # XXX hand-constructed follow set.
        'a': '$',
        'p': '$',
        'd': 'NS([VBA]).$',
        's': 'NS([VBA]).$',
        'c': 'NS([VBA]).$',
        'i': '])',
        'j': '])',
        'e': ')',
        'b': ')',
        }
    for s in nonterminals:
        def fmt(f):
            return ''.join(sorted(f))
        # print 's=%r Fo=%r follow=%r' % (s, fmt(Fo[s]), fmt(follow[s]))
        assert set(Fo[s]) == follow[s], 'Fo[%r] got %r, expected %r' % (s, fmt(follow[s]), fmt(Fo[s]))


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
    return ' '.join(setfu(set))


def pretty_print_set(label, set, key=None):
    print label
    w = max(len(i) for i in set)
    for x in sorted(set, key=key):
        print '    %-*s: %s' % (w, x, setrep(set[x]))
    print


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
    # print u'first(%s) => {%s}' % (w or u'\u03b5', ', '.join(sorted(f)))
    return f


def make_follow():
    follow = collections.defaultdict(set)
    follow[grammar[0][0]] = set('$')
    # pretty_print_set('follow 1', follow)
    for g in grammar:
        for i in range(3, len(g)):
            b, beta = g[i - 1], g[i:]
            if b in nonterminals:
                # print u'2: follow(%s) |= first(%s) = %s (except \u03b5)' % (b, beta or u'\u03b5', setrep(first(beta)))
                follow[b] |= first(beta) - set('-')
    # pretty_print_set('follow 2', follow)
    done = False
    while not done:
        done = True
        # print 'closing follow'
        for g in grammar:
            for i in reversed(range(2, len(g))):
                a, b, beta = g[0], g[i], g[i+1:]
                if '-' not in first(beta):
                    break
                # else:
                #     print u'%s =*=> \u03b5' % (beta or u'\u03b5')
                if b in nonterminals:
                    # print '3: follow(%s) |= follow(%s) = %s' % (b, a, setrep(follow[a]))
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

