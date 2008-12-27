#!/usr/bin/python
# -*- coding: utf-8 -*-

import collections
import re


class MySet(set):
    def __unicode__(self):
	return u'{%s}' % u' '.join(unicode(i) for i in self)


class MyDict(collections.defaultdict):
    def __unicode__(self):
	def pairs():
	    for k in self:
		yield u'    %s: %s' % (unicode(k), unicode(self[k]))
	return u'{\n%s\n}' % u',\n'.join(pairs())

# Nonterminals
#     p program
#     d datum
#     s simple
#     c compound
#     i interior
#     e elements
#     b bytes
#     x comment
# 
# Terminals
# ( BEGIN_LIST
# ) END
# [ ALT_BEGIN_LIST
# ] ALT_END_LIST
# . PAIR
# V BEGIN_VECTOR
# B BEGIN_BYTEVECTOR
# A ABBREV
# S SIMPLE_DATUM
# N EXACT_NUMBER
# ; COMMENT

grammar = (
    (u'p=cp',	None),
    (u'p=d',   'ACCEPT'),
#    (u'p=',    'EOF'),
    (u'd=s',	None),
    (u'd=c',	None),
    (u's=N',	None),
    (u's=S',	None),
    (u'c=(i)', '$$ = $2'),
    (u'c=[i]', '$$ = $2'),
    (u'c=Ve)', '$$ = Vector($2)'),
    (u'c=Bb)', '$$ = ByteVectory($2)'),
    (u'c=Ad',  '$$ = abbrev($1, $2)'),
    (u'i=di',  '$$ = Cons($1, $2)'),
    (u'i=ci',  '$$ = $2'),
    (u'i=d.d', '$$ = Cons($1, $3)'),
    (u'i=',    '$$ = Nil'),
    (u'e=de',  '$$ = Cons($1, $2)'),
    (u'e=xe',  '$$ = $2'),
    (u'e=',    '$$ = Nil'),
    (u'b=Nb',  '$$ = Cons($1, $2)'),
    (u'b=xb',  '$$ = $2'),
    (u'b=',    '$$ = Nil'),
    (u'x=Cd',	None),
    )
ZZZgrammar = (
    (u'p=p*b', 'xyz'),
    (u'p=p+b', 'xyz'),
    (u'p=b', 'xyz'),
    (u'b=0', 'xyz'),
    (u'b=1', 'xyz'),
    )
grammar = ((u'Z=p', 'ACCEPT'),) + grammar
print 'grammar'
import pprint
pprint.pprint(grammar)
nonterminals = set(rule[0][0] for rule in grammar)
print 'nonterminals', u' '.join(sorted(nonterminals))
terminals = set(ch for ch in ''.join(r[0] for r in grammar)
		if ch not in nonterminals | set('='))
print 'terminals', u' '.join(sorted(terminals))
symbols = MySet(nonterminals | terminals | set(u'$'))


def close(item_set):
    closure = MySet(item_set)
    next_syms = set()
    items = list(item_set)
    while items:
	item = items.pop()
	m = re.search(ur'•(.)', item)
	if m:
	    sym = m.group(1)
	    if sym in nonterminals and sym not in next_syms:
		next_syms.add(sym)
		for rule in (r[0] for r in grammar):
		    if rule.startswith(sym):
			new_item = rule.replace(u'=', u'=•')
			items.append(new_item)
			closure.add(new_item)
    return closure


rule_0 = MySet([grammar[0][0].replace(u'=', u'=•')])
print 'rule 0', unicode(rule_0)
state_0 = close(rule_0)
import pprint
print 'state 0', unicode(state_0)
print
assert state_0 == close(state_0)

def advance(item_set):
    new_sets = MyDict(MySet)
    for item in item_set:
	m = re.search(ur'•(.)', item)
	if not m:
	    continue
	sym = m.group(1)
	new_item = re.sub(ur'•(.)', ur'\1•', item)
	new_sets[sym].add(new_item)
#	 new_sets.setdefault(sym, set()).add(new_item)
    for sym in new_sets:
	new_sets[sym] = close(new_sets[sym])
    return new_sets

import pprint
print 'advance from state 0'
#pprint.pprint(advance(state_0), indent=4)
print unicode(advance(state_0))

def transition_table(grammar):
    def find_state(state):
	try:
	    return states.index(state)
	except ValueError:
	    row = len(states)
	    states.append(state)
	    rows.append(row)
	    tt.append({})
	    return row
    item_0 = grammar[0].replace(u'=', u'=•')
    states = [close([item_0])]
    tt = [{}]
    rows = [0]
    while rows:
	row = rows.pop(0)
	next_states = advance(states[row])
	d = {}
	for sym, state in next_states.iteritems():
	    next_row = find_state(state)
	    tt[row][sym] = next_row
    return states, tt


print 'tt'
states, tt = transition_table([g[0] for g in grammar])
print 'states', u'[%s]' % u', '.join(unicode(s) for s in states)
print 'transition table'
pprint.pprint(tt)
print '%d states' % len(states)


def goto_table(tt):
    def nonterminal_columns(row):
	return dict((sym, row[sym]) for sym in row if sym in nonterminals)
    return [nonterminal_columns(row) for row in tt]

print 'goto table'
goto = goto_table(tt)
pprint.pprint(goto)

def action_table(states, tt):
    def terminal_columns(row):
	return dict((sym, 's%d' % row[sym]) for sym in row if sym in terminals)
    def reduce_states(state):
	for item in state:
	    if item.endswith(u'•'):
		for i, rule in enumerate(g[0] for g in grammar):
		    if i and rule == item[:-1]:
			yield 'r%d' % i
    def actions(state, row):
	a = terminal_columns(row)
	if accept_item in state:
	    a[u'$'] = 'a0'
	for reduction in reduce_states(state):
	    if a or 1:
		print 'conflict state', unicode(state)
		print '		  row', row
		print '	       reduce', reduction
		print '	       action', a
		print
	    # assert not a, 'parser conflict detected'
	    for sym in terminals:
		if sym not in a:
		    a[sym] = reduction
	    a = dict((sym, reduction) for sym in symbols)
	return a
    accept_item = grammar[0][0] + u'•'
    return [actions(state, row) for state, row in zip(states, tt)]

print 'action table'
actions = action_table(states, tt)
pprint.pprint(actions)

def parse(input):
    input = input + '$'
    stack = [0]
    while True:
#	print 'stack', stack
#	print 'stack[-1]', stack[-1]
#	print 'input[0]', input[0]
#	print 'actions[stack[-1]]', actions[stack[-1]]
	action = actions[stack[-1]][input[0]]
	v, n = action[0], int(action[1:])
	if v == 's':
	    term, input = input[0], input[1:]
            print 'CONSUMING', term
	    stack.append(n)
	    print 'shift %s %s' % (action, term)
	elif v == 'r':
	    rhl = len(grammar[n][0]) - 2
	    stack, popped = stack[:-rhl], stack[-rhl:]
	    print 'reduce %s' % (action, )
	    yield grammar[n], popped
            print 'n', n
            print 'grammar[n]', grammar[n]
            print 'stack', stack
	    stack.append(goto[stack[-1]][grammar[n][0][0]])
	elif v == 'a':
	    yield stack.pop()
	    print 'final stack', stack
	    break
	else:
	    assert False, 'action = %s' % action

print 'parsing'
for i in parse(u'(SS)'):
#for i in parse(u'0+1*0'):
    print i
