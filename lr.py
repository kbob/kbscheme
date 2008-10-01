import re

# Grammar symbols are single chars. '=' and '.' are reserved.
# Augment the grammar manually. (i.e., start with 'S=s'.)

grammar = (
    's=f',
    'f=x',
    'f=(l)',
    'l=',
    'l=lf',
    )

nonterminals = set(rule[0] for rule in grammar)
print 'nonterminals', nonterminals
terminals = set(ch for ch in ''.join(grammar)
                if ch not in nonterminals | set('='))
print 'terminals', terminals
symbols = nonterminals | terminals | set('$')
print 'symbols', symbols

rule_0 = set([grammar[0].replace('=', '=.')])
print 'rule 0', rule_0

def close(item_set):
    closure = set(item_set)
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
    return closure

state_0 = close(rule_0)
import pprint
print 'state 0'
pprint.pprint(sorted(state_0), indent=4)
assert state_0 == close(state_0)


def advance(item_set):
    new_sets = {}
    for item in item_set:
        m = re.search(r'\.(.)', item)
        if not m:
            continue
        sym = m.group(1)
        new_item = re.sub(r'\.(.)', r'\1.', item)
        new_sets.setdefault(sym, set()).add(new_item)
    for sym in new_sets:
        new_sets[sym] = close(new_sets[sym])
    return new_sets

import pprint
print 'advance from state 0'
pprint.pprint(advance(state_0), indent=4)
print


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
    item_0 = grammar[0].replace('=', '=.')
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
states, tt = transition_table(grammar)
print 'states'
pprint.pprint(states)
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
            if item.endswith('.'):
                for i, rule in enumerate(grammar):
                    if i and rule == item[:-1]:
                        yield 'r%d' % i
    def actions(state, row):
        a = terminal_columns(row)
        if accept_item in state:
            a['$'] = 'a0'
        for reduction in reduce_states(state):
            if a:
                print 'conflict state', state
                print '           row', row
                print '        reduce', reduction
                print '        action', a
                print
            assert not a, 'parser conflict detected'
            for sym in terminals:
                if sym not in a:
                    a[sym] = reduction
            a = dict((sym, reduction) for sym in symbols)
        return a
    accept_item = grammar[0] + '.'
    return [actions(state, row) for state, row in zip(states, tt)]

print 'action table'
actions = action_table(states, tt)
pprint.pprint(actions)

def parse(input):
    input = input + '$'
    stack = [0]
    while True:
        action = actions[stack[-1]][input[0]]
        v, n = action[0], int(action[1:])
        if v == 's':
            term, input = input[0], input[1:]
            stack.append(n)
            print 'shift %s %s' % (action, term)
        elif v == 'r':
            rhl = len(grammar[n]) - 2
            stack, popped = stack[:-rhl], stack[-rhl:]
            print 'reduce %s' % (action, )
            yield grammar[n], popped
            stack.append(goto[stack[-1]][grammar[n][0]])
        elif v == 'a':
            yield stack.pop()
            print 'final stack', stack
            break
        else:
            assert False, 'action = %s' % action
        

def xxparse(input):
    input += '$'
    stack = []
    while True:
        ch = input[0]
        if ch in 'LV[':
            stack.append(ch)
            input = input[1:]
        elif ch == '[':
            stack.append(ch)
        elif ch == ']':
            pass

print 'parsing'
#for i in parse('0+1*0'):
#    print i
for i in parse('(xx)'):
    print i
