#include "read.h"

#include "io.h"
#include "print.h"			/* XXX */
#include "roots.h"
#include "scan.h"
#include "test.h"
#include "types.h"

#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#undef DUMP_TABLES

#if 0
build nonterminals;
count terminals including $ and -;
assert(1 << terminal_count > 1 << terminal_count / 2);
assert(1 << terminal_count < USHRT_MAX);
typedef uint16_t terminal_set_t;

terminal_set_t sym_first   [sizeof nonterminals];
terminal_set_t follow      [sizeof grammar];
build parse table [sizeof nonterminals * sizeof terminals];

/*
 * Indices.
 * symbols = terminals + {$, epsilon} + nonterminals.
 * sym_first is indexed by [symbols].
 * parse_table is indexed by [nonterminals] X [terminals + {$}].
 * follow is indexed by [nonterminals].
 * terminal_set_t is indexed by [terminals + {$, epsilon}].
 *
 * terminals_size = count of [terminals + {$}].
 * exterminals_size = count of [terminals + {$, epsilon}] = terminals_size + 1.
 *
 * symbol indices: terminals come first, then $, then epsilon, then nonterminals.
 * Each terminal (and $ and epsilon) has both a symbol index and a terminal index.
 * Each nonterminal has both a symbol index and a nonterminal index.
 * sym_index maps any symbol char to its symbol index.
 * term_index maps a terminal char, $ or epsilon to its terminal index.
 * nonterm_index maps a nonterminal to its nonterminal index.
 *
 * A terminal's terminal index == its symbol index.
 */

#endif

/*
 * Declare an array with the given base type, name, and size.
 *
 * Also define three functions.
 *
 * alloc_foo(n) allocates the next n available elements and returns its index.
 * next_foo(n) allocates the next n available elements and returns its address.
 *
 * Also defines the variable foo_size which holds the current number
 * of elements allocated.
 */

#ifdef NDEBUG
  #define IF_ASSERTIONS(x)
#else
  #define IF_ASSERTIONS(x) x
#endif

#define SIZED_ARRAY(type, var, size)					\
    static type var[(size)];						\
    static size_t var##_size = 0;					\
									\
    static size_t alloc_##var(size_t n)					\
    {									\
	size_t index = var##_size;					\
	var##_size += n;						\
	assert(var##_size <= (size));					\
	return index;							\
    }									\
									\
    static inline type *next_##var(size_t n)				\
    {									\
        return &var[alloc_##var(n)];					\
    }									\
									\
    IF_ASSERTIONS(							\
	__attribute__((destructor))					\
	static void verify_size_##var(void)				\
	{								\
	    if (var##_size != (size)) {					\
		fprintf(stderr, __FILE__ ":%d: warning: "		\
				"array `" #var "' allocated %d, "	\
				 "used %zd %s.\n",			\
				 __LINE__, (size), var##_size,		\
				 var##_size == 1 ? "entry" : "entries");\
	    }								\
	}								\
    )

typedef unsigned char uint8_t;
#define UINT8_MAX UCHAR_MAX
typedef unsigned short uint16_t;
#define UINT16_MAX USHRT_MAX

static bool build(bool init, obj_t *actions, obj_t **obj_out);

ROOT(ACTION_BEGIN_LIST);
ROOT(ACTION_BEGIN_VECTOR);
ROOT(ACTION_BEGIN_BYTEVEC);
ROOT(ACTION_END_SEQUENCE);
ROOT(ACTION_DOT_END);
ROOT(ACTION_ABBREV);
ROOT_CONSTRUCTOR(ACTION_DISCARD)
{
    build(true, NIL, NULL);		/* Initialize action roots. */
    return ACTION_DISCARD;
}

typedef enum char_type {
    CT_NONE = 0,
    CT_TERMINAL = 0x40,
    CT_NONTERMINAL = 0x80,
    CTMASK = 0xC0,
    SYMMASK = ~(CTMASK)
} char_type_t;

typedef uint16_t exterminal_set_t;

typedef struct production {
    char        p_lhs;
    const char *p_rhs;
    obj_t     **p_action;
} production_t;

typedef struct token_pair {
    char         tm_term;
    token_type_t tm_ttype;
} token_pair_t;

#if 0
/* This is the original YACC grammar. */

%%

program  :				comment program
         |				datum { YYACCEPT; }
         |				/* empty */
         ;

datum    :				EXACT_NUMBER { emit($1); }
         |				SIMPLE       { emit($1); }
         | { emit(OPEN_LIST); }		'(' sequence ')'
         | { emit(OPEN_LIST); }		'[' sequence ']'
         | { emit(OPEN_VECTOR); }	BEGIN_VECTOR elements ')'
         | { emit(OPEN_BYTEVECTOR); }	BEGIN_BYTEVECTOR bytes ')'
         | { emit(OPEN_ABBREV); }	ABBREV { emit($2); } datum
         ;

sequence :				datum tail
         |				comment sequence
         | { emit(CLOSE_SEQ); }		/* empty */
         ;

tail     :				datum tail
         |				comment tail
         | { emit(DOT_CLOSE); }		'.' comments datum comments
         | { emit(CLOSE_SEQ); }		/* empty */
         ;

elements :				datum elements
         |				comment elements
         | { emit(CLOSE_SEQ); }		/* empty */
         ;

bytes    :                              EXACT_NUMBER { emit($1); } bytes
         |                              comment bytes
         | { emit(CLOSE_SEQ); }	  /* empty */
         ;

comments :                              comment comments
	 |                              /* empty */
         ;

comment  : { emit(DISCARD); }           COMMENT datum
         ;

%%
#endif

/*
 * The following initializer declares the same grammar as the YACC
 * code above with some loss of readability.
 *
 * Here's the substitution table.
 *
 *  Terminals
 *	( = begin list
 *	) = end
 *	[ = alt begin list
 *	] = alt end list
 *	. = period
 *	V = begin vector
 *	B = begin bytevector
 *	N = exact number
 *	S = simple datum (e.g., symbol, character, string, inexact number)
 *	A = abbrev (e.g., quote, quasisyntax)
 *	; = comment, (#;)
 *
 *  Nonterminals
 *	p = program
 *	d = datum
 *	i = sequence (aka list interior)
 *      j = tail (list interior after 1st element)
 *	e = elements (vector interior)
 *	b = bytes (bytevector interior)
 *	x = comment
 *	z = comments
 */

static const production_t grammar[] = {
    { 'p', "xp",                         },
    { 'p', "d",                          },
    { 'p', "",                           },

    { 'd', "N",                          },
    { 'd', "S",                          },
    { 'd', "(i)",  &ACTION_BEGIN_LIST    },
    { 'd', "[i]",  &ACTION_BEGIN_LIST    },
    { 'd', "Ve)",  &ACTION_BEGIN_VECTOR  },
    { 'd', "Bb)",  &ACTION_BEGIN_BYTEVEC },
    { 'd', "Ad",   &ACTION_ABBREV        },

    { 'i', "dj",                         },
    { 'i', "xi",                         },
    { 'i', "",     &ACTION_END_SEQUENCE  },

    { 'j', "dj",                         },
    { 'j', "xj",                         },
    { 'j', ".ydy", &ACTION_DOT_END       },
    { 'j', "",     &ACTION_END_SEQUENCE  },

    { 'e', "de",                         },
    { 'e', "xe",                         },
    { 'e', "",     &ACTION_END_SEQUENCE  },

    { 'b', "Nb",                         },
    { 'b', "xb",                         },
    { 'b', "",     &ACTION_END_SEQUENCE  },

    { 'x', ";d",   &ACTION_DISCARD       },

    { 'y', "xy",                         },
    { 'y', "",                           },
};
static const size_t grammar_size = sizeof grammar / sizeof *grammar;

/*
 * token_pairs maps between the grammar's token symbols
 * and the token_type_t values that yylex() returns.
 */
static token_pair_t token_pairs[] = {
    { 'N', TOK_EXACT_NUMBER },
    { 'S', TOK_SIMPLE },
    { 'A', TOK_ABBREV },
    { ';', TOK_COMMENT },
    { 'V', TOK_BEGIN_VECTOR },
    { 'B', TOK_BEGIN_BYTEVECTOR },
    { '(', TOK_LPAREN },
    { ')', TOK_RPAREN },
    { '.', TOK_PERIOD },
    { '[', TOK_LBRACKET },
    { ']', TOK_RBRACKET },
    { '$', TOK_EOF },
};
static size_t token_pairs_size = sizeof token_pairs / sizeof *token_pairs;

static const uint8_t NO_RULE = UINT8_MAX;

static char start_symbol;
static uint8_t charmap[256];
static const size_t charmap_size = sizeof charmap / sizeof *charmap;
static size_t terminals_size;
static size_t epsilon;
static size_t exterminals_size;
static size_t nonterminals_size;

#define NT   12				/* number of terminal symbols */
#define NXT (NT + 1)			/* number of terminal symbols */
#define NN    8				/* number of nonterminal symbols */
#define NS  (NXT + NN)			/* number of symbols */
#define NPE (NT * NN)			/* number of parsing table entries */

SIZED_ARRAY(char,             symbols,           NS);
SIZED_ARRAY(exterminal_set_t, sym_first,         NS);
SIZED_ARRAY(exterminal_set_t, follow,            NN);
SIZED_ARRAY(uint8_t,          parsing_table,     NPE);

static inline bool char_is_nonterminal(char c)
{
    return (charmap[(uint8_t)c] & CTMASK) == CT_NONTERMINAL;
}

/*
 * Initialize charmap and symbols.
 * symbols maps symbol indices to ASCII chars (not Unicode).
 * charmap maps unsigned chars to symbol indices.
 */

static void init_symbols(void)
{
    size_t i, j;
    const char *p;

    start_symbol = grammar[0].p_lhs;
    for (i = 0; i < charmap_size; i++)
	charmap[i] = CT_NONE;
    for (i = 0; i < grammar_size; i++)
	charmap[(uint8_t)grammar[i].p_lhs] = CT_NONTERMINAL;
    for (i = 0; i < grammar_size; i++)
	for (p = grammar[i].p_rhs; *p; p++)
	    if (charmap[(uint8_t)*p] == CT_NONE)
		charmap[(uint8_t)*p] = CT_TERMINAL;
    assert(charmap['$'] == CT_NONE);
    charmap['$'] = CT_TERMINAL;
    assert(charmap['-'] == CT_NONE);
    for (i = j = 0; i < token_pairs_size; i++, j++) {
	token_pair_t *tpp = &token_pairs[i];
	assert(tpp->tm_ttype < token_pairs_size);
	uint8_t *cmp = &charmap[(uint8_t)tpp->tm_term];
	assert(*cmp == CT_TERMINAL);
	*cmp |= tpp->tm_ttype;
	*next_symbols(1) = tpp->tm_term;
    }
    terminals_size = symbols_size;
    charmap['-'] = CT_TERMINAL | j++;
    epsilon = symbols_size;
    *next_symbols(1) = '-';
    exterminals_size = symbols_size;
    for (i = 0; i < charmap_size; i++)
	if (charmap[i] == CT_NONTERMINAL) {
	    charmap[i] |= j++;
	    *next_symbols(1) = i;
	}
    nonterminals_size = symbols_size - exterminals_size;
    assert(symbols_size < CTMASK);

#ifdef DUMP_TABLES
    printf("start_symbol = %c\n", start_symbol);
    printf("terminals_size = %d\n", terminals_size);
    printf("exterminals_size = %d\n", exterminals_size);
    printf("nonterminals_size = %d\n", nonterminals_size);
    printf("symbols_size = %d\n", symbols_size);
    printf("epsilon = %d\n", epsilon);
    printf("charmap\n");
    for (i = 0; i < charmap_size; i++)
	if (charmap[i])
	    printf("   ['%c'] = 0x%x\n", i, charmap[i]);
    printf("\n");

    printf("symbols\n");
    for (i = 0; i < symbols_size; i++)
	printf("   %2d: %c\n", i, symbols[i]);
    printf("\n");
#endif
}

static inline size_t sym_index(char sym)
{
    uint8_t cm = charmap[(size_t)sym];
    assert(cm & CTMASK);
    return cm & SYMMASK;
}

static inline size_t term_index(char term)
{
    uint8_t cm = charmap[(size_t)term];
    assert(cm & CT_TERMINAL);
    return cm & SYMMASK;
}

static inline size_t nonterm_index(char nonterm)
{
    uint8_t cm = charmap[(size_t)nonterm];
    assert((cm & CTMASK) == CT_NONTERMINAL);
    return (cm & SYMMASK) - exterminals_size;
}

static inline char terminal(size_t term_index)
{
    assert(term_index < terminals_size);
    return symbols[term_index];
}

static inline char nonterminal(size_t nonterm_index)
{
    assert(nonterm_index < nonterminals_size);
    return symbols[nonterm_index + exterminals_size];
}

/*
 * sym_first maps each symbol (including $ and epsilon) to its
 * "first-set", the set of terminals (plus epsilon) that may be first
 * in a string derived from the symbol.
 *
 * A terminal's first-set just contains the terminal.
 *
 * A nonterminal's first-set is constructed by repeatedly
 * applying productions to the symbol, finding all terminals
 * that can be reached.
 */

static void init_first(void)
{
    size_t i, j;
    for (i = 0; i < exterminals_size; i++)
	*next_sym_first(1) = 1 << i;
    for (i = 0; i < nonterminals_size; i++)
	*next_sym_first(1) = 0;
    for (i = 0; i < grammar_size; i++)
	if (!*grammar[i].p_rhs)
	    sym_first[sym_index(grammar[i].p_lhs)] |= 1 << epsilon;
    bool done = false;
    while (!done) {
	done = true;
	for (i = 0; i < grammar_size; i++) {
	    const production_t *pp = &grammar[i];
	    exterminal_set_t *x = &sym_first[sym_index(pp->p_lhs)];
	    for (j = 0; pp->p_rhs[j]; j++) {
		exterminal_set_t *y = &sym_first[sym_index(pp->p_rhs[j])];
		if (*y & ~*x) {
		    exterminal_set_t b = *x;
		    *x |= *y;
		    exterminal_set_t a = *x;
		    assert(b != a);
		    assert(a & ~b);
		    done = false;
		}
		if (!(*y & 1 << epsilon))
		    break;
	    }
	}
    }

#ifdef DUMP_TABLES
    printf("sym_first\n");
    for (i = 0; i < symbols_size; i++) {
	printf("  %c:", symbols[i]);
	exterminal_set_t s = sym_first[i];
	for (j = 0; j < exterminals_size; j++)
	    if (s & 1 << j)
		printf(" %c", symbols[j]);
	    printf("\n");
    }
    printf("\n");

    printf("first\n");
    exterminal_set_t first_(const char *w);
    for (i = 0; i < grammar_size; i++) {
	const production_t *pp = &grammar[i];
	printf("   %c=%-5s:", pp->p_lhs, pp->p_rhs);
	exterminal_set_t f = first_(pp->p_rhs);
	for (j = 0; j < exterminals_size; j++)
	    if (f & 1 << j)
		printf(" %c", symbols[j]);
	    printf("\n");
	
    }
    printf("\n");
#endif
}

static exterminal_set_t first(const char *w)
{
    const char *p;
    exterminal_set_t f = 1 << epsilon;
    for (p = w; *p; p++) {
	exterminal_set_t yf = sym_first[sym_index(*p)];
	f |= yf;
	if (!(yf & 1 << epsilon)) {
	    f &= ~(1 << epsilon);
	    break;
	}
    }
    return f;
}

exterminal_set_t first_(const char *w)
{
    return first(w);
}

static void init_follow(void)
{
    int i, j;
    const char *p;

    for (i = 0; i < nonterminals_size; i++)
	*next_follow(1) = 0;
    follow[nonterm_index(start_symbol)] = 1 << sym_index('$');
    for (i = 0; i < grammar_size; i++)
	for (p = grammar[i].p_rhs; *p; p++) {
	    char b = *p;
	    const char *beta = p + 1;
	    if (char_is_nonterminal(b))
		follow[nonterm_index(b)] |= first(beta) & ~(1 << epsilon);
	}
    bool done = false;
    while (!done) {
	done = true;
	for (i = 0; i < grammar_size; i++) {
	    const production_t *pp = &grammar[i];
	    char a = pp->p_lhs;
	    exterminal_set_t *fa = &follow[nonterm_index(a)];
	    for (j = strlen(pp->p_rhs); --j >= 0; ) {
		char b = pp->p_rhs[j];
		const char *beta = pp->p_rhs + j + 1;
		if (!(first(beta) & 1 << epsilon))
		    break;
		if (char_is_nonterminal(b)) {
		    exterminal_set_t *fb = &follow[nonterm_index(b)];
		    if (*fa & ~*fb) {
			done = false;
			*fb |= *fa;
		    }
		}
	    }
	}
    }

#ifdef DUMP_TABLES
    printf("follow\n");
    for (i = 0; i < nonterminals_size; i++) {
	printf("   %c:", nonterminal(i));
	for (j = 0; j < symbols_size; j++)
	    if (follow[i] & 1 << j)
		printf(" %c", symbols[j]);
	printf("\n");
    }
    printf("\n");
#endif
}

static int pt_entry(char A, char a)
{
    int found = NO_RULE;
    size_t ia = sym_index(a);
    exterminal_set_t foA = follow[nonterm_index(A)];
    bool a_follows_A = (bool)(foA & 1 << ia);
    size_t i;
    for (i = 0; i < grammar_size; i++) {
	const production_t *pp = &grammar[i];
	if (pp->p_lhs == A) {
	    exterminal_set_t fig = first(pp->p_rhs);
	    if (fig & 1 << ia) {
		assert(found == NO_RULE && "grammar not LL(1)");
		found = i;
	    }
	    if (a_follows_A && (fig & 1 << epsilon)) {
		assert(found == NO_RULE && "grammar not LL(1)");
		found = i;
	    }		
	}
    }
    return found;
}

static uint8_t parsing_table_entry(size_t i, size_t j)
{
    assert(i < nonterminals_size);
    assert(j < terminals_size);
    return parsing_table[i * terminals_size + j];
}

/* parsing table maps nonterminal X terminal -> nonterminal */

/*
 * I have a token type.  I need a terminal index.
 */

static void init_parsing_table(void)
{
    size_t i, j;

    for (i = 0; i < nonterminals_size; i++) {
	char A = nonterminal(i);
	for (j = 0; j < terminals_size; j++) {
	    char a = terminal(j);
	    *next_parsing_table(1) = pt_entry(A, a);
	}
    }
    
#ifdef DUMP_TABLES
    printf("parsing table\n");
    printf("    ");
    for (j = 0; j < TOKEN_TYPE_COUNT; j++)
	printf("%3c", terminal(j));
    printf("\n");
    printf("    ");
    for (j = 0; j < terminals_size; j++)
	printf("---");
    printf("\n");
    for (i = 0; i < nonterminals_size; i++) {
	printf("%3c :", nonterminal(i));
	for (j = 0; j < terminals_size; j++) {
	    uint8_t e = parsing_table_entry(i, j);
	    if (e == NO_RULE)
		printf(" - ");
	    else
		printf("%3d", e);
	}
	printf("\n");
    }

    printf("\n");
#endif    
}

static uint8_t get_rule(char symbol, size_t term)
{
    uint8_t rule = NO_RULE;
    if (char_is_nonterminal(symbol))
	rule = parsing_table_entry(nonterm_index(symbol), term);
    //printf("get_rule(%c, %c) => %d\n", symbol, terminal(term), rule);
    return rule;
}
static obj_t *parse(instream_t *in)
{
    AUTO_ROOT(actions, NIL);
    AUTO_ROOT(yylval, NIL);
    AUTO_ROOT(tmp, make_fixnum(TOK_EOF));
    AUTO_ROOT(stack, make_pair(tmp, NIL));
    tmp = make_fixnum(sym_index(start_symbol));
    stack = make_pair(tmp, stack);
    int tok = yylex(&yylval, in);
    while (true) {
	int i = fixnum_value(pair_car(stack));
	//printf("i = %d = %c\n", i, symbols[i]);
	assert(0 <= i && i < symbols_size);
	uint8_t rule = get_rule(symbols[i], tok);
	if (rule != NO_RULE) {
	    const production_t *pp = &grammar[rule];
	    stack = pair_cdr(stack);
	    int j;
	    for (j = strlen(pp->p_rhs); --j >= 0; ) {
		tmp = make_fixnum(sym_index(pp->p_rhs[j]));
		stack = make_pair(tmp, stack);
	    }
	    if (pp->p_action)
		actions = make_pair(*pp->p_action, actions);
	    
	} else {
	    //printf("no rule, stack = "); print_stdout(stack);
	    int sym = fixnum_value(pair_car(stack));
	    stack = pair_cdr(stack);
	    //printf("sym = %d = %c\n", sym, symbols[sym]);
	    if (sym == TOK_EOF)
		break;
	    assert(sym == tok && "syntax error");
	    if (yylval)
		actions = make_pair(yylval, actions);
	    if (actions && !pair_cdr(stack))
		break;
	    yylval = NIL;
	    tok = yylex(&yylval, in);
	}
    }
    POP_FUNCTION_ROOTS();
    return actions;
}

static obj_t *build_vector(obj_t *list)
{
    PUSH_ROOT(list);
    obj_t *p = list;
    size_t i, size = 0;
    do
	size++;
    while ((p = pair_cdr(p)));
    AUTO_ROOT(vec, make_vector(size, NIL));
    for (i = 0, p = list; i < size; i++) {
	vector_set(vec, i, pair_car(p));
	p = pair_cdr(p);
    }
    POP_FUNCTION_ROOTS();
    return vec;
}

static obj_t *build_bytevec(obj_t *list)
{
    assert(false && "XXX implement bytevectors");
}

static bool build(bool init, obj_t *actions, obj_t **obj_out)
{
    if (init) {
	ACTION_BEGIN_LIST    = make_C_procedure(&&begin_list,       NIL, NIL);
	ACTION_BEGIN_VECTOR  = make_C_procedure(&&begin_vector,     NIL, NIL);
	ACTION_BEGIN_BYTEVEC = make_C_procedure(&&begin_bytevector, NIL, NIL);
	ACTION_ABBREV        = make_C_procedure(&&abbrev,           NIL, NIL);
	ACTION_END_SEQUENCE  = make_C_procedure(&&end_sequence,     NIL, NIL);
	ACTION_DOT_END       = make_C_procedure(&&dot_end,          NIL, NIL);
	ACTION_DISCARD       = make_C_procedure(&&discard,          NIL, NIL);
	return false;
    }
    PUSH_ROOT(actions);
    AUTO_ROOT(vstack, NIL);
    AUTO_ROOT(reg, NIL);
    AUTO_ROOT(tmp, NIL);
    while (actions) {
	obj_t *op = pair_car(actions);
	actions = pair_cdr(actions);
	if (is_procedure(op) && procedure_is_C(op))
	    goto *procedure_body(op);

 /* default: */
	reg = make_pair(op, reg);
	continue;

    begin_list:
	reg = make_pair(reg, pair_car(vstack));
	vstack = pair_cdr(vstack);
	continue;

    begin_vector:
	reg = build_vector(reg);
	reg = make_pair(reg, pair_car(vstack));
	vstack = pair_cdr(vstack);
	continue;

    begin_bytevector:
	reg = build_bytevec(reg);
	reg = make_pair(reg, pair_car(vstack));
	vstack = pair_cdr(vstack);
	continue;

    abbrev:
	tmp = make_pair(pair_car(pair_cdr(reg)), NIL);
	tmp = make_pair(pair_car(reg), tmp);
	reg = make_pair(tmp, pair_cdr(pair_cdr(reg)));
	continue;

    end_sequence:
	vstack = make_pair(reg, vstack);
	reg = NIL;
	continue;

    dot_end:
	vstack = make_pair(pair_cdr(reg), vstack);
	reg = pair_car(reg);
	continue;

    discard:
	reg = pair_cdr(reg);
	continue;
    }
    assert(vstack == NIL);

    bool success = false;
    if (reg) {
	assert(pair_cdr(reg) == NIL);
	*obj_out = pair_car(reg);
	success = true;
    }
    POP_FUNCTION_ROOTS();
    return success;
}

bool read_stream(instream_t *in, obj_t **obj_out)
{
    return build(false, parse(in), obj_out);
}

__attribute__((constructor))
static void init_parser(void)
{
    init_symbols();
    init_first();
    init_follow();
    init_parsing_table();
}

/* lists */
TEST_READ(L"(a b)",			L"(a b)");
TEST_EVAL(L"(pair? '(a b))",		L"#t");
TEST_READ(L"[a b]",			L"(a b)");
TEST_EVAL(L"(pair? '[a b])",		L"#t");

/* vectors */
TEST_READ(L"#(a b)",			L"#(a b)");
TEST_READ(L"#(a (b c))",		L"#(a (b c))");
TEST_READ(L"#(a #(b c))",		L"#(a #(b c))");

/* bytevectors */
//TEST_READ(L"#vu8(1 2)",			L"#vu8(1 2)");

/* abbreviations */
TEST_READ(L"'a",			L"(quote a)");
TEST_READ(L"'(a b)",			L"(quote (a b))");
TEST_READ(L"#('a '(a b))",		L"#((quote a) (quote (a b)))");
TEST_READ(L"('a b c)",			L"((quote a) b c)");
TEST_READ(L"(a 'b c)",			L"(a (quote b) c)");
TEST_READ(L"(a b 'c)",			L"(a b (quote c))");
TEST_READ(L"'''a",                      L"(quote (quote (quote a)))");
TEST_READ(L"`a",			L"(quasiquote a)");
TEST_READ(L",a",			L"(unquote a)");
TEST_READ(L",@a",			L"(unquote-splicing a)");
TEST_READ(L"#'a",			L"(syntax a)");
TEST_READ(L"#`a",			L"(quasisyntax a)");
TEST_READ(L"#,a",			L"(unsyntax a)");
TEST_READ(L"#,@a",			L"(unsyntax-splicing a)");

/* comments */
TEST_READ(L"#; asdf ghjk",		L"ghjk");
TEST_READ(L"(a#;()b)",                  L"(a b)");
TEST_READ(L"(a#;(comment)b)",           L"(a b)");
TEST_READ(L"(a#;(\n)b)",                L"(a b)");
TEST_READ(L"(a#;\t()b)",                L"(a b)");
TEST_READ(L"(a#;((c)(d))b)",            L"(a b)");
TEST_READ(L"(#;c a . b)",		L"(a . b)");
TEST_READ(L"(#;c#;c a . b)",		L"(a . b)");
TEST_READ(L"(a#;c . b)",		L"(a . b)");
TEST_READ(L"(a #;c#;c . b)",		L"(a . b)");
TEST_READ(L"(a #;c#;c . #;c b)",	L"(a . b)");
TEST_READ(L"(a #;c . #;c #;c b)",	L"(a . b)");
TEST_READ(L"(a #;c#;c . #;c b #;c)",	L"(a . b)");
TEST_READ(L"(a . #;c#;c b#;c#;c)",	L"(a . b)");
TEST_READ(L"(a#;c . #;c#;c b#;c#;c)",	L"(a . b)");
TEST_READ(L"(a . #;()#;() b#;()#;())",	L"(a . b)");
TEST_READ(L"(a#!r6rs b)",		L"(a b)");
TEST_READ(L"#!r6rs(a b)",		L"(a b)");
TEST_READ(L"(#!r6rs a b)",		L"(a b)");
TEST_READ(L"(#!r6\x33s a b)",		L"(a b)");
