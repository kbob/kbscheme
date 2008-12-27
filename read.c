#include "read.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

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
    type *next_##var(size_t n)						\
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

typedef enum action {
    A_ACCEPT,				/* final: parse successful */
    A_EOF,				/* final: reached end of input */
    A_NIL,				/* NIL */
    A_TAKE_1st,				/* 1st sym of RHS */
    A_TAKE_2nd,				/* 2nd of RHS */
    A_CONS_1_2,				/* cons(1st, 2nd) */
    A_CONS_1_3,				/* cons(1st, 3rd) */
    A_VEC_2nd,				/* vector(2nd) */
    A_BYTVEC_2nd,			/* bytevector(2nd) */
    A_ABBR_1_2,				/* (1st 2nd) */
} action_t;

typedef enum char_type {
    CT_NONE = 0,
    CT_TERMINAL = 0x40,
    CT_NONTERMINAL = 0x80,
    CT_MARKER = 0xC0,
    CTMASK = 0xC0,
    SYMMASK = ~(CTMASK)
} char_type_t;

typedef uint8_t bitset_word_t;
typedef bitset_word_t *bitset_t;
typedef const bitset_word_t *const_bitset_t;
#define BITS_PER_WORD 8
#define BITSET_WORDS(size) (((size) + BITS_PER_WORD - 1) / BITS_PER_WORD)

typedef struct production {
    char        p_lhs;
    const char *p_rhs;
    action_t    p_action;
} production_t;

typedef struct item {
    uint8_t     i_prod;
    uint8_t     i_pos;
} item_t;

typedef bitset_t item_set_t;

typedef size_t transition_t;
#define NULL_TRANSITION (~(size_t)0)
#define TRANSITION(set, sym) transitions[(set) * symbols_size + (sym)]

typedef struct actionX {
} actionX_t;

typedef struct goto_ {
} goto_t;

/* This is the original YACC grammar. */
#if 0
%%
program  : comment program
         | datum			{ *opp = $1; YYACCEPT;    }
         | /* empty */			{ *reached_eof = true;    }
         ;

datum    : simple
         | compound
         ;

simple   : EXACT_NUMBER
         | SIMPLE
         ;

compound : '(' sequence ')'		{ $$ = $2;                }
         | '[' sequence ']'		{ $$ = $2;                }
         | BEGIN_VECTOR elements ')'	{ $$ = build_vector($2);  }
         | BEGIN_BYTEVECTOR bytes ')'	{ $$ = build_bytevec($2); }
         | ABBREV datum			{ $$ = make_abbr($1, $2); }
         ;

sequence : datum sequence		{ $$ = make_pair($1, $2); }
         | comment sequence		{ $$ = $2;                }
         | datum '.' datum		{ $$ = make_pair($1, $3); }
         | /* empty */			{ $$ = NIL;               }
         ;

elements : datum elements		{ $$ = make_pair($1, $2); }
         | comment elements		{ $$ = $2;                }  
         | /* empty */			{ $$ = NIL;               }
         ;

bytes    : EXACT_NUMBER bytes		{ $$ = make_pair($1, $2); }
         | comment bytes		{ $$ = $2;                }
         | /* empty */			{ $$ = NIL;               }
         ;

comment  : COMMENT datum
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
 *	V = begin vector
 *	B = begin bytevector
 *	D = dot
 *	N = exact number
 *	S = simple datum (e.g., symbol, character, string, inexact number)
 *	A = abbrev (e.g., quote, quasisyntax)
 *	C = comment, (#;)
 *
 *  Nonterminals
 *	Z = start
 *	p = program
 *	d = datum
 *	s = simple
 *	c = compound
 *	i = sequence (aka interior)
 *	e = elements
 *	b = bytes
 *	x = comment
 */

static const production_t grammar[] = {
    { 'Z', "p",   A_TAKE_1st   },	/* initial production added */

    { 'p', "xp",  A_TAKE_1st   },
    { 'p', "d",   A_ACCEPT     },
    { 'p', "",    A_EOF        },

    { 'd', "s",   A_TAKE_1st   },
    { 'd', "c",   A_TAKE_1st   },

    { 's', "N",   A_TAKE_1st   },
    { 's', "S",   A_TAKE_1st   },

    { 'c', "(i)", A_TAKE_2nd   },
    { 'c', "[i]", A_TAKE_2nd   },
    { 'c', "Ve)", A_VEC_2nd    },
    { 'c', "Bb)", A_BYTVEC_2nd },
    { 'c', "Ad",  A_ABBR_1_2   },

    { 'i', "di",  A_CONS_1_2   },
    { 'i', "xi",  A_TAKE_2nd   },
    { 'i', "dDd", A_CONS_1_3   },
    { 'i', "",    A_NIL        },

    { 'e', "de",  A_CONS_1_2   },
    { 'e', "xe",  A_TAKE_2nd   },
    { 'e', "",    A_NIL        },

    { 'b', "Nb",  A_CONS_1_2   },
    { 'b', "xb",  A_TAKE_2nd   },
    { 'b', "",    A_NIL        },

    { 'x', "Cd",  A_NIL        },
};
static const size_t grammar_size = sizeof grammar / sizeof *grammar;

static uint8_t charmap[256];
static const size_t charmap_size = sizeof charmap / sizeof *charmap;

SIZED_ARRAY(char,          symbols,      21);
SIZED_ARRAY(item_t,        items,        63);
SIZED_ARRAY(bitset_word_t, bitsets,     312);
SIZED_ARRAY(item_set_t,    item_sets,    39);
SIZED_ARRAY(transition_t,  transitions, 39 * 21);
SIZED_ARRAY(action_t,      actions,     100);
SIZED_ARRAY(goto_t,        gotos,       100);

static inline bool char_is_nonterminal(char c)
{
    return (charmap[(uint8_t)c] & CTMASK) == CT_NONTERMINAL;
}

static inline void bitset_clr_all(bitset_t set, size_t size)
{
    size_t i, alloc_size = BITSET_WORDS(size);
    for (i = 0; i < alloc_size; i++)
	set[i] = 0;
}

static inline void bitset_set_bit(size_t bit, bitset_t set)
{
    set[bit / BITS_PER_WORD] |= 1 << bit % BITS_PER_WORD;
}

static inline void bitset_clr_bit(size_t bit, bitset_t set)
{
    set[bit / BITS_PER_WORD] &= ~(1 << bit % BITS_PER_WORD);
}

static inline bool bitset_is_set(size_t bit, const_bitset_t set)
{
    return set[bit / BITS_PER_WORD] & 1 << bit % BITS_PER_WORD ? true : false;
}

static inline bool bitset_next_set(size_t *bitp,
				   const_bitset_t set,
				   size_t size)
{
    size_t bit = *bitp + 1;
    if (bit > size)
	bit = 0;
    while (!bitset_is_set(bit, set))
	if (++bit >= size) {
	    *bitp = bit;
	    return false;
	}
    *bitp = bit;
    return true;
}

static inline bitset_t bitset_alloc(size_t size)
{
    bitset_t set = next_bitsets(BITSET_WORDS(size));
    bitset_clr_all(set, size);
    return set;
}

static inline bool bitsets_equal(const_bitset_t a,
				 const_bitset_t b,
				 size_t size)
{
    size_t i;
    for (i = 0; i < BITSET_WORDS(size); i++)
	if (a[i] != b[i])
	    return false;
    return true;
}

static inline void bitset_copy(bitset_t dest, const_bitset_t src, size_t size)
{
    size_t i;
    for (i = 0; i < BITSET_WORDS(size); i++)
	dest[i] = src[i];
}

static void init_symbols(void)
{
    size_t i, j;
    const char *p;

    for (i = 0; i < charmap_size; i++)
	charmap[i] = CT_NONE;
    for (i = 0; i < grammar_size; i++)
	charmap[(uint8_t)grammar[i].p_lhs] = CT_NONTERMINAL;
    for (i = 0; i < grammar_size; i++)
	for (p = grammar[i].p_rhs; *p; p++)
	    if (!charmap[(uint8_t)*p])
		charmap[(uint8_t)*p] = CT_TERMINAL;
    assert(charmap['$'] == CT_NONE);
    charmap['$'] = CT_MARKER;
    for (i = j = 0; i < charmap_size; i++)
	if (charmap[i] != CT_NONE) {
	    charmap[i] |= j++;
	    *next_symbols(1) = (char)i;
	}
    assert(!(symbols_size & CTMASK));
}

#if 0
#include <string.h> /* XXX */
static const char *item_repr(const item_t *item) /* XXX */
{
    static char buf[100];
    const production_t *prod = &grammar[item->i_prod];
    snprintf(buf, sizeof buf, "(%d) %c ::= ", item - items, prod->p_lhs);
    const char *p;
    char *q = buf + strlen(buf);
    size_t i;
    for (i = 0, p = prod->p_rhs; ; i++, p++) {
	    *q++ = ((i == item->i_pos) ? '^' : ' ');
	    if (!*p)
		break;
	    *q++ = *p;
	}
    *q = '\0';
    return buf;
}
#endif

static void print_note_XXX(const char *label, const item_set_t set)
{
    printf("%s [", label);
    const char *sep = "";
    size_t i = items_size;
    while (bitset_next_set(&i, set, items_size)) {
	printf("%s%d", sep, i);
	sep = " ";
    }
    printf("]\n");
}

static void close_item_set(item_set_t set)
{
    print_note_XXX("close", set);
    bitset_word_t unprocessed[BITSET_WORDS(items_size)];
    bitset_copy(unprocessed, set, items_size);
    bool done;
    do {
	done = true;
	size_t i = items_size;
	while (bitset_next_set(&i, unprocessed, items_size)) {
	    bitset_clr_bit(i, unprocessed);
	    const item_t *item = &items[i];
	    //printf("closing %s\n", item_repr(item));
	    const production_t *prod = &grammar[item->i_prod];
	    //printf("    item %d\n", i);
	    //printf("    i_prod = %d (%c=%s)\n",
	    //	   item->i_prod, prod->p_lhs, prod->p_rhs);
	    //printf("    i_pos = %d\n", item->i_prod);
	    char nextsym = prod->p_rhs[item->i_pos];
	    if (char_is_nonterminal(nextsym)) {
		//printf("nextsym = %c\n", nextsym);
		size_t j;
		for (j = 0; j < items_size; j++) {
		    if (!bitset_is_set(j, set)) {
			const item_t *item2 = &items[j];
			if (item2->i_pos == 0 &&
			    grammar[item2->i_prod].p_lhs == nextsym)
			{
			    //printf("adding %s\n", item_repr(item2));
			    bitset_set_bit(j, set);
			    bitset_set_bit(j, unprocessed);
			    done = false;
			}
		    }
		}
	    }
	    //printf("\n");
	}
    } while (!done);
    print_note_XXX("close returns", set);
}

static void init_items(void)
{
    size_t i, j;
    const char *p;

    for (i = 0; i < grammar_size; i++)
	for (j = 0, p = grammar[i].p_rhs; ; j++, p++) {
	    item_t *item = next_items(1);
	    item->i_prod = i;
	    item->i_pos  = j;
	    if (!*p)
		break;
	}
#if 0
    printf("items\n");
    for (i = 0; i < items_size; i++) {
	const item_t *item = &items[i];
	const production_t *prod = &grammar[item->i_prod];
	printf("    %2d   %c ::= ", i, prod->p_lhs);
	const char *p;
	for (j = 0, p = prod->p_rhs; ; j++, p++) {
	    putchar((j == item->i_pos) ? '^' : ' ');
	    if (!*p)
		break;
	    putchar(*p);
	}
	printf("\n");
    }
    printf("\n");
#endif
}

#if 0
static bool symbol_follows_item_set(char symbol, item_set_t set)
{
    size_t i;

    for (i = 0; i < items_size; i++) {
	const item_t *item = &items[i];
	if (grammar[item->i_prod].p_rhs[item->i_pos] == symbol)
	    return true;
    }
    return false;
}
#endif

static item_set_t alloc_item_set(void)
{
    /* XXX move this function higher in the file. */
    item_set_t *setp = next_item_sets(1);
    *setp = bitset_alloc(items_size);
    return *setp;
}

static void init_item_sets(void)
{
    size_t i, j, k;
    item_set_t set_0 = alloc_item_set();
    bitset_set_bit(0, set_0);
    close_item_set(set_0);
    (void)alloc_transitions(symbols_size);
    for (k = 0; k < symbols_size; k++)
	TRANSITION(0, k) = NULL_TRANSITION;
    for (i = 0; i < item_sets_size; i++) {
	item_set_t set = item_sets[i];
	/* XXX iterate symbols instead */
	for (j = 0; j < charmap_size; j++) {
	    if (charmap[j] != CT_NONE) {
		char symbol = (char)j;
		//printf("symbol %c\n", (char)j);
		size_t sym_index = charmap[j] & SYMMASK;
		bitset_word_t nset[BITSET_WORDS(items_size)];
		bitset_clr_all(nset, items_size);
		bool nset_is_empty = true;
		k = items_size;
		while (bitset_next_set(&k, set, items_size)) {
		    item_t *item = &items[k];
		    if (grammar[item->i_prod].p_rhs[item->i_pos] == symbol) {
			bitset_set_bit(k + 1, nset);
			nset_is_empty = false;
		    }
		}
		if (nset_is_empty) {
		    //printf("nset is empty.\n");
		    continue;
		}
		close_item_set(nset);
		transition_t dest = NULL_TRANSITION;
		for (k = 0; k < item_sets_size; k++)
		    if (bitsets_equal(item_sets[k], nset, items_size))
			dest = k;
		if (dest == NULL_TRANSITION) {
		    dest = item_sets_size;
		    item_set_t dest_set = alloc_item_set();
		    bitset_copy(dest_set, nset, items_size);
		    (void)alloc_transitions(symbols_size);
		    for (k = 0; k < symbols_size; k++)
			TRANSITION(dest, k) = NULL_TRANSITION;
		}
		TRANSITION(i, sym_index) = dest;
	    }
	}
    }
    assert(transitions_size == item_sets_size * symbols_size);
    for (i = 0; i < item_sets_size; i++) {
	item_set_t set = item_sets[i];
	printf("set %zd\n", i);
	size_t j = items_size;
	while (bitset_next_set(&j, set, items_size)) {
	    const item_t *item = &items[j];
	    const production_t *prod = &grammar[item->i_prod];
	    printf("    %2d   %c ::= ", j, prod->p_lhs);
	    size_t k;
	    const char *p;
	    for (k = 0, p = prod->p_rhs; ; k++, p++) {
		putchar((k == item->i_pos) ? '^' : ' ');
		if (!*p)
		    break;
		putchar(*p);
	    }
	    printf("\n");
	}
	printf("\n");
    }
    printf("transitions\n");
    printf("    %2s  ", "");
    for (j = 0; j < symbols_size; j++)
	printf("  %c", symbols[j]);
    printf("\n\n");
    for (i = 0; i < item_sets_size; i++) {
	printf("    %2d  ", i);
	for (j = 0; j < symbols_size; j++) {
	    transition_t t = TRANSITION(i, j);
	    if (t == NULL_TRANSITION)
		printf("  -");
	    else
		printf(" %2d", t);
	}
	printf("\n");
    }
    printf("\n");
}

static void init_actions(void)
{
}

static void init_goto(void)
{
}

__attribute__((constructor))
static void init_parser(void)
{
    init_symbols();
    init_items();
    init_item_sets();
    init_actions();
    init_goto();
}
