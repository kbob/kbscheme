/* Question: Can yacc/bison handle a right-recursive list? */
/* Answer:   Yes.  But it uses a lot of stack space. */

%{

    #include <assert.h>
    #include <stdio.h>
    #include <stdlib.h>

    typedef struct object object_t, *obptr_t;
    typedef enum bool { false = 0, true = 1 } bool;
    typedef char atom_t;
    typedef struct pair {
	object_t *p_car, *p_cdr;
    } pair_t;
    typedef enum obj_type { OT_ATOM, OT_PAIR } obj_type_t;
    struct object {
	obj_type_t o_type;
	union {
	    atom_t ou_atom;
	    struct pair ou_pair;
	} o_u;
    };

    #define YYSTYPE obptr_t

    static int yylex(YYSTYPE *);
    static void yyerror(object_t **, bool *, const char *);

    static object_t *cons(object_t *car, object_t *cdr);

%}

%token LPAREN RPAREN ATOM

%pure-parser
%parse-param {object_t **objp}
%parse-param {bool *eofp}
%error-verbose

%%

start : form				{ *eofp = false; *objp = $1; }
      |					{ *eofp = true; }
      ;

form  : ATOM				{ $$ = $1; }
      | LPAREN forms RPAREN		{ $$ = $2; }
      ;

forms : form forms			{ $$ = cons($1, $2); }
      |					{ $$ = NULL; }
      ;

%%

static const char *p;

static void yyerror(object_t **objp, bool *eofp, const char *s)
{
    fprintf(stderr, "yyerror: %s\n", s);
}

static int yylex(YYSTYPE *lvalp)
{
    switch (*p++) {
    case '(':
	return LPAREN;
    case ')':
	return RPAREN;
    case 'a':
	{
	    object_t *atom = malloc(sizeof (object_t));
	    atom->o_type = OT_ATOM;
	    atom->o_u.ou_atom = 'a';
	    *lvalp = atom;
	}
	return ATOM;
    case '\0':
	return --p, 0;
    default:
	assert(0 && "unknown char");
    }
}

static object_t *cons(object_t *car, object_t *cdr)
{
    object_t *obj = malloc(sizeof (object_t));
    obj->o_type = OT_PAIR;
    obj->o_u.ou_pair.p_car = car;
    obj->o_u.ou_pair.p_cdr = cdr;
    return obj;
}

static void print(object_t *obj)
{
    if (obj == NULL) {
	printf("()");
	return;
    }
    switch (obj->o_type) {

    case OT_ATOM:
	printf("%c", obj->o_u.ou_atom);
	return;

    case OT_PAIR:
	printf("(");
	print(obj->o_u.ou_pair.p_car);
	printf(" . ");
	print(obj->o_u.ou_pair.p_cdr);
	printf(")");
	return;

    default:
	fprintf(stderr, "unknown object type %d\n", obj->o_type);
	exit(1);
    }
}

int main()
{
    p = "(aa(a)())";
    //p = "a";
    //p = "()";
    p = ")";
    object_t *obj;
    bool eof;
    int failed = yyparse(&obj, &eof);
    if (failed)
	fprintf(stderr, "failed\n");
    else {
	print(obj);
	printf("\n");
    }    
    return 0;
}
