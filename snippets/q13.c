#define DEFINE_BLOCK(name, syntax_) \
    char syntax[] = #syntax_; \
    void name() { }

DEFINE_BLOCK(b_continue_if, <test> <consequent> <alternate>?)

