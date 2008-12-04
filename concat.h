#ifndef CONCAT_INCLUDED
#define CONCAT_INCLUDED

/* concatenate into one identifier */
#define CAT_(a, b) CAT__(a, b)
#define CAT__(a, b) a ## b

#endif /* !CONCAT_INCLUDED */
