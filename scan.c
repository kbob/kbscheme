// identifier:
//	initial subsequent*
// boolean:
//	#f
//	#t
// number:
//	...
// character:
//	#\\.
//	#\\(null|alarm|backspace|tab|linefeed|newline|vtab|page|return)
//	#\\(esc|space|delete)
//	#\[[:xdigit:]]+
// string:
//      "([^"\\]|\\[abtnvfr"\\]|\s\n\s|\\\x+;)*"
// punctuation:
//	(
//	)
//	[
//	]
//	#(
//	#vu8(
//	'
//	`
//	,
//	,@
//	.
//	#'
//	#`
//	#,
//	#,@

/*
classify characters.
classes:
    self - many single-char classes
    digit
    xdigit
    alpha (US)
    Ll Lu Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co
    Nd Mc Me
    Zs Zl Zp
 */

/*
 * Chars are members of multiple classes: 8 is self (in "#vu8("),
 * a digit, and in Nd.
 *
 * Do we need an NFA?
 */

#if 0

I want to write...
  #!r6rs => ignore
  initial subsequent* => identifier
  ->subsequent* => identifier
  ... => identifier
etc.

#endif
