#include "proc.h"
#include "test.h"
#include "obj_eof.h"

LIBRARY(L"(rnrs io ports (6))");

/*
 * r6rs-lib 8.1, I/O Condition types
 *
 * &i/o							# condition type
 * (make-i/o-error)					# procedure
 * (i/o-error? obj)					# procedure
 *
 * &i/o-read						# condition type
 * (make-i/o-read-error)				# procedure
 * (i/o-read-error? obj)				# procedure
 *
 * *i/o-write						# condition type
 * (make-i/o-write-error)				# procedure
 * (i/o-write-error? obj)				# procedure
 *
 * &i/o-invalid-position				# condition type
 * (make-i/o-invalid-position-error)			# procedure
 * (i/o-invalid-position-error? obj)			# procedure
 * (i/o-error-position condition)			# procedure
 *
 * &i/o-filename					# condition type
 * (make-i/o-filename-error)				# procedure
 * (i/o-filename-error? obj)				# procedure
 * (i/o-error-filename condition)			# procedure
 *
 * &i/o-file-protection					# condition type
 * (make-i/o-file-protection-error)			# procedure
 * (i/o-file-protection-error? obj)			# procedure
 *
 * &i/o-file-is-read-only				# condition type
 * (make-i/o-file-is-read-only-error)			# procedure
 * (i/o-file-is-read-only-error? obj)			# procedure
 *
 * &i/o-file-already-exists				# condition type
 * (make-i/o-file-already-exists-error)			# procedure
 * (i/o-file-already-exists-error? obj)			# procedure
 *
 * &i/o-file-does-not-exist				# condition type
 * (make-i/o-file-does-not-exist-error)			# procedure
 * (i/o-file-does-not-exist-error? obj)			# procedure
 *
 * &i/o-port						# condition type
 * (make-i/o-port-error)				# procedure
 * (i/o-port-error? obj)				# procedure
 * (i/o-error-port condition)				# procedure
 */

/*
 * r6rs-lib 8.2.2, File options
 *
 * (file-options <file-options symbol> ...) 		# syntax
 */

/*
 * r6rs-lib 8.2.3, Buffer modes
 *
 * (buffer-mode <buffer-mode symbol>)			# syntax
 * (buffer-mode? obj)					# procedure
 */

/*
 * r6rs-lib 8.2.4, Transcoders
 *
 * (latin-1-codec)					# procedure
 * (utf-8-codec)					# procedure
 * (utf-16-codec)					# procedure
 *
 * (eol-style <eol-style-symbol>)			# syntax
 *
 * (native-eol-style)					# procedure
 *
 * &i/o-decoding					# condition type
 * (make-i/o-decoding-error)				# procedure
 * (i/o-decoding-error? obj)				# procedure
 *
 * &i/o-encoding					# condition type
 * (make-i/o-encoding-error)				# procedure
 * (i/o-encoding-error? obj)				# procedure
 * (i/o-encoding-error-char condition)			# procedure
 *
 * (error-handling-mode <error-handling-mode symbol>)	# syntax
 *
 * (make-transcoder codec)				# procedure
 * (make-transcoder codec eol-style			# procedure
 * (make-transcoder codec eol-style handling-mode)	# procedure
 *
 * (native-transcoder)					# procedure
 *
 * (transcoder-codec transcoder)			# procedure
 * (transcoder-eol-style transcoder)			# procedure
 * (transcoder-error-handling-mode transcoder)		# procedure
 *
 * (bytevector->string bytevector transcoder)		# procedure
 *
 * (string->bytevector string transcoder) 		# procedure
 */

/*
 * r6rs-lib 8.2.5, End-of-file object
 *
 * (eof-object)						# procedure
 *
 * (eof-object? obj)					# procedure
 */

DEFINE_PROC(L"eof-object")
{
    RETURN(make_eof());
}

DEFINE_PROC(L"eof-object?")
{
    RETURN(make_boolean(is_eof(pair_car(F_SUBJ))));
}

TEST_EVAL(L"(eof-object? (eof-object))", L"#t");

/*
 * r6rs-lib 8.2.6, Input and output ports
 *
 * (port? obj)						# procedure
 *
 * (port-transcoder port)				# procedure
 *
 * (textual-port? port)					# procedure
 * (binary-port? port)					# procedure
 *
 * (transcoded-port binary-port transcoder)
 *							# procedure
 *
 * (port-has-port-position? port)			# procedure
 * (port-position port)					# procedure
 *
 * (port-has-set-port-position!? port)			# procedure
 * (set-port-position! port pos)			# procedure
 *
 * (close-port port)					# procedure
 *
 * (call-with-port port proc)				# procedure
 */

/*
 * r6rs-lib 8.2.7, Input ports
 *
 * (input-port? obj)
 *
 * (port-eof? input-port)
 *
 * (open-file-input-port filename)			# procedure
 * (open-file-input-port filename file-options)		# procedure
 * (open-file-input-port filename file-options		# procedure
 *      buffer-mode)
 * (open-file-input-port filename file-options		# procedure
 *      buffer-mode maybe-transcoder)
 *
 * (open-bytevector-input-port bytevector) 		# procedure
 * (open-bytevector-input-port bytevector  		# procedure
 *     maybe-transcoder)
 *
 * (open-string-input-port string)			# procedure
 *
 * (standard-input-port)				# procedure
 *
 * (current-input-port)					# procedure
 *
 * (make-custom-binary-input-port id			# procedure
 *     read! get-position set-position! close)
 *
 * (make-custom-textual-input-port id			# procedure
 *     read! get-position set-position! close)
 */

/*
 * r6rs-lib 8.2.8, Binary input
 *
 * (get-u8 binary-input-port)				# procedure
 *
 * (lookahead-u8 binary-input-port)			# procedure
 *
 * get-bytevector-n binary-input-port count)		# procedure
 *
 * get-bytevector-n! binary-input-port			# procedure
 *     bytevector start count)
 *
 * get-bytevector-some binary-input-port)		# procedure
 *
 * get-bytevector-all binary-input-port)		# procedure
 */

/*
 * r6rs-lib 8.2.9, Textual input
 *
 * (get-char textual-input-port)			# procedure
 *
 * (lookahead-char textual-input-port)			# procedure
 *
 * (get-string-n textual-input-port count)		# procedure
 *
 * (get-string-n! textual-input-port			# procedure
 *     string start count)
 *
 * (get-string-all textual-input-port)			# procedure
 *
 * (get-line textual-input-port)			# procedure
 *
 * (get-datum textual-input-port)			# procedure
 */

/*
 * r6rs-lib 8.2.10, Output ports
 *
 * (output-port? obj)					# procedure
 *
 * (flush-output-port output-port)			# procedure
 *
 * (output-port-buffer-mode output-port)		# procedure
 *
 * (open-file-output-port filename)			# procedure
 * (open-file-output-port filename file-options)	# procedure
 * (open-file-output-port filename file-options		# procedure
 *     buffer-mode)
 * (open-file-output-port filename file-options		# procedure
 *     buffer-mode maybe-transcoder)
 *
 * (open-bytevector-output-port)			# procedure
 * (open-bytevector-output-port maybe-transcoder)	# procedure
 *
 * (call-with-bytevector-output-port proc)		# procedure
 * (call-with-bytevector-output-port proc		# procedure
 *     maybe-transcoder)
 *
 * (open-string-output-port)				# procedure
 *
 * (call-with-string-output-port proc)			# procedure
 *
 * (standard-output-port)				# procedure
 * (standard-error-port)				# procedure
 *
 * (current-output-port)				# procedure
 * (current-error-port)					# procedure
 *
 * (make-custom-binary-output-port id			# procedure
 * write! get-position set-position! close)
 *
 * (make-custom-textual-output-port id			# procedure
 * write! get-position set-position! close)
 */

/*
 * r6rs-lib 8.2.11, Binary output
 *
 * (put-u8 binary-output-port octet)			# procedure
 *
 * (put-bytevector binary-output-port bytevector)	# procedure
 * (put-bytevector binary-output-port bytevector start)	# procedure
 * (put-bytevector binary-output-port bytevector start	# procedure
 *     count)
 */

/*
 * r6rs-lib 8.2.12, Textual output
 *
 * (put-char textual-output-port char)			# procedure
 *
 * (put-string textual-output-port string)		# procedure
 * (put-string textual-output-port string start)	# procedure
 * (put-string textual-output-port string start count)	# procedure
 *
 * (put-datum textual-output-port datum)		# procedure
 */

/*
 * r6rs-lib 8.2.13, Input/output ports
 *
 * (open-file-input/output-port filename)		# procedure
 * (open-file-input/output-port filename file-options)	# procedure
 * (open-file-input/output-port filename file-options	# procedure
 *     buffer-mode)
 * (open-file-input/output-port filename file-options	# procedure
 *     buffer-mode transcoder)
 *
 * (make-custom-binary-input/output-port id read!	# procedure
 *     write! get-position set-position! close)
 *
 * (make-custom-textual-input/output-port id read!	# procedure
 *     write! get-position set-position! close)
 */
