; This is the top-level program loop.
; Scheme automatically reads and runs it.

((lambda ()
 (define env (draft-environment)) ; XXX should inherit (rnrs (6)).
 (define (repl)
  ((lambda (x)
     (if (not (eq? x (quote exit)))
	 ((lambda ()
	    (draft-print (eval x env))
	    (repl)))))
   (draft-read)))
 (repl)))
