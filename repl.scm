((lambda ()
 (define (repl)
  ((lambda (x)
     (if (not (eq? x (quote exit)))
	 ((lambda ()
	    (draft-print (eval x (draft-environment)))
	    (repl)))))
   (draft-read)))
 (repl)))