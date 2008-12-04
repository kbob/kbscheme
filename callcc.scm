; Simple example call/cc will print 123 then 234.

(define foo ())
(draft-print (call-with-current-continuation (lambda (a) (set! foo a))))
(foo 123)
(foo 234)

; Simple example returns the first negative element of my-numbers
; -- from r6rs section 11.15.

(define (for-each proc list)
  (if (null? list)
      ()
      ((lambda ()
	 (proc (car list))
	 (for-each proc (cdr list))))))
(for-each draft-print (quote (1 2 3)))
(define (negative? n) (< n 0))
(call/cc
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x)
                    (exit x)))
              '(54 0 37 -3 245 19))
    #t))      

(define plus3 ())
(+ 3 (call/cc
      (lambda (exit)
	(set! plus3 exit)
	4)))
(plus3 5)
