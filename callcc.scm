(quote (reader does not handle comments so use quoted lists))

(quote (Simple example call/cc will print 123 then 234))

(define foo ())
(draft-print (call-with-current-continuation (lambda (a) (set! foo a))))
(foo 123)
(foo 234)

(quote (Simple example returns the first negative element of my-numbers))
(quote (from r6rs section 11.15))
(quote (n.b., reader doesn't do negative numbers))

(define (for-each proc list)
  (if (null? list)
      ()
      ((lambda ()
	 (proc (car list))
	 (for-each proc (cdr list))))))
(for-each draft-print (quote (1 2 3)))
(define (negative? n) (< n 0))
(define my-numbers
  (cons 54 (cons 0 (cons 37 (cons (- 3) (cons 245 (cons 19 ())))))))
my-numbers
(call/cc
  (lambda (exit)
    (for-each (lambda (x)
                (if (negative? x)
                    (exit x)))
              my-numbers)
    #t))      

(define plus3 ())
(+ 3 (call/cc
      (lambda (exit)
	(set! plus3 exit)
	4)))
(plus3 5)
