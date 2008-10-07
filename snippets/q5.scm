(define cont '())

(define (f1) (display 'f1) 1)
(define (f2)
  (define f2val
    (call-with-current-continuation
      (lambda (exit)
        (set! cont exit) (exit 2))))
  (display 'f2)
  f2val)
(define (f3) (display 'f3) 3)

(display (+ (f1) (f2) (f3)))

(cont 5)
