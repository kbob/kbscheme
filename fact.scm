(define (fact n)
  (define (f2 p n)
    (if (= n 0)
      p
      (f2 (* n p) (- n 1))))
  (f2 1 n))


(fact 7)
