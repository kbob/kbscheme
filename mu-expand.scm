(lambda (x)
  (define (trav z)
    (if (eq? z (quote +))
	(quote -)
	(if (pair? z)
	    (cons (trav (car z)) (trav (cdr z)))
	    z)))
;  (draft-print (quote mu-transform))
;  (draft-print x)
;  (draft-print (trav x))
  (trav x))
