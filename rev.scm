(import (rnrs))

(define (rev l)
  (define (rr l r)
    (if (null? l)
	r
	(rr (cdr l) (cons (car l) r))))
  (rr l '()))

(write (rev '(a b c)))
(newline)

(define (expand form env)
  (define (macro-use? form)
    (and (pair? form) (pair? (car form)) (eq? (caar form) define-syntax)))
  (define (define-syntax? form)
    (and (pair? form) (eq? (car form) define-syntax)))
  (cond
   [(null? form) => '()]
   [(macro-use? form) => (expand (transform form env) env)]
   [(define-syntax? form) => (expand (cdr form) (prepend-env env (car form)))]
    
   [else ...]))
