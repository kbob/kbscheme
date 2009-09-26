(define t 42)

(define-syntax or2
  (syntax-rules ()
    ((_ e1 e2)
     (let ((t e1)) (if t t e2)))))

(let ((if #f)) (or2 if t))
