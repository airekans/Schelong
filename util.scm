(define (concat a b)
  (if (null? a)
      b
      (cons (car a) (concat (cdr a) b))))
