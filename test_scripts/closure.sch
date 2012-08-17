(define (get-number-closure n)
  (lambda () n))

(define get-1 (get-number-closure 1))
(get-1)

(define get-100 (get-number-closure 100))
(get-100)
