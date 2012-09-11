(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

count ; should be 1

w ; count should be changed to 2

count ; should be 2
