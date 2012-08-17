(define (fact-iter n)
  (define (iter x result)
    (if (= x 1)
	result
	(iter (- x 1) (* x result))))
  (iter n 1))

(fact-iter 1)
(fact-iter 2)
(fact-iter 3)
(fact-iter 4)
