(define (fact n)
  (if (< n 2)
      n
      (* n (fact (- n 1)))))

(fact 3)
(fact 4)
(fact 5)
