(define (even? n)
  (= 0 (% n 2)))

(define (map f l)
  (if (null? l)
      l
      (cons (f (car l))
	    (map f (cdr l)))))

(define (range n m)
  (if (> n m)
      (quote ())
      (cons n (range (+ n 1) m))))

(define (largest-odd n)
  (if (even? n)
      (- n 1)
      n))

(map (lambda (n)
       (if (even? n)
	   (- n 1)
	   n))
     (range 1 5000))
