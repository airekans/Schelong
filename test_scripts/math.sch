(define (square x)
  (* x x))

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) 
            (- 1 y)))

(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(f 2 3)
(f-let 2 3)

(= (f 2 3) (f-let 2 3))
