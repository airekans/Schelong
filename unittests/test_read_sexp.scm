(load "unittest")
(load "util")

;;; Test read-atom
(define s (make-stream (open-input-string "abc ")))

(let ((t (read-atom s)))
  (expect-eq '(abc (0 0) (0 3)) t))

(set! s (make-stream (open-input-string "a")))

(let ((t (read-atom s)))
  (expect-eq '(a (0 0) (0 1)) t))

(set! s (make-stream (open-input-string "a\n abc")))

(let ((t (read-atom s)))
  (expect-eq '(a (0 0) (0 1)) t))
(expect-eq #\space (stream-read-char s))
(expect-eq '(abc (1 1) (1 4)) (read-atom s))

(set! s (make-stream (open-input-string "a\nabc")))

(let ((t (read-atom s)))
  (expect-eq '(a (0 0) (0 1)) t))
(expect-eq '(abc (1 0) (1 3)) (read-atom s))

;;; Test read-list
(set! s (make-stream (open-input-string "()")))

(let ((t (read-list s)))
  (expect-eq '() (token t))
  (expect-eq '(0 0) (beg-lci t))
  (expect-eq '(0 2) (end-lci t)))

(set! s (make-stream (open-input-string "(  )")))

(let ((t (read-list s)))
  (expect-eq '() (token t))
  (expect-eq '(0 0) (beg-lci t))
  (expect-eq '(0 4) (end-lci t)))

(set! s (make-stream (open-input-string "(abc)")))
(let ((t (read-list s)))
  (expect-eq '(((abc (0 1) (0 4))) (0 0) (0 5)) t))

(set! s (make-stream (open-input-string "( abc)")))
(let ((t (read-list s)))
  (expect-eq '(((abc (0 2) (0 5))) (0 0) (0 6)) t))


(set! s (make-stream (open-input-string "( abc 123)")))
(let ((t (read-list s)))
  (expect-eq '(((abc (0 2) (0 5)) (123 (0 6) (0 9))) (0 0) (0 10)) t))

(set! s (make-stream (open-input-string "(abc\n 123)")))
(let ((t (read-list s)))
  (expect-eq '(((abc (0 1) (0 4)) (123 (1 1) (1 4))) (0 0) (1 5)) t))

(set! s (make-stream (open-input-string "((abc))")))
(let ((t (read-list s)))
  (expect-eq '(((((abc
		   (0 2) (0 5)))
		 (0 1) (0 6)))
	       (0 0) (0 7))
	     t))

(set! s (make-stream (open-input-string "((abc) 123)")))
(let ((t (read-list s)))
  (expect-eq '(((((abc
		   (0 2) (0 5)))
		 (0 1) (0 6))
		(123
		 (0 7) (0 10)))
	       (0 0) (0 11))
	     t))

(set! s (make-stream (open-input-string "(\n)")))
(let ((t (read-list s)))
  (expect-eq '(() (0 0) (1 1)) t))

(set! s (make-stream (open-input-string "(a\nb)")))
(let ((t (read-list s)))
  (expect-eq '(((a (0 1) (0 2)) (b (1 0) (1 1))) (0 0) (1 2)) t))

(set! s (make-stream (open-input-string "()\n")))
(let ((t (read-list s)))
  (expect-eq '(() (0 0) (0 2)) t))

(set! s (make-stream (open-input-string "(\n)\n")))
(let ((t (read-list s)))
  (expect-eq '(() (0 0) (1 1)) t))

;;; Test read-sexp
(set! s (make-stream (open-input-string "")))
(let ((t (read-sexp s)))
  (expect-eq '() t))

(set! s (make-stream (open-input-string "  ")))
(let ((t (read-sexp s)))
  (expect-eq '() t))

(set! s (make-stream (open-input-string "abc")))
(let ((t (read-sexp s)))
  (expect-eq '(abc (0 0) (0 3)) t))

(set! s (make-stream (open-input-string " abc")))
(let ((t (read-sexp s)))
  (expect-eq '(abc (0 1) (0 4)) t))

(set! s (make-stream (open-input-string " \n abc")))
(let ((t (read-sexp s)))
  (expect-eq '(abc (1 1) (1 4)) t))

(set! s (make-stream (open-input-string "(abc)")))
(let ((t (read-sexp s)))
  (expect-eq '(((abc (0 1) (0 4))) (0 0) (0 5)) t))

(set! s (make-stream (open-input-string "(abc) 123")))
(let ((t (read-sexp s)))
  (expect-eq '(((abc (0 1) (0 4))) (0 0) (0 5)) t))
(let ((t (read-sexp s)))
  (expect-eq '(123 (0 6) (0 9)) t))

(set! s (make-stream (open-input-string "(define a 1)")))
(let ((t (read-sexp s)))
  (expect-eq '(((define (0 1) (0 7))
		(a (0 8) (0 9))
		(1 (0 10) (0 11))) (0 0) (0 12)) t))

;;; Test read-sexps
(set! s (make-stream (open-input-string "")))
(expect-true (null? (read-sexps s)))

(set! s (make-stream (open-input-string "  ")))
(expect-true (null? (read-sexps s)))

(set! s (make-stream (open-input-string "1")))
(expect-eq '((1 (0 0) (0 1))) (read-sexps s))

(set! s (make-stream (open-input-string "1 2 ")))
(expect-eq '((1 (0 0) (0 1)) (2 (0 2) (0 3))) (read-sexps s))

(set! s (make-stream (open-input-string "1 (add)")))
(expect-eq '((1 (0 0) (0 1)) (((add (0 3) (0 6))) (0 2) (0 7)))
	   (read-sexps s))
