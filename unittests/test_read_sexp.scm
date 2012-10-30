(load "unittest")
(load "util")

;;; Test read-atom
(set-current-input-port! (open-input-string "abc"))
(define s (make-stream))

(let ((t (read-atom s)))
  (expect-eq '(abc (0 0) (0 3)) t))


;;; Test read-list first
(set-current-input-port! (open-input-string "()"))
(set! s (make-stream))

(let ((t (read-list s)))
  (expect-eq '() (token t))
  (expect-eq '(0 0) (beg-cli t))
  (expect-eq '(0 2) (end-cli t)))

(set-current-input-port! (open-input-string "(  )"))
(set! s (make-stream))

(let ((t (read-list s)))
  (expect-eq '() (token t))
  (expect-eq '(0 0) (beg-cli t))
  (expect-eq '(0 4) (end-cli t)))

(set-current-input-port! (open-input-string "(abc)"))
(set! s (make-stream))

;; (let ((t (read-list s)))
;;   (expect-eq '((abc (0 1) (0 4)) (0 0) (0 5))))