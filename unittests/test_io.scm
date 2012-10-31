(load "unittest")
(load "util")

(define s (make-stream (open-input-string "hel\n\nabc")))

(expect-eq #\h (stream-peek-char s))
(expect-eq 0 (stream-get-line-num s))
(expect-eq 0 (stream-get-col-num s))

(expect-eq #\h (stream-peek-char s))
(expect-eq 0 (stream-get-line-num s))
(expect-eq 0 (stream-get-col-num s))

(expect-eq #\h (stream-read-char s))
(expect-eq 0 (stream-get-line-num s))
(expect-eq 1 (stream-get-col-num s))

(expect-eq #\e (stream-read-char s))
(expect-eq #\l (stream-read-char s))

(expect-eq #\a (stream-peek-char s))
(expect-eq 2 (stream-get-line-num s))
(expect-eq 0 (stream-get-col-num s))

(expect-eq #\a (stream-read-char s))
(expect-eq 2 (stream-get-line-num s))
(expect-eq 1 (stream-get-col-num s))

(set! s (make-stream (open-input-string "a")))

(expect-eq #\a (stream-read-char s))
(expect-true (eof-object? (stream-peek-char s)))
(expect-true (eof-object? (stream-read-char s)))
(expect-eq (stream-read-char s) (stream-read-char s))
