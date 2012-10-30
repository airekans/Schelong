(load "unittest")
(load "util")

(set-current-input-port! (open-input-string "hel\n\nabc"))
(define s (make-stream))

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