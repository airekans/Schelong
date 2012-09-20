#| #! /bin/sh
exec scheme --silent --load "$0"
|#

(load "driver-loop.scm")

(driver-loop)
