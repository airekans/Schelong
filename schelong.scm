#| #! /bin/sh
exec scheme --silent --load "$0"
|#

(load "driver_loop.scm")

(driver-loop)
