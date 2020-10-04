#!/usr/bin/sbcl --script

(defparameter *graph* '((a (a b . 7) (a c . 9) (a f . 14))
                        (b (b c . 10) (b d . 15))
                        (c (c d . 11) (c f . 2))
                        (d (d e . 6))
                        (e (e f . 9))))

(defparameter *unknown* most-positive-fixnum)
