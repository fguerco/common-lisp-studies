
;; 0                   1                   2                   3
;;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;; |                          time_low                             |
;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;; |       time_mid                |         time_hi_and_version   |
;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;; |clk_seq_hi_res |  clk_seq_low  |         node (0-1)            |
;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;; |                         node (2-5)                            |
;; +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


;; Field                  Data Type     Octet  Note
;;                                      #

;; time_low               unsigned 32   0-3    The low field of the
;;                        bit integer          timestamp

;; time_mid               unsigned 16   4-5    The middle field of the
;;                        bit integer          timestamp

;; time_hi_and_version    unsigned 16   6-7    The high field of the
;;                        bit integer          timestamp multiplexed
;;                                             with the version number
;; clock_seq_hi_and_rese  unsigned 8    8      The high field of the
;; rved                   bit integer          clock sequence
;;                                             multiplexed with the
;;                                             variant

;; clock_seq_low          unsigned 8    9      The low field of the
;;                        bit integer          clock sequence

;; node                   unsigned 48   10-15  The spatially unique
;;                        bit integer          node identifier



;; o  Set the two most significant bits (bits 6 and 7) of the
;;    clock_seq_hi_and_reserved to zero and one, respectively.

;; o  Set the four most significant bits (bits 12 through 15) of the
;;    time_hi_and_version field to the 4-bit version number from
;;    Section 4.1.3.

;; o  Set all the other bits to randomly (or pseudo-randomly) chosen
;;    values.


(defun uuid-v4 ()
  (let ((bytes (map 'list (lambda (x) (random (expt 256 x))) #(4 2 2 2 6))))
    (setf (ldb (byte 4 12) (elt bytes 2)) 4)
    (setf (ldb (byte 2 14) (elt bytes 3)) 1)
    (apply #'format nil "~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x" bytes)))


(defun test-uuid (uuid)
  (print uuid)
  (let ((f2 (parse-integer (subseq uuid 14 18) :radix 16))
        (f3 (parse-integer (subseq uuid 19 23) :radix 16)))
    (assert (= 4 (ldb (byte 4 12) f2)))
    (assert (= 1 (ldb (byte 2 14) f3)))))


(dotimes (x 100)
  (test-uuid (uuid-v4)))
