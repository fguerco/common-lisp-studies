(let
    ((in (open "/path/to/file")))
  (loop for line = (read-line in nil)
        while line
        do (format t "~a~%" line))
  (close in))


(with-open-file (in "/path/to/file")
  (format t "~%~a~%" (read-line in nil)))



(pathname-directory (pathname "/foo/bar/baz.txt")) ;; (:ABSOLUTE "foo" "bar")
(pathname-name (pathname "/foo/bar/baz.txt"))      ;; "baz"
(pathname-type (pathname "/foo/bar/baz.txt"))      ;; "txt"


(make-pathname
 :directory '(:absolute "home" "user")
 :name "test"
 :type "txt")


(probe-file "/path/to/file")

(file-author "/path/to/file")

(file-write-date "/path/to/file")

(with-open-file (in "/path/to/file" :element-type '(unsigned-byte 8))
  (file-length in))


(let ((s (make-string-input-stream "texto para input stream")))
  (unwind-protect
       (loop for c = (read-char s nil)
             while c
             do (format t "~a~%" c))
    (close s)))

(with-input-from-string (s "texto input stream")
  (read-line s))



(directory (make-pathname :name :wild
                          :type "so"
                          :directory '(:absolute "usr" "lib")))
