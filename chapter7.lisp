;;;1.
(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~x~%" line))))

(defun string-file (file)
  (with-open-file (str file :direction :input)
    (do ((res ())
         (line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof) (reverse res))
      (push line res))))
      
      
;;;2.
(defun expr-file (file)
  (with-open-file (str file :direction :input)
    (do ((res ())
         (expr (read str nil 'eof)
               (read str nil 'eof)))
        ((eql expr 'eof) (reverse res))
      (push expr res))))
