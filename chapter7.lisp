(defpackage "CHAPTER7"
  (:use "COMMON-LISP" "RING" "FILE"))

(in-package chapter7)

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

;;;3.
(defun remove-comments (in out)
  (with-open-file (str-in in :direction :input)
    (with-open-file (str-out out :direction :output :if-exists :supersede)
      (do ((c (read-char str-in nil 'eof)
              (read-char str-in nil 'eof)))
          ((eql c 'eof))
        (if (eql c #\%)
            (progn
              (read-line str-in nil)
              (terpri str-out))
            (princ c str-out))))))

;;;4.
(defun columnize (arr)
  (let ((ly (array-dimension arr 0))
        (lx (array-dimension arr 1)))
    (do ((x 0)
         (y 0))
        ((= y ly))
      (format t "~10,2F" (aref arr y x))
      (if (= x (1- lx)) 
          (progn
            (setf x 0)
            (incf y)
            (terpri))
          (incf x)))))

(eval '(columnize #2a ((2.34363 34534.4 34.234) (123.235 23.4366 23622.0) (2.3 43.4 5.32))))


;;;5.
;(load "stringsub.lisp")
(defun stream-subst-w (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((or (char= c (char old pos)) (char= #\+ (char old pos)))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(defun file-subst-w (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst-w old new in out))))


;;;6.
(defun stream-subst-6 (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((or (char= c (char old pos)) (char= #\+ (char old pos)))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((and (char= #\# (char old pos)) (digit-char-p c))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((and (char= #\& (char old pos)) (alphanumericp c))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(defun file-subst-6 (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst-6 old new in out))))


(defun stream-subst-7 (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((or
              (or (char= c (char old pos)) (char= #\+ (char old pos)))
              (and (char= #\# (char old pos)) (digit-char-p c))
              (and (char= #\& (char old pos)) (alphanumericp c)))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(defun file-subst-7 (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
                               :if-exists :supersede)
      (stream-subst-7 old new in out))))
