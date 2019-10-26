(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun foob (x)
  (single? (bar x)))


;;;1.
;;Looks like it does not...

;;;2.
(defun foo (x)
  (if (zerop x)
      0
      (+ 1 (foo (1- x)))))

(defun foo-tail (x acc)
  (if (zerop x)
      acc
      (foo-tail (1- x) (1+ acc))))
