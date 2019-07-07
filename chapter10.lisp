;;;;Chapter 10 Exercises

;;;1.
;;(a)
;`(,z ,x z)
;;(b)
;`(x ,y ,@z)
;;(c)
;`((,@z ,x) z)

;;;2.
(defmacro ifcond (pred then else)
  `(cond (,pred ,then)
         (t ,else)))

;;;3.
(defmacro nth-expr (n &rest exprs)
  `(eval (nth (1- ,n) (quote ,exprs))))

;;;4.
(defmacro ntimesdo (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))

(defmacro ntimesrec (n &rest body)
  (princ n)
  (let ((h (gensym)))
    `(let ((,h ,n))
       (princ ,h)
       (if (= ,h 0)
           nil
           (progn
             ,@body
             (ntimesrec (1- ,h) ,@body)
             )))))

(defmacro ntimestest (n)
  (princ n)
  (let ((g (gensym)))
    `(let ((,g ,n))
      (princ ,g)
      (if (= ,g 0)
          nil
          (ntimestest (1- ,g))))))
#|
(defun ntimesrec-helper (n &rest body)
  (if (= n 0)
      nil
      (progn
        body
        (ntimesrec-helper (1- n) body))))
|#
(defmacro ntimesrec-helper (n &rest body)
  `(if (= ,n 0)
       nil
       (progn
         ,@body
         (ntimesrec-helper (1- ,n) ,@body))))
