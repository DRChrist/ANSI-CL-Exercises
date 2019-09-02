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

;;Boom Baby!
(defmacro ntimesrec (n &rest body)
  (let ((h (gensym)))
    `(let ((,h ,n))
       (defun rechelper (x)
         (if (= x 0)
             nil
             (progn
               ,@body
               (rechelper (- x 1))
               )))
       (rechelper ,h))))

;;;5.
(defmacro n-of (n &rest expr)
  (let ((g (gensym))
        (h (gensym))
        (j (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1))
            (,j nil))
           ((>= ,g ,h) (reverse ,j))
         (setf ,j (cons ,@expr ,j))))))

;;;6.
;;Do not really understand this exercise
(defmacro evalrevert (lst &rest body)
  `(let ((copylist (copy-list ,lst)))
     ,@body
     (setf ,lst copylist)
     (princ ,lst)))


;;;7.
(defmacro pushm (obj lst)
  `(setf ,lst (cons ,obj ,lst)))
