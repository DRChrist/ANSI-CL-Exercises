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

;;This does not work
(defmacro ntimesrec (n &rest body)
  (let ((h (gensym)))
    `(let ((,h ,n))
       (if (= ,h 0)
           nil
           (progn
             ,@body
             (ntimesrec (1- ,h) ,@body)
             )))))

;;This works, but I am not sure it prevents multiple evaluation.
;;It doesn't work if called inside a let-body, but I don't know if it is supposed to do that.
(defmacro ntimesrec-helper (n &rest body)
  `(if (= ,n 0)
       nil
       (progn
         ,@body
         (ntimesrec-helper (1- ,n) ,@body))))

;;;5.
(defmacro n-of (n &rest expr)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do))))
