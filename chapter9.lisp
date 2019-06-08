(defpackage "CHAPTER9"
  (:use "COMMON-LISP" "RAYTRACING" "MATHUTIL")
  (:export "RAY-TEST"))

(in-package chapter9)

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .3)
  ;(defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9999)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))


;;;1.
(defun non-decreasingp (lst)
  (if (null (cdr lst))
      t
      (if (<= (car lst) (car (cdr lst)))
          (non-decreasingp (cdr lst))
          nil)))

;;;2.
(defun make-change (int)
  (multiple-value-bind (c25 rest25)
      (floor int 25)
    (multiple-value-bind (c10 rest10)
        (floor rest25 10)
      (multiple-value-bind (c5 c1)
          (floor rest10 5)
        (values c25 c10 c5 c1)))))


;;;3.
(defun song-contest ()
  (do ((count 0 (incf count))
       (res (list (cons 'wigglie 0) (cons 'wobblie 0)))
       (winner (random 2) (random 2)))
      ((= count 10) res)
    (if (= winner 1)
        (incf (cdr (assoc 'wigglie res)))
        (incf (cdr (assoc 'wobblie res))))))

;;;4.
(defun find-intersection (qx qy wx wy tx ty fx fy)
  )
