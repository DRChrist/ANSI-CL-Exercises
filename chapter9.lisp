(defpackage "CHAPTER9"
  (:use "COMMON-LISP" "RAYTRACING" "SPHERES" "MATHUTIL")
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
