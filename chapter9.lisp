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
;;Could check for more edge-cases such as ax=bx or cx=dx
(defun find-intersection (ax ay bx by cx cy dx dy)
  (let* ((a1 (/ (- ay by) (- ax bx)))
         (a2 (/ (- cy dy) (- cx dx)))
         (b1 (- ay (* a1 ax)))
         (b2 (- cy (* a2 cx))))
    (if (= a1 a2)
        nil
        (let* ((resx (/ (- b2 b1) (- a1 a2)))
               (resy (+ (* a1 resx) b1)))
          (values resx resy)))))

;;;5.
(defun approx (f min max epsilon)
;;No real idea here...
  )

;;;6.
;;ax**3 + bx**2 + cx + d
;;x(x(ax + b) + c) + d

;;ax**4 + bx**3 + cx**2 + dx + e
;;x(x(x(ax + b) + c) + d) + e
(defun horner (x &rest coefs)
  (if (null x)
      nil
      (hornerhelper x (cdr coefs) (* x (car coefs)))))

(defun hornerhelper (x coefs acc)
  (if (null (cdr coefs))
      (+ acc (car coefs))
      (hornerhelper x (cdr coefs) (* x (+ acc (car coefs))))))

;;without accumulator
(defun horner2 (x &rest coefs)
  (if (null x)
      nil
      (+ (car (last coefs)) (hornerhelper2 x (cdr (reverse coefs))))))

(defun hornerhelper2 (x coefs)
  (if (null (cdr coefs))
      (* x (car coefs))
      (* x (+ (hornerhelper2 x (cdr coefs)) (car coefs)))))

;;;7.
;;64 bits
(eval '(typep (+ 2305843009000000000 (expt 2 61)) 'fixnum))

;;;8.
;;I think just one... Every decimal number is both single-float and short-float.
