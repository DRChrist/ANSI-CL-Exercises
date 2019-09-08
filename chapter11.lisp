;;;;Chapter 11 CLOS

;;;1.
(defclass rectangle ()
  ((height :accessor rect-h :initarg :height :initform 1)
   (width :accessor rect-w :initarg :visible :initform 1)))

(defclass circle ()
  ((radius :accessor circle-r :initarg :radius :initform 1)))

(defmethod area ((x rectangle))
  (* (rect-h x) (rect-w x)))

(defmethod area ((x circle))
  (* pi (expt (circle-r x) 2)))

;;;2.
(defparameter *world* nil)
(defconstant eye (make-instance 'point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (- 50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))

(defclass point ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (z :accessor z :initarg :z :initform 0)))

(defclass surface ()
  ((color :accessor surface-color :initarg :color :initform 'black)))

(defclass sphere (surface)
  ((radius :accessor sphere-radius :initarg :radius :initform 1)
   (center :accessor sphere-center :initarg :center :initform (make-instance 'point))))

(defmethod defsphere (x y z r c)
  (let ((s (make-instance 'sphere
                          :radius r
                          :center (make-instance 'point :x x :y x :z z)
                          :color c)))
    (push s *world*)
    s))

(defmethod intersect ((s surface) (pt point) xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defmethod sphere-intersect ((s sphere) (pt point) xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-instance 'point
                       :x (+ (x pt) (* n xr))
                       :y (+ (y pt) (* n yr))
                       :z (+ (z pt) (* n zr))))))

(defmethod normal ((s surface) (pt point))
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defmethod sphere-normal ((s sphere) (pt point))
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere 'sphere -80 -150 -1200 200 .7)
  (tracer (make-pathname :name "spheres2.pgm") res))

;;;3.
;;maybe later

;;;4.
;;Not fun and not done
;; (defun most-spec-app-meth (genfun params)
;;   (let ((meths (methods genfun)))
;;     (if (null meths)
;;         nil
;;         (check-specificity meths params ()))))

;; (defun check-specificity (meths params prec)
;;   (if (null meths)
;;       nil
;;       (let ((lst (specializations (car meths))))
;;         )))

;;;5.
;;Not sure what the point is here
(defclass shape-counter (surface)
  ctr :accessor ctr)
(defmethod area :before ((x shape-counter))
  (incf (ctr x)))

;;;6.
