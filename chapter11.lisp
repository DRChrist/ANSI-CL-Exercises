;;;;Chapter 11 CLOS

;;;1.
(defclass rectangle ()
  ((height :accessor rect-h :initarg :height :initform 1)
   (width :accessor rect-w :initarg :visible :initform 1)))

(defclass circle ()
  (radius :accessor circle-r :initarg :radius :initform 1))

(defmethod area ((x rectangle))
  (* (rect-h x) (rect-w x)))

(defmethod area ((x circle))
  (* pi (expt (circle-r x) 2)))

;;;2.
(defclass point ()
  ((x :accessor x :initarg :x :initform 0)
   (y :accessor y :initarg :y :initform 0)
   (z :accessor z :initarg :z :initform 0)))

(defclass surface ()
  (color :accessor surface-color :initarg :color :initform 'black))

(defclass sphere (surface)
  (radius :accessor sphere-radius :initarg :radius :initform 1)
  (center :accessor sphere-center :initarg :center :initform (make-instance 'point)))

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
        (make-instance 'point :x ()))))
