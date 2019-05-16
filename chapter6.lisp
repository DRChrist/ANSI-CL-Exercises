;;;1.
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defun my-tokens (str &key (test #'constituent) (start '0))
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (my-tokens str test p2)
                    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))


;;;2.
;;What is key here?
(defun bin-search (obj vec &key key (test #'eql) (start 0) (end (length vec)))
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec start end))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
        (if (funcall test obj (aref vec start))
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2)
                (finder obj vec start (- mid 1))
                (if (> obj obj2)
                    (finder obj vec (+ mid 1) end)
                    obj)))))))

;;;3.
(defun count-args (&rest args)
  (length args))

(eval '(format t "5 vs ~x ~%" (count-args 'a 'b 'c 'd 'e)))


;;;4.
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins))
             (sec nil))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf sec max
                    max score))))
        (values max sec))))

(eval '(format t "(8 5) vs ~x ~%" (multiple-value-list (most #'abs '(2 3 -4 5 -8)))))

;;;5.
(defun my-remove-if (fn lst)
  (filter (lambda (x) (if (not (funcall fn x))
                          x
                          nil))
          lst))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(eval '(format t "(1 0 -2) vs ~x ~%" (my-remove-if (lambda (x) (if (> x 2) x)) '(1 3 7 0 -2 5))))


;;;6.
(let ((x))
  (defun greatest-arg (num)
    (unless
        (and x (> x num))
      (setf x num))
    x))

(let ((x))
  (defun greatest-arg2 (num)
    (if (null x)
        (setf x num)
        (if (< x num)
            (setf x num)))
    x))

;;;7.

(let ((last-num))
  (defun greater-than-last (num)
    (if (null last-num)
        (progn
          (setf last-num num)
          nil)
        (let ((res (> num last-num)))
          (setf last-num num)
          res)
        )))

;;;8.
(let ((saved ()))
  (defun frugal (int)
    (if (null int)
        nil
        (let ((res (cdr (assoc int saved))))
          (if (null res)
              (let ((newres (expensive int)))
                (push (cons int newres) saved)
                newres)
              res)))))

(defun expensive (int)
  (+ 2 int))


;;;9.
(let ((*print-base* 8))
  (defun my-apply (fn &rest args)
    (apply fn (straighten args))))

(defun straighten (lst)
  (let ((x (car (last lst))))
    (if (consp x)
        (append (butlast lst) x)
        lst)))
