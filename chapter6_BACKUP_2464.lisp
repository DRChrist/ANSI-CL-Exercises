<<<<<<< HEAD
(defun some-fun (x)
  )
=======
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
        (if (funcall test obj (aref vec start)))
        obj
        nil)
    (let ((mid (+ start (round (/ range 2)))))
      (let ((obj2 (aref vec mid)))
        (if (< obj obj2)
            (finder obj vec start (- mid 1))
            )))))

;;;3.
(defun count-args (&rest args)
  (length args))
>>>>>>> 7c38baaadc2cb1746c11793f9e1994104112624a
