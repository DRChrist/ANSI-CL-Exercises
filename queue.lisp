(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj))) ;sets (car q) and (cdr q) to point to the same place
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q)))) ;moves the pointer (cdr q) to point directly to the latest cons
  (car q))

(defun dequeue (q)
  (pop (car q)))
