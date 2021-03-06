;;;Queues
(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj))) ;sets (car q) and (cdr q) to point to the same place
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q)))) ;moves the pointer (cdr q) to point directly to the latest cons
  (car q))

(defun dequeue (q)
  (pop (car q)))


;;;Doubly linked lists
(defstruct (dl (:print-function print-dl))
  prev data next)

(defun print-dl (dl stream depth)
  (declare (ignore depth))
  (format stream "#<DL ~A>" (dl->list dl)))

(defun dl->list (lst)
  (if (dl-p lst)
      (cons (dl-data lst) (dl->list (dl-next lst)))
      lst))

(defun dl-insert (x lst)
  (let ((elt (make-dl :data x :next lst)))
    (when (dl-p lst)
      (if (dl-prev lst)
          (setf (dl-next (dl-prev lst)) elt
                (dl-prev elt) (dl-prev lst)))
      (setf (dl-prev lst) elt))
    elt))

(defun dl-list (&rest args)
  (reduce #'dl-insert args
          :from-end t :initial-value nil))

(defun dl-remove (lst)
  (if (dl-prev lst)
      (setf (dl-next (dl-prev lst)) (dl-next lst)))
  (if (dl-next lst)
      (setf (dl-prev (dl-next lst)) (dl-prev lst)))
  (dl-next lst))

;;;Constant structure
(defun arith-op (x)
  (member x '(+ - * /)))

(eval '(progn
        (setf q (make-queue))
        (enqueue 'a q)
        (enqueue 'b q)
        (enqueue 'c q)))

;;;1.
(setf a (list 'a))
(setf aa (list (list 'a) (list 'a)))
(setf one (list a a a))
(setf two (cons (list 'a) aa))
(setf three (cons (list 'a) (cons a (list a))))
(print one)
(print two)
(print three)

;;;2.


;;;3.
(defun copy-queue (q)
  (let ((new-q (make-queue)))    
    (setf (car new-q) (copy-list (car q))
          (cdr new-q) (last (car new-q)))
    new-q))

;;;4.
(defun skip-queue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (push obj (car q)))
  (car q))

;;;5.
(defun jump-queue (obj q)
  (if (null (car q))
      nil
      (progn
        (delete obj (car q))
        (push obj (car q))
        (if (eql obj (car (cdr q)))
            (setf (cdr q) (last (car q))))
        (car q))))

;;;6.
;;Don't think I want to think about circular lists.
