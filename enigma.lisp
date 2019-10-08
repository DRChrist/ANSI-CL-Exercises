
(defun printexercise (heading &optional code output prints?)
  (format t "~x:~%" heading)
  (if (null code)
      (format t "TBD~%~%")
      (progn
        (format t "~x~%" code)
        (format t "actual:  ")
        (if prints?
            (eval code)
            (format t "~x" (eval code)))
        (terpri)
        (if output (format t "expected: ~x~%" output))
        (terpri))))

;checks whether there is an empty list in the list x
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

;returns the index of x in the list y
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

;returns true if lst has an element that is a list
(defun chk-lst (lst)
  (if (null lst)
      nil
      (or (listp (car lst))
          (chk-lst (cdr lst)))))

(defun print-dots-it (n)
  (do ((i 0 (+ i 1)))
      ((= i n) 'Dotted!)
    (format t ".")))

(defun print-dots-rec (n)
  (if (= n 0)
      'Dotted!
      (format t ".")
      )
  (print-dots-rec (- n 1)))

(defun four-lst (lst)
  (car (cdr (cdr (cdr lst)))))

(defun gr-than (a b)
  (if (> a b)
      a
      b))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (format t "Queue: ~x~%" queue)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

(defun new-paths (path node net)
  (format t "Path: ~x~%" path)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(defun new-union (lst1 lst2)
  (let ((res ()))
    (dolist (n (append lst1 lst2))
      (if (not (member n res))
          (setf res (cons n res))))
    (reverse res)))

(defun occurrences (lst)
  (occ-loop lst ()))

(defun occ-loop (lst res)
  (if (null lst)
      res
      (let ((x (assoc (car lst) res)))
        (if x
            (occ-loop (cdr lst) (inc-assoc res x))
            (occ-loop (cdr lst) (cons (cons (car lst) 1) res))))))

(defun inc-assoc (res ele)
  (let ((n (car ele))
        (l (+ (cdr ele) 1)))
    (cons (cons n l) (remove-if (lambda (y) (eql (car y) n)) res))))

(defun quarter-turn (arr)
  (let ((l0 (array-dimension arr 0))
        (l1 (array-dimension arr 1)))
    (let ((res-arr (make-array (list l0 l1) :initial-element nil)))
      (do ((i 0)
           (j 0)
           (n (1- l0)))
          ((= j l1))
        (setf (aref res-arr i n) (aref arr j i))
        (if (eql i (1- l0))
            (progn
             (setf i 0)
             (setf j (1+ j))
             (setf n (1- n)))
            (setf i (1+ i))))
      res-arr)))

(defun r-copy-list (lst)
  (reduce #'copier (reduce #'copier lst)))

(defun copier (a b)
  (if (listp a)
      (cons b a)
      (list b a)))

(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~X>" (node-elt n)))))
  elt (l nil) (r nil))

(defstruct (trinode (:print-function
                     (lambda (n s d)
                       (format s "elt:~X  left:~X mid:~X right:~X"
                               (trinode-elt n)
                               (trinode-left n)
                               (trinode-mid n)
                               (trinode-right n)))))
  elt (left nil) (mid nil) (right nil))

(defun copy-tritree (tt)
  (if (null tt)
      nil
      (make-trinode
       :elt  (trinode-elt tt)
       :left (copy-tritree (trinode-left tt))
       :mid (copy-tritree (trinode-mid tt))
       :right (copy-tritree (trinode-right tt)))))

(defun tt-find (obj tt)
  (if (null tt)
      nil
      (progn
        (format t "obj: ~x  elt: ~x ~%" obj (trinode-elt tt))
        (or (eql (trinode-elt tt) obj)
            (tt-find obj (trinode-left tt))
            (tt-find obj (trinode-mid tt))
            (tt-find obj (trinode-right tt))))))

(PrintExercise
 "Setting up tt"
 '(progn
   (setf tt nil)
   (setf tt (make-trinode
             :elt 'XXX
             :left (make-trinode :elt 'abc)
             :mid (make-trinode :elt 'def)
             :right (make-trinode :elt'ghi)))
   tt)
 "N/A")

(eval '(progn
        (setf tt2 nil)
        (setf tt2 (make-trinode
                   :elt '111
                   :left (make-trinode :elt '222)
                   :mid (make-trinode :elt '333)
                   :right (make-trinode :elt '444)))))

(defun list-bst (bst)
  (if (null bst)
      nil
      (append
       (list-bst (node-r bst))
       (list (node-elt bst))
       (list-bst (node-l bst)))))

  
(defun print-bst (bst)
  (if (null bst)
      nil
      (progn
        (print-bst (node-r bst))
        (format t "~x~%" bst)
        (print-bst (node-l bst))
        )))


(defun bst-insert (obj bst <)
  (if (null bst) ; check if we are starting with an empty tree 
      (make-node :elt obj) 
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                q(make-node
                 :elt elt
                 :l (bst-insert obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-insert obj (node-r bst) <)
                 :l (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

(defun my-copy-list (lst)
  (reduce (function cons) lst :from-end t :initial-value nil))

(PrintExercise
 "Setting up the nums bst"
 '(progn
   (setf nums nil)
   (dolist
       (x '(5 8 4 2 1 9 6 7 3))
     (setf nums (bst-insert x nums (function <)))
     )
   nums
   )
 "N/A")

(defun lst-to-hash (lst)
  (if (null lst)
      nil
      (let ((ht (make-hash-table)))
        (dolist (n lst)
          (setf (gethash (car n) ht) (cdr n)))
        ht)))

(eval '(progn
        (setf alist nil)
        (setf alist '((a . 1) (b . 2) (c . 3)))))

(eval '(setf ht (lst-to-hash alist)))

(defun hash-to-lst (ht)
  (let ((lst ()))
    (maphash #'(lambda (k v)
                 (progn
                   (format t "lst: ~x ~%" lst)
                   (push (cons k v) lst)))
             ht)
    (reverse lst)))





;;;;;;;;;;;;;;;;;;;;;
;;;Destructive binary search tree from chapter 12
;;;;;;;;;;;;;;;;;;;;;
(defun bst-insert! (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (progn (bsti obj bst <)
             bst)))

(defun bsti (obj bst <)
  (let ((elt (node-elt bst)))
    (if (eql obj elt)
        bst
        (if (funcall < obj elt)
            (let ((l (node-l bst)))
              (if l
                  (bsti obj l <)
                  (setf (node-l bst)
                        (make-node :elt obj))))
            (let ((r (node-r bst)))
              (if r
                  (bsti obj r <)
                  (setf (node-r bst)
                        (make-node :elt obj))))))))

(defun bst-delete (obj bst <)
  (if bst (bstd obj bst nil nil <))
  bst)

(defun bstd (obj bst prev dir <)
  (let ((elt (node-elt bst)))
    (if (eql elt obj)
        (let ((rest (percolate! bst)))
          (case dir
            (:l (setf (node-l prev) rest))
            (:r (setf (node-r prev) rest))))
        (if (funcall < obj elt)
            (if (node-l bst)
                (bstd obj (node-l bst) bst :l <))
            (if (node-r bst)
                (bstd obj (node-r bst) bst :r <))))))

(defun percolate! (bst)
  (cond ((null (node-l bst))
         (if (null (node-r bst))
             nil
             (rperc! bst)))
        ((null (node-r nst)) (lperc! bst))
        (t (if (zerop (random 2))
               (lperc! bst)
               (rperc! bst)))))

(defun lperc! (bst)
  (setf (node-elt bst) (node-elt (node-l bst)))
  (percolate! (node-l bst)))

(defun rperc! (bst)
  (setf (node-elt bst) (node-elt (node-r bst)))
  (percolate! (node-r bst)))
