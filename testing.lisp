(defun triple (x)
  "Compute three times X." ; Inline comments works
  (* 3 X))

(defun negate (X)
  "Negate the value of X."
  (- X))

(defun count-up (n)
  (format t "~a " n)
  (if (= n 0)
      0
      (+ n (count-up (- n 1)))))

(defun print-dots-rec (n)
  (if (= n 0)
      'Dotted!
      (or (format t ".")
          (print-dots-rec (- n 1)))))

(defun cnt-symbols-it (lst x)
  (let ((res 0))
    (dolist (y lst)
      (if (eql y x)
          (setf res (+ res 1))
          0))
    res))

(defun cnt-symbols-rec (lst x)
  (if (null lst)
      0
      (if (eql x (car lst))
          (+ 1 (cnt-symbols-rec (cdr lst) x))
          (cnt-symbols-rec (cdr lst) x))))

(defun summit1 (lst)
  (setf lst (remove nil lst))
  (apply #'+ lst))

(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (null x)
            (summit (cdr lst))
            (+ x (summit (cdr lst)))))))

(defun new-union (lst1 lst2)
  (reverse (union-loop (append lst1 lst2) '())))

(defun union-loop (lst res)
  (if (null lst)
      res
      (if (member (car lst) res)
          (union-loop (cdr lst) res)
          (union-loop (cdr lst) (cons (car lst) res))))
  )

;Still need to sort the output
(defun occurrences (lst)
  (occ-loop lst))


(defun occ-loop (lst)
  (let ((res ()))
    (dolist (x lst)
      (format t "res: ~x " res)
      (let ((y (assoc x res)))
        (format t "y: ~x ~%" y)
        (if y
            (setf (cdr y) (+ (cdr y) 1))
            (setf res (cons (cons x 1) res)))))
    res))

(defun pos+-rec (lst)
  (reverse (pos+-loop lst () 0)))

(defun pos+-loop (lst res i)
  (if (null lst)
      res
      (pos+-loop (cdr lst) (cons (+ (car lst) i) res) (+ i 1))))

(defun pos+-ite (lst)
  (let ((ix 0)
        (res ()))
    (dolist (n lst)
      (setf n (+ n ix))
      (setf res (cons n res))
      (setf ix (+ ix 1)))
    (reverse res)))

(defun pos+-map (lst)
  (let ((ix -1))
    (mapcar (lambda (x) (setf ix (+ ix 1)) (+ x ix)) lst)))

;I don't understand this
(defun cons-gov (x y)
  (cons y x))

(defun showdots (lst)
  (let ((cnt 0))
    (dolist (n lst)
      (format t "(~x . " n)
      (setf cnt (+ cnt 1)))
    (format t "NIL")
    (do  ((i 0 (1+ i)))
         ((= i cnt))
      (format t ")"))))


(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (n-elts elt n)
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)
      elt))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (format t "queue: ~x~%" queue)
  (format t "cdr queue: ~x~%" (cdr queue))
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
  (format t "path: ~x node: ~x~%" path node)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

(setf mynet '((a b c) (b c) (c d)))
(setf mynet1 '((a h e j k) (e q u) (q r g k t) (t y z x c w) (q w j) (w p f s)))


(defun longest-path (start end net)
  (lbfs end (list (list start)) net))

(defun lbfs (end queue net)
  (format t "queue: ~x~%" queue)
  (format t "cdr queue: ~x~%" (cdr queue))
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (if (null (cdr queue))
                  (reverse path)
                  (lbfs end
                        (append (cdr queue)
                                (new-paths path node net))
                        net))
              (lbfs end
                    (append (cdr queue)
                            (new-paths path node net))
                    net))))))


(defstruct (my-node (:print-function
                  (lambda (n s d)
                    (format s "#<~X>" (my-node-data n)))))
  data (left nil) (mid nil) (right nil))

(defun tree-copy (tree)
  (let ((res-tree (copy-my-node tree)))
    (tree-copier tree res-tree)
    res-tree))

(defun tree-copier (inp res-tree)
  (if (not (null (my-node-right inp)))
      (tree-copier (my-node-right inp) (copy-my-node (my-node-right inp)))
      nil)
  (if (not (null (my-node-mid inp)))
      (tree-copier (my-node-mid inp) (copy-my-node (my-node-mid inp)))
      nil)
  (if (not (null (my-node-left inp)))
      (tree-copier (my-node-left inp) (copy-my-node (my-node-left inp)))
      nil))

(defun t-traversal (obj tree)
  (format t "~x > ~x~%" (my-node-data tree) (my-node-left tree))
  (format t "~x > ~x~%" (my-node-data tree) (my-node-mid tree))
  (format t "~x > ~x~%" (my-node-data tree) (my-node-right tree))
  (if (eql obj (my-node-data tree))
      (progn (format t "Found it!")
             t)
      (progn
        (if (not (null (my-node-right tree)))
            (t-traversal obj (my-node-right tree))
            nil)
        (if (not (null (my-node-mid tree))) 
            (t-traversal obj (my-node-mid tree))
            nil)
        (if (not (null (my-node-left tree)))
            (t-traversal obj (my-node-left tree))
            nil))))
