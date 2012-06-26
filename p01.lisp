;;; P01. Get the last element of a argarray
(defun list-last (argarray)
  (if (not (rest argarray))
    argarray
    (list-last (rest argarray))))

;;; P02. Get the second last element of a argarray
(defun argarray-but-last (argarray)
  (last (butlast argarray)))

;;; P03. Get the kth element from a list
(defun list-element-at (argarray k)
  (if (= k 1)
    (car argarray)
    (list-element-at (rest argarray) (- k 1))))

;;; P04. Get the size of a list
(defun list-size (argarray)
  (list-size-acc argarray 0))

(defun list-size-acc (argarray n)
  (if (car argarray)
    (list-size-acc (rest argarray) (+ n 1))
    n))

;;; P05. Reverse a list
(defun list-reverse (argarray)
  (list-reverse-acc argarray '()))

(defun list-reverse-acc (argarray revarray)
  (if (car argarray)
    (list-reverse-acc (rest argarray) (append (list (car argarray)) revarray))
    revarray))

;;; P06. Check if a list is a palindrome
(defun list-is-palindrome (argarray)
  (list-equal argarray (list-reverse argarray)))

(defun list-equal (list1 list2)
  (cond
    ;; if the lists are different sizes they're not equal
    ((/= (list-size list1) (list-size list2)) nil)
    ;; if we're at the end of both lists they're equal
    ((and (not (rest list1)) (not (rest list2))) t)
    ;; if the elements are not equal the lists are not equal
    ((/= (car list1) (car list2)) nil)
    ;; go to the next elements
    (t (list-equal (rest list1) (rest list2)))))

;;; P07. Flatten a nested list
;;; TODO: this is O(n^2) if i'm not mistaken. bad! bad! bad!
(defun list-flatten (argarray)
  (cond
    ((null argarray) argarray)
    ((atom argarray) (list argarray))
    (t (append (list-flatten (first argarray)) (list-flatten (rest argarray))))))

;;; P08. Remove duplicate consecutive elements of a list
(defun list-compress (argarray)
  (list-compress-acc argarray '()))

(defun list-compress-acc (argarray result)
  (cond
    ((null argarray) (list-reverse result))
    ((eq (first argarray) (first result)) (list-compress-acc (rest argarray) result))
    (t (list-compress-acc (rest argarray) (append (list (first argarray)) result)))))

;;; P09. Pack consecutive duplicates into sublists
(defun list-pack (argarray)
  t)

;;; kate: indent-mode LISP; indent-width 2; tab-width 2;