;Problem 13 - Lab 1 Lisp
;a)Write a function to return the union of two sets.
;union(l1..ln, r1...rm, c) =l1..ln, m = 0
;                           r1..rm, m = 0
;                           r1 U (union(l1..ln, r2..rm, c)), member(r1, l1..ln) != nil
;                           (union(l1..ln, r2..rm, c)), otherwise 

(defun list_union(l r)
    (cond
    ((null l) r)
    ((null r) l)
    ((member (car l) r ) r) 
    (T (cons (car l) r))
    )
)
(princ "Union")
(print (list_union '(1 2 3) '(2 3 5 7)))
(princ #\linefeed)

;b) Write a function to return the product of all numerical atoms in ;a list, at any level
;product(l1..ln) = 1, n = 0
;               product(l1) * product(l2..ln), l1 is a list
;               l1 * product(l2..ln), l1 is a number      
;               product(l2...ln) otherwise

(defun product(l)
     (cond
     ((null l) 1)
     ((listp (car l)) (* (product (car l)) (product (cdr l))))
     ((numberp (car l)) (* (car l) (product (cdr l))))
     (T (product (cdr l)))
     )
)
(princ "Product")
(print (product '(1 ((2 3) 5))))
(princ #\linefeed)


;c)Write a function to sort a linear list with keeping the double ;values.
;insert(l1..ln, e) = [e], n = 0
;                  e U l1...ln, e < l1 
;                  l1 U insert(l2..ln, e) otherwise

;sort(l1..ln) = nil, n = 0
;               insert(sort(l2..ln), l1), otherwise

(defun insert(l e)
     (cond 
     ((null l) (list e))
     ((or (equal (car l) l) (< e (car l))) (cons e l))
     (T (cons (car l) (insert (cdr l) e)))
     )
)


(defun sort2(l)
     (cond
     ((null l) nil)
     (T (insert (sort2 (cdr l)) (car l)))
     )
)
(princ "Sort")
(print (sort2 '(5 2 7 1 9 4)))
(princ #\linefeed)

;d) Build a list which contains positions of a minimum numeric ;element from a given linear list
;min_list(l1..ln, m) = 1000, n = 0
;                    min_list(l2...ln, l1), l1 < m 
;                    min_list(l2...ln, m), otherwise
;
;solve(l1...ln, min, pos) = [], n = 0
;                         pos U solve(l2..ln, min, pos++), l1 = min
;                         solve(l2..ln, min, pos++), otherwise

(defun min_list(l m)
     (cond
     ((null (car l)) m)
     ((< (car l) m) (min_list (cdr l) (car l)))
     (T (min_list (cdr l) m))
     )
)

(defun solve(l m pos)
     (cond
     ((null l) nil)
     ((equal (car l) m) (cons pos (solve (cdr l) m (+ 1 pos))))
     (T (solve (cdr l) m (+ 1 pos)))
     )
)

(defun wrapper(l)
    (solve l (min_list l 1000) 0)
)

(princ "Position list")
(print (wrapper '(3 2 5 6 2 7 7 2 3 9)))
(princ #\linefeed)
