;Problem 13 - Lab 1 Lisp
;a)
;union(l1..ln, r1...rm, c) = [], n = m = 0
;                           l1..ln, m = 0
;                           r1..rm, m = 0
;                           r1 U (union(l1..ln, r2..rm, c)), member(r1, l1..ln) != nil
;                           (union(l1..ln, r2..rm, c)), otherwise 

(defun union(l r)
    (cond
    ((null l) r)
    ((null r) l)
    ((member (car l) r ) r) 
    (T (cons (car l) r))
    )
)

(print (union '(1 2 3) '(2 3 5 8 2)))

;b)
;product(l1..ln) = 1, n = 0
;               product(l1) * product(l2..ln), l1 is a list
;               l1 * product(l2..ln), l1 is a number      
;               product(l2...ln) otherwise

(defun nrProduct(l)
     (cond
     ((null l) 1)
     ((listp (car l)) (* (nrProduct (car l)) (nrProduct (cdr l))))
     ((numberp (car l)) (* (car l) (nrProduct (cdr l))))
     (T (nrProduct (cdr l)))
     )
)

(print (nrProduct '(5 (1 2 (3)) 2)))

;c)
;insert(l1..ln, e) = [e], n = 0
;                  e U l1...ln, e < l1 
;                  l1 U insert(l2..ln, e) otherwise

;sort(l1..ln) = nil, n = 0
;               insert(sort(l2..ln), l1), otherwise

(defun insertEl(l e)
     (cond 
     ((null l) (list e))
     ((or (equal (car l) l) (< e (car l))) (cons e l))
     (T (cons (car l) (insertEl (cdr l) e)))
     )
)


(defun sort2(l)
     (cond
     ((null l) nil)
     (T (insertEl (sort2 (cdr l)) (car l)))
     )
)

(print (sort2 '(2 5 9 7 4 1)))

;d)
;min(a, b) = a, if a < b
;            b, otherwise

;min_list(l1..ln, m) = 1000, n = 0
;                    min_list(l2...ln, l1), l1 < m 
;                    min_list(l2...ln, m), otherwise
;
;solve(l1...ln, min, pos) = [], n = 0
;                         pos U solve(l2..ln, min, pos++), l1 = min
;                         solve(l2..ln, min, pos++), otherwise

(defun min_list(l m)
     (cond
     ((null l) 1000)
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
    (solve l (min_list l 0) 0)
)

(print (wrapper '(3 2 5 6 2 7 7 2 3 9)))