;16. Determine if a tree of type (2) is ballanced (the difference between the depth of two subtrees is equal to 1).

;maximum(a, b) = a, a >= b 
;                b, otherwise

(defun maximum(a b)
    (cond
    ((< a b) b)
    (T a)
    )
)

;height(l1..ln) = 0, n = 0
;                 1 + maximum(height(l2), height(l3)), otherwise

(defun height(l)
    (cond
    ((null l) 0)
    ((atom l) 1)
    (T (+ 1 (maximum (height (car (cdr l))) (height (cdr (cdr l))))))
    )
)

;absolute(a, b) = a - b, a >= b
;                 b - a, otherwise

(defun absolute(a b)
    (cond
    ((< a b) (- b a))
    (T (- a b))
    )
)

;isBallanced(l1...ln) = 1, n = 0
;                       1, absolute(height(l2) - height(l3)) <= 1 && isBallanced(l2) = 1 && isBallanced(l3) = 1
;                       0, otherwise

;(defun isBallanced(l)
;    (cond 
;    ((null l) 1)
;    ((and (atom (car l)) (null (cdr l))) 1)
;    ((and (null (cdr(cdr l))) (equal 1 (absolute (height (car (cdr l))) 0))) 1)
;    ((and (equal 1 (absolute (height (car (cdr l))) (height (cdr (cdr l))))) (equal 1 (isBallanced (car (cdr l)))) (equal 1 (isBallanced (cdr (cdr l))))))
;    (t 0)
;    )
;)


(defun isBallanced(l)
    (cond 
    ((null l) 1)
    ((and (atom (car l)) (null (cdr l))) 1)
    ((and (>= 1 (absolute (height (car (cdr l))) (height (cdr (cdr l))))) (equal 1 (isBallanced (car (cdr l)))) (equal 1 (isBallanced (cdr (cdr l))))) 1)
    (t 0)
    )
)

(princ "Ballanced3")
(print (isBallanced '(A(B(C)))))
(princ #\linefeed)
(princ "Ballanced")
(print (isBallanced '(A(B))))
(princ #\linefeed)
(princ "Ballanced2")
(print (isBallanced '(A(B)(C))))
(princ #\linefeed)
(princ "Ballanced4")
(print (isBallanced '(A(B)(C(D)(E)))))
(princ #\linefeed)
