;R2
;Se da o lista neliniara formata din numere si atomi.
;Sa se determine lista elementelor numerice divizibile cu 5 din lista initiala, de la orice nivel, in ordine inversa.
;L= (5 a (b 10 (c 5 (12 (d 25) b 10)) 7)) => R = (10 25 5)

;reverseLst(l1..ln) = [], n = 0
;                     reverseLst(l2..ln) U reverseLst(l1)
;                     
(defun reverseLst (lst)
	(cond
		((null lst) nil)
		(t (append (reverseLst (cdr lst)) (list (car lst))))
	)
)

;div5(l1..ln) = [], n = 0
;               l1 U div5(l2..ln), l1 is an atom and l1 is divisibile with 5
;               div5(l1) U div5(l2..ln), if l1 is a list 
;               div5(l2..ln), otherwise


(defun div5 (lst)
	(cond
		((null lst) nil)
		((and (numberp lst) (equal (mod lst 5) 0)) (list lst))
		((listp lst) (mapcan 'div5 lst))
	)
)

;solve(l1..ln) = [], n = 0
;               reverseLst(div5(l1...ln)), otherwise

(defun solve(lst)
    (reverseLst (div5 lst))
)
(print (solve '(5 a (b 10 (c 51 (12 (d 25) b 101)) 7))))

;appears(l1..ln, el) = 0, n = 0
;                      1 + appears(l2..ln), l1 is an atom and l1 = el 
;                      appears(l2..ln), otherwise

;removeLst(l1..ln, el) = l1..ln - el 

;unique(l1...ln, r1..rm) = [], n = 0
;                  l1 U unique(l2..ln), if appears(r1..rm, l1) = 1 and l1 ; ;                   is an atom, append(l1, r1..rm)
;                  unique(l1) U unique(l2..ln), if l1 is a list 
; 

(defun appears(lst el)
       (cond 
       ((null (car lst)) 0)
       ((and (atom (car lst)) (equal (car lst) el)) (+ 1 (appears(cdr lst) el)))
       (t (appears (cdr lst) el))
       )
)

(defun unique(lst1 lst2)
    (cond 
    ((null lst1) lst2)
    ((and (atom (car lst1)) (>= (appears lst2 (car lst1)) 1)) (unique (cdr lst1) (append (car lst1) lst2)))
    ((listp (car lst1)) (append (unique (car lst1) lst2) (unique (cdr lst1) lst2)))
    )
)

(defun f(x &rest y)
    (cond
    ((null y) x)
    (t (append x (mapcar #'car y)))
    )
)

(print (append (f '(1 2)) (f '(3 4) '(5 6) '(7 8))))