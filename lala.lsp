; se da o lista formata din numere si atomi
; sa se determine lista elementelor divizibile cu 3 din lista initiala
; de la orice nivel, in ordine inversa si o singura data

; map, mapcar in principal

(defun div3 (lst)
	(cond
		((null lst) nil)
		((and (numberp lst) (equal (mod lst 3) 0)) (list lst))
		((listp lst) (mapcan 'div3 lst))
	)
)

(defun reverseLst (lst)
	(cond
		((null lst) nil)
		(t (append (reverseLst (cdr lst)) (list (car lst))))
	)
)

(defun solve6(lst)
    (reverseLst(div3 lst))
)

;Write a function to check if an atom is member of a list (the list is non-liniar)
;check(l1..ln, a) = 0, n = 0
;                   1, l1 is an atom and l1 = a 
;                   check(l1) && check(l2..ln), l1 is a list 
;                   check(l2...ln), otherwise

(defun check2(l a)
    (cond 
    ((null l) 0)
    ((and (atom (car l)) (equal (car l) a)) 1)
    ((listp (car l)) (or (check2 (car l) a) (check2 (cdr l) a)))
    (T (check2 (cdr l) a))
    )
)

(defun check(l a)
    (cond 
    ((null l) 0)
    ((and (atom l) (equal l a)) 1)
    ((and (atom l) (not (equal l a))) 0)
    ((numberp l) 0)
    (T (apply '+ (mapcar #'(lambda (el) (check el a)) l)))
    )
)

(defun wrapper(l a)
    (cond 
    ((null l) nil)
    ((> (check l a) 0) 1)
    (t 0)
    )
)

;Write a function that returns the sum of numeric atoms in a list, at any level.;

;. Define a function to tests the membership of a node in a n-tree represented as (root list_of_nodes_subtree1 ... list_of_nodes_subtreen)
;Eg. tree is (a (b (c)) (d) (E (f))) and the node is "b" => true
;exactly like check function from above

;Write a function that returns the product of numeric atoms in a list, at any level.
(defun product(l)
    (cond
    ((null l) 1)
    ((numberp l) l)
    ((atom l) 1)
    (T (apply '* (mapcar #'product l)))
    )
)

;Write a function that computes the sum of even numbers and the decrease the sum of odd numbers, at any level of a list.
(defun evenSum(l)
    (cond
    ((null l) 0)
    ((and (numberp l) (equal (mod l 2) 0)) l)
    ((and (numberp l) (not (equal (mod l 2) 0))) 0)
    ((atom l) 0)
    (T (apply '+ (mapcar #' evenSum l)))
    )
)

;Write a function that returns the maximum of numeric atoms in a list, at any level.
(defun maxNum(l)
    (cond
    ((null l) 0)
    ((numberp l) l)
    ((atom l) 0)
    ((listp l) (apply 'max (mapcar #' maxNum l)))
    )
)

;Write a function that substitutes an element E with all elements of a list L1 at all levels of a given list L.
(defun substitutes(lst e l1)
    (cond 
    ((and (atom lst) (equal lst e)) l1)
    ((atom lst) lst)
    ((numberp lst) lst)
    (t (mapcar #'(lambda(a) (substitutes a e l1)) lst))
    )
)

;Write a function to determine the number of nodes on the level k from a n-tree represented as follows: (root list_nodes_subtree1 ... ;list_nodes_subtreen)
;Eg: tree is (a (b (c)) (d) (e (f))) and k=1 => 3 nodes


;Write a function that reverses a list together with all its sublists elements, at any level.


; Sa se elimine toate aparitiile unui element E dintr-o lista neliniara
;removeEl(l1...ln, e) = [], n = 0
;                       l1 U removeEl(l2...ln), l1 is not a list and l1 != e 
;                       removeEl(l1) U removeEl(l2...ln), l1 is a list 
;                       removeEl(l2..ln), l1 = e 
(defun removeE (lst e)
	(cond
		((null lst) nil)
		((and (numberp (car lst)) (= (car lst) e)) (removeE (cdr lst) e))
		((listp (car lst)) (cons (removeE (car lst) e) (removeE (cdr lst) e)))
		(T (cons (car lst) (removeE (cdr lst) e)))
	)
)

; Se da o lista neliniara
; Sa se scrie un program LISP pentru determinarea numarului
; de subliste de la orice nivel pentru care suma atomilor
; numerici de la nivelurile impare este numar par
; nivelul superficial al listei se considera 1.
; Prelucrarea se va face folosind o functie MAP

(defun sum (lst)
	(cond
		((null lst) 0)
		((numberp (car lst)) (+ (car lst) (sum (cdr lst))))
		;((listp (car lst)) (+ (sum (car lst)) (sum (cdr lst))))
		(T (sum (cdr lst)))
	)
)

(defun sum2 (elem)
	(cond
		((numberp elem) elem)
		(T (apply '+ (mapcar 'sum2 elem)))
	)
)

(defun parity (num)
	(cond
		(( = (mod num 2) 0) 1)
		(T 0)
	)
)

(defun solve (elem)
	(cond
		((atom elem) 0)
		
		; if it's not an atom, then it's a list
		(( = (parity (sum elem)) 1) (+ 1 (apply '+ (mapcar 'solve elem))))
		(T (mapcar 'solve elem))
	)
)

(defun main (lst)
	(cond
		(T (sum2 (solve lst)))
	)
)


;. Write a function that returns the number of atoms in a list, at any level.

;nrAt(l1...ln) = 0, n = 0
;                1 + nrAt(l2..ln), l1 is an atom 
;                nrAt(l1) + nrAt(l2..ln), otherwise

(defun nrAt(lst)
    (cond 
    ((null lst) 0)
    ((atom lst) 1)
    (t (apply '+ (mapcar #'nrAt lst)))
    )
)

;Write a function that reverses a list together with all its sublists elements, at any level.

;reverse(l1...ln) = nil, n = 0
;                   reverse(l2..ln) U l1, l1 is an atom 
;                   reverse(l2..ln) U reverse(l1), otherwise

(defun reverseL (lst)
	(cond
		((null lst) nil)
		((atom (car lst)) (append (reverseL(cdr lst)) (list (car lst))))
		(T (append (reverseL(cdr lst)) (list(reverseL (car lst)))))
	)
)



(defun reverseLC (lst)
	(cond
		((list lst) (reverseL lst))
		(T (apply 'append (mapcar 'reverseLC lst)))
	)
)

;sa se stearga dintr-o lista neliniara toate sublistele care au un nr par de elemente.
;nrElem(l1...ln) = 0, n = 0
;                  1 + nrElem(l2..ln), if l1 is an atom 
;                  nrElem(l1) + nrElem(l2..ln), if l1 is a list 

(defun nrElem(lst)
    (cond 
    ((null lst) 0)
    ((atom (car lst)) (+ 1 (nrElem (cdr lst))))
    (t (nrElem (cdr lst)))
    )
)

;remove(l1...ln) = [], n = 0
;                  l1 U remove(l2..ln), l1 is an atom or l1 is a list with odd nr of elems 
;                  remove(l2..ln), otherwise

(defun remove2(l)
    (cond 
    ((null l) nil)
    ((atom l) l)
    ((and (listp l) (equal (mod (nrElem l) 2) 1)) (remove2 l))
    )
)

(defun main2(lst)
    (mapcar #'remove2 lst)
)

(defun removeLst (lst)
	(cond 
		((null lst) nil)
		((atom (car lst)) (cons (car lst) (removeLst(cdr lst))))
		(( equal (mod (nrElem(car lst)) 2) 1) (cons (removeLst(car lst)) (removeLst(cdr lst))))
		(T (removeLst(cdr lst)))
	)
)

;sa se determine in ordine inversa toate elementele numerice unice din lista data
;nrAp(l1..ln, el) = 0, n = 0
;                   nrAp(l2..ln), l1 is a list or l1 is an atom and l1 != el 
;                   1 + nrAp(l2..ln), otherwise

(defun nrAp(l e)
    (cond 
    ((null l) 0)
    ((and (atom l) (equal l e)) 1)
    ((atom l) 0)
    (T (apply '+ (mapcar #'(lambda (el) (nrAp el e)) l)))
     )
)

(defun flattenList (lst)
	(cond
		((null lst) nil)
		((numberp lst) (list lst))
		((listp lst) (mapcan 'flattenList lst))
	)
)

(defun flat(l)
    (flattenList l)
)

;solvee(l) = [], n = 0
;            


;myGcd(l1, l2) = l1, l2 = 0
;                myGcd(l2, l1 % l2), otherwise

(defun myGcd(a b)
    (cond 
    ((equal b 0) a)
    (t (myGcd b (mod a b)))
    )
)

;provideList(l1..ln, level) = [], n = 0
;                             l1 U provideList(l2..ln, level), level % 2 = 1 && numberp(l1) && l1 % 2 = 0
;                             provideList(l1, level + 1) U provideList(l2..ln, level), l1 is a list 

(defun provideList(l level)
    (cond 
    ((null l) nil)
    ((and (equal (mod level 2) 0) (numberp (car l)) (equal (mod (car l) 2) 1)) (cons (car l) (provideList (cdr l) level)))
    ((listp (car l)) (cons (provideList (car l) (+ 1 level)) (provideList (cdr l) level)))
    (t (provideList (cdr l) level))
    )
)

(defun solve13 (l)
    (flattenList (provideList l 1))
)

(defun gcdOnList(l)
    (cond 
    ((null l) 0)
    (t (myGcd (car l) (gcdOnList (cdr l))))
    )
)

(defun result(l)
    (gcdOnList (solve13 l))
)

;getList(l1..ln, k, kids, aux) = [], n = 0
;                                  , aux < k 

(defun f(l1 l2)
    (append (f (car l1) l2) 
            (cond 
            ((null l1) (cdr l2))
            (t (list (f (car l1) l2) (car l2)))
            )
    )
)

(defun remove3 (lst)
	(cond
		((null lst) nil)
		((and (numberp (car lst)) (equal (mod (car lst) 3) 0)) (remove3 (cdr lst)))
		((listp (car lst)) (cons (remove3 (car lst)) (remove3 (cdr lst))))
		(T (cons (car lst) (remove3 (cdr lst))))
	)
)

(defun rem3(l)
    (cond
    ((null l) nil)
    ((and (atom l) (not (numberp l))) l)
    ((and (numberp l) (not (equal (mod l 3) 0))) l)
    ((listp (mapcan #'rem3 l)))
    )
)

(defun removeMod3(l)
    (mapcan #'(lambda (el)
        (cond
        ((null el) (list nil))
        ((consp el) (list (removeMod3 el)))
        ((and (numberp el) (equal (mod el 3) 0)) nil)
        (t (list el))
        )
     )
     l)
)



;R2-p4
;????????????????
(defun maxim(l)
    (cond
    ((null l) 0)
    ((and (numberp (car l)) (> (car l) (maxim (cdr l)))) (car l))
    (t (maxim (cdr l)))
    )
)


(defun removeList(l level)
    (mapcan #'(lambda (el)
        (cond 
        ((null el) (list nil))
        ((consp el) (list (removeList el (+ 1 level ))))
        ((and (listp el) (equal (mod (maxim el) 2) 0)) nil)
        (t (list el))
        )
    )
    l)
)

;R3-p5
(defun minodd(x lvl)
    (cond
        ((AND (numberp x) (= (mod lvl 2) 1)) x)
        ((atom x) 999999)
        (t (apply 'min (mapcar (lambda (el) (minodd el (+ 1 lvl))) x)))
    )
)

(defun check11(l)
    (cond
    ((= (mod (minodd l 0) 2) 0) 1)
    (t 0)
    )
)

(defun solve11(l)
    (cond
        ((atom l) 0)
        (t (+ (check11 l) (apply '+ (mapcar 'solve l)))
        )
    )
)

;R6-p5 
;firstOdd(l1..ln, first) = 0, n = 0
;                          1 + firstOdd(l2..ln, first + 1), first = 0 && l1 is a number && l1 % 2 == 1 && firstOdd(l2..ln) > 0
;                          firstOdd(l2..ln, first), first = 1 || (first = 0 && l1 is an non-numerical atom)
;                          firstOdd(l1, 0) + firstOdd(l2..ln, first), l1 is a list 
;                          firstOdd(l2...ln, first + 1) - 1, first = 0 && l1 is a number && l1 % 2 = 0 

;(defun firstOdd(l first)
;    (cond
;    ((null l) 0)
;   ((and (equal first 0) (numberp (car l)) (equal (mod (car l) 2) 1)) (+ 1 firstOdd (cdr l) (+ 1 first)))
;    ((or (equal first 1) (and (equal first 0) (not (numberp (car l))))) (firstOdd (cdr l) first))
;    ((and (equal first 0) (numberp (car l)) (equal (mod (car l) 2) 0)) (- 1 firstOdd (cdr l) (+ 1 first)))
;    ((listp (car l)) (+ (firstOdd (car l) 0) (firstOdd (cdr l) first)))
;    )
;)

(defun firstOdd(l first)
    (cond
    ((null l) 0)
    ((and (numberp l) (equal first 0) (equal (mod l 2) 1)) 1)
    ((and (numberp l) (equal first 0) (equal (mod l 2) 0)) -1)
    ((listp l) (apply '+ (mapcar #'(lambda (a) (firstOdd a first) ) l)))
    (t 0)
)
)    
    
(defun sumOdd(x k)
    (cond
    ((and (numberp x) (equal (mod k 2) 0)) 0)
    ((numberp x) x)
    ((atom x) 0)
    (t (apply '+ (mapcar (lambda (el) (sumOdd el (+ 1 k))) x)))
    )
)

(defun newCheck(l)
    (- 1 (mod (sumOdd l 0) 2))
)

(defun cnt(l)
    (cond
    ((atom l) 0)
    (t (+ (newCheck l) (apply '+ (mapcar #'cnt l))))
    )
)


(defun Vasi(lst lvl)
    (cond
		((null lst) 0)	
		((atom lst) 0)
		((and (= (mod lvl 2) 0) (and (= ( mod (maxim lst) 2) 0) (listp (car lst)))) (+ 1 (Vasi lst (+ 1 lvl)))) 
		((and (= (mod lvl 2) 0) (= ( mod (maxim lst) 2) 0))  1)
		(t (apply '+ (mapcar (lambda (el) (Vasi el (+ 1 lvl))) lst)))
    )
)


(defun firstAtom(l)
    (cond 
    ((null l) nil)
    ((numberp (car l)) (car l))
    (t (firstAtom (cdr l)))
    )
)

(defun transform(l)
    (cond
    ((numberp l) (list l))
    ((atom l) nil)
    (t (apply 'append (mapcar #'transform l)))
    )
)

;R11 and R10 - problem 5

(defun checkNr(l)
    (setq l (transform l))
    (cond
    ((null l) 0)
    ((= (mod (car l) 2) 1) 1)
    (t 0)
    )
)

(defun findNr(l)
    (cond
    ((atom l) 0)
    (t (+ (checkNr l) (apply '+ (mapcar #'findNr l))))
    )
)


(defun checkLast(l)
    (setq l (transform l))
    (cond
    ((null l) 0)
    ((and (null (cdr l)) (= (mod (car l) 2) 1)) 1)
    (t (checkLast (cdr l)))
    )
)

(defun findLast(l)
    (cond
    ((atom l) 0)
    (t (+ (checkLast l) (apply '+ (mapcar #'findLast l))))
    )
)

;(write (findLast '(A (B 2) (1 C 4) (D 1 (9 F)) ((G 7) 6))))

(defun removeOccurence (elem elemToBeRemoved)
	(cond
		((and (atom elem) (/= elem elemToBeRemoved)) elem)
		((listp elem) (mapcar (lambda (x) (removeOccurence x elemToBeRemoved)) elem))
	)
)


 (defun my_append ( l k)
	(if (null l)
		k 
		(cons (car l) (my_append (cdr l) k))
	)
)

(defun my_reverse (l) 
	(cond 
		((null l) nil)
		((listp (car l)) (my_append (my_reverse (cdr l)) (list (my_reverse (car l)))))
		(t (my_append (my_reverse (cdr l)) (list (car l))))
	)
)

(defun g(l)
    (R (my_reverse l))
)

(defun R (lst)
	(cond
		((null lst) 0)
		((atom lst) 0)
		((numberp (car lst)) ( + 1 (R (cdr lst))))
		((listp (car lst)) (R (cdr lst)))
		(t (apply '+ (mapcar (lambda(el) (R el)) lst)))
	)
)

(defun r10_solve(l)
	(cond 
		((null l) 0)
		((atom l) 0)
		((and (numberp (car l)) (equal (mod (car l) 2) 1)) (+ 1 (r10_solve (cdr l))))
		((listp (car l)) (r10_solve (cdr l)))
		(t (apply '+ (mapcar (lambda(el) (r10_solve el))(my_reverse l))))
	)
)


(defun P (lst)
	(cond
		((null lst) 0)
		((atom lst) 0)
		((numberp (car lst)) (if (= (mod (car lst) 2) 1)( + 1 (P (cdr lst))) (P (cdr lst))))
		((atom (car lst)) (P (cdr lst)))
		(t (apply '+ (mapcar(lambda(el) (P el))  lst)))
	)
)

     
;(print (P (my_reverse '(A (B 2) (1 C 4) (D 1 (9 F)) ((G 7) 6)))))

;(print (r10_solve '(A (B 2) (1 C 4) (D 1 (9 F))((G 7)6))))
;(print (g '(A (B 2) (1 C 4) (D 1 (9 F))((G 7)6))))


;(setq h 'g)

(defun my_replace(l lvl e)
    (cond
    ((null l) 0)
    ((and (atom l) (if (equal (mod lvl 2) 0) e l)))
    ((listp l) (mapcar #'(lambda (a) (my_replace a (+ 1 lvl) e)) l))
    )
)

(defun ding(l)
    (cond 
    ((null l) nil)
    ((and (numberp l) (not (equal (mod l 3) 0))) (list l))
    ((listp l) (list (mapcan 'ding l)))
    ((and (atom l) (not (numberp l))) (list l))
    )
)


(defun test(l)
    (cond 
    ((null l) nil)
    ((and (numberp l) (not (equal (mod l 3) 0))) l)
    ((and (atom l) (not (numberp l))) l)
    ((listp l) (mapcon #'test l))
    )
)

























