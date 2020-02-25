;13. Define a function that returns the depth of a tree represented as (root list_of_nodes_subtree1 ...list_of_nodes_subtreen)
;Eg. the depth of the tree (a (b (c)) (d) (e (f))) is 3

;depth(l1..ln, level) = level, n = 1 and l1 is an atom
;                      max (depth(li, level + 1)), (i=1- >n), otherwise

(defun depth(tree level)
    (cond 
    ((atom tree) level)
    ((listp tree) (apply 'max(mapcar(lambda (a) (depth a (+ 1 level))) tree)))
    )
)

;treeDepth(l1..ln) = depth(l1..ln, 0)
(defun treeDepth(tree)
    (depth tree 0)
)

(princ "Depth")
(print (treeDepth '(A(B)(C))))
(princ #\linefeed)
(princ "Depth2")
(print (treeDepth '(A(B(C))(D)(E(F)))))
(princ #\linefeed)