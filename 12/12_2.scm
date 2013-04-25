#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define sum-tree
  (lambda (tree)
    (cases bintree tree
      (interior-node (key left right)
                     (+ (sum-tree left) (sum-tree right)))
      (leaf-node (num) 
                 num))))

(define print-tree
  (lambda (tree)
    (cases bintree tree
      (interior-node (key left right)
                     (eopl:printf "interior-node ~a\n" key)
                     (print-tree left)
                     (print-tree right)
                     )
      (leaf-node (num)
                 (eopl:printf "leaf-node ~a\n" num))
      )
    )
  )

(define _max-interior
  (lambda (tree)
    (let ((max (cons 'null 0)))
      (cases bintree tree
        (interior-node (key left right)
                       (let ((left-sum (sum-tree left)))
                         (let ((right-sum (sum-tree right)))
                           (let ((sum (+ left-sum right-sum)))
                             (if (> sum (cdr max))
                                 (set! max (cons key sum))
                                 0))))

                       (let ((left-max (_max-interior left)))
                         (let ((right-max (_max-interior right)))
                           (if (and (pair? left-max) (> (cdr left-max) (cdr max)))
                               (set! max left-max)
                               0)
                           (if (and (pair? right-max) (> (cdr right-max) (cdr max)))
                               (set! max right-max)
                               0)))
                       )
        (leaf-node (num) num))
      max
      )
    )
  )

(define max-interior
  (lambda (tree)
    (car (_max-interior tree))
    )
  )

(define tree-1 (interior-node `foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node `bar (leaf-node -1) tree-1))
(define tree-3 (interior-node `baz tree-2 (leaf-node 1)))
;(define tree-4 (interior-node `baz tree-2 tree-s))
(if (eq?  (max-interior tree-1) (string->symbol "foo"))
    (eopl:printf "tree-1 PASS\n")
    (eopl:printf "tree-1 FAIL! Expected 'foo' Got ~a\n" (max-interior tree-1)))
    
(if (eq?  (max-interior tree-2) (string->symbol "foo"))
    (eopl:printf "tree-2 PASS\n")
    (eopl:printf "tree-2 FAIL! Expected 'foo' Got ~a\n" (max-interior tree-2)))
    
(if (eq?  (max-interior tree-3) (string->symbol "baz"))
    (eopl:printf "tree-3 PASS\n")
    (eopl:printf "tree-3 FAIL! Expected 'baz' Got ~a\n" (max-interior tree-3)))