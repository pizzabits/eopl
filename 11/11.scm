#lang racket

;Question 1
(define (cons-cell-count lst)
  (if (pair? lst)
      (+ 1 (+ (cons-cell-count (car lst)) (cons-cell-count (cdr lst))))
      0)
  )

;Question 2
(define (occurs-?s lst)
  (if (null? lst)
      0
      (if (eq? '? (car lst))
          (+ 1 (occurs-?s (cdr lst)))
          (occurs-?s (cdr lst)))
      )
  )

;Question 3
(define (filter predicat lst)
  (if (null? lst)
      lst
      (if (predicat (car lst))
          (cons (car lst) (filter predicat (cdr lst)))
          (filter predicat (cdr lst))
          )
      )
  )


;Question 4
(define (zip lst1 lst2)
  (if (null? lst1)
      lst1
      (cons (list (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))
      )
  )


;Question 5
(define (member-?* lst)
  (if (symbol? lst)
      (if (eq? lst '?)
          #t
          #f)
      (if (null? lst)
          #f
          (or (member-?* (car lst)) (member-?* (cdr lst))))
      )
  )

;Question 6
(define foo
  (lambda (ls s)
    (cond
      [(null? ls) `((). ,s)]			; empty list? 
      [(pair? (car ls))				; first element is a pair?
       (let ((p (foo (car ls) s)))		        ; assign p with foo(first elemnt of ls, s)
         (let ((p1 (foo (cdr ls) (cdr p))))	        ; assign p1 with foo(rest of ls's elements, s of p!)
           `(,(cons  (car p) (car p1)). ,(cdr p1))))]
      [(or (null? (car ls)) (odd? (car ls)))	;first elemnt of ls is odd or null?
       (let ((p (foo (cdr ls) s)))		        ; p gets other elements
         `(,(cons (car ls) (car p)) . ,(cdr p)))]
      [else (let ((p (foo (cdr ls) s)))		; p gets relevant elements and s
              `(,(car p) . ,(add1 (cdr p))))])))



;Tests

;1
(cons-cell-count `a)
(cons-cell-count `(3 . 4))
(cons-cell-count `(a b . c))
(cons-cell-count `((a b . c) 3 . 4))
(display "\n")

;2
(occurs-?s `(? Y z ? ?))
(display "\n")

;3
(filter even? `(1 2 3 4 5 6))
(display "\n")

;4
(zip `(1 2 3) `(a b c))
(display "\n")

;5
(member-?* `(a b c))
(member-?* `(a ? c))
(member-?* `((a ((?)) ((d) b c))))

(member-?* `(?))
(member-?* `())
(display "\n")

;6
(foo `(2 3 (7 4 5 6) 8 (9) 2) 0)
(display "\n")