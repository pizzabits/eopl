;Question 3.c
#lang eopl

(define-datatype stack-type stack-type?
  (empty-stack)
  (push (val integer?)
        (stack stack-type?))
  )

(define empty-stack-error
  (lambda ()
    (eopl:error 'eval-expression "Couldn't pop/top on an empty stack!")
    )
  )

(define pop
  (lambda (stack)
    (cases stack-type stack
      (empty-stack ()
                   (empty-stack-error))
      (push (top-val rest-of-stack) rest-of-stack)
      )
    )
  )

(define top
  (lambda (stack)
    (cases stack-type stack
      (empty-stack ()
                   (empty-stack-error))
      (push (top-val rest-of-stack) top-val)
      )
    )
  )

(define empty-stack?
  (lambda (stack)
    (cases stack-type stack
      (empty-stack () #t)
      (push (top-val rest-of-stack) #f)
      )
    )
  )

;; Tests
(define s3 (push 3 (empty-stack)))
(define s1_3 (push 1 s3))
(define six (+ (top (pop s1_3)) (top s3)))
(if (eq? 6 six) (eopl:printf "PASS\n") (eopl:printf "FAIL! Expected 6, Got: ~a\n" six))
(pop (empty-stack))