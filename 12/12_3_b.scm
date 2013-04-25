;Question 3.b
#lang eopl

(define empty-stack-error
  (lambda ()
    (eopl:error 'eval-expression "Couldn't pop/top on an empty stack!")
    )
  )

(define empty-stack (lambda () '()))
(define push (lambda (val stack) (cons val stack)))

(define pop (lambda (stack) 
              (if (empty-stack? stack)
                  (empty-stack-error)
                  (cdr stack))))

(define top (lambda (stack)
              (if (empty-stack? stack)
                  (empty-stack-error)
                  (car stack))))

(define empty-stack? (lambda (stack) (eq? stack '())))


(define EXPECT_1 (top (push 1 (push 8 (push 5 (empty-stack))))))
(if (eq? 1 EXPECT_1)
    (eopl:printf "Test 1 PASS\n")
    (eopl:printf "Test 1 FAIL : expected 1, Got: ~a\n" EXPECT_1))

(define EXPECT_LIST_8_5 (pop (push 1 (push 8 (push 5 (empty-stack))))))
(if (eq? '() (pop (pop EXPECT_LIST_8_5))) (eopl:printf "Test 2 PASS\n") (eopl:printf "Test 2 FAIL : expected (8 5), Got: ~a\n" EXPECT_LIST_8_5))

(define EXPECT_FALSE (empty-stack? (pop (pop (push 1 (push 8 (push 5 (empty-stack))))))))
(if (eq? #f EXPECT_FALSE) (eopl:printf "Test 3 PASS\n") (eopl:printf "Test 3 FAIL : expected #f, Got: ~a\n" EXPECT_FALSE))

(define EXPECT_EMPTY (empty-stack? (pop (pop (pop (push 1 (push 8 (push 5 (empty-stack)))))))))
(if (eq? #t EXPECT_EMPTY) (eopl:printf "Test 4 PASS\n") (eopl:printf "Test 4 FAIL : expected #t, Got: ~a\n" EXPECT_EMPTY))