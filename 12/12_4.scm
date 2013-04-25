#lang eopl

;; Question א
(define number->sequence 
  (lambda (num)
    (cons num (list '() '()))
    )
  )

;; Question ב
(define current-element
  (lambda (lst)
    (car lst)
    )
  )

;; helper function: returns current, left list and right list.
(define flatten-two-way-list
  (lambda (lst)
    (let ((current (current-element lst)))
      (let ((left-list (cadr lst)))
        (let ((right-list (caddr lst)))
          (list current left-list right-list)
          )))
    )
  )

;; Question ז - defined earlier because of its use in Question ג
(define at-left-end?
  (lambda (lst)
    (let ((flat (flatten-two-way-list lst)))
      (eq? (cadr flat) '())
      )
    )
  )

;; Question ח - defined earlier because of its use in Question ד
(define at-right-end?
  (lambda (lst)
    (let ((flat (flatten-two-way-list lst)))
      (eq? (caddr flat) '())
      )
    )
  )

;; Question ג
(define move-to-left 
  (lambda (lst)
    (let ((flat (flatten-two-way-list lst)))
      (if (at-left-end? lst)
          (eopl:error "Cannot move anymore left from the begining of the list!")
          (cons (car (cadr flat)) (list (cdr (cadr flat)) (cons (car flat) (caddr flat)))))
      )
    )
  )

;; Question ד
(define move-to-right
  (lambda (lst)
    (let ((flat (flatten-two-way-list lst)))
      (if (at-right-end? lst)
          (eopl:error "Cannot move anymore right from the end of the list!")
          (cons (car (caddr flat)) (list (cons (car flat) (cadr flat)) (cdr (caddr flat)))))
      )
    )
  )

;; Question ה
(define insert-to-left
  (lambda (number lst)
    (let ((flat (flatten-two-way-list lst)))
      (cons (car flat) (list (cons number (cadr flat)) (caddr flat))))
    )
  )


;; Question ו
(define insert-to-right
  (lambda (number lst)
    (let ((flat (flatten-two-way-list lst)))
      (cons (car flat) (list (cadr flat) (cons number (caddr flat))))
      )
    )
  )


;; Tests
(number->sequence 7)
(current-element `(6 (5 4 3) (7 8 9)))

;; should #t
(at-left-end? `(6 () (7 8 9)))
;; should #f
(at-left-end? `(6 (5 4 3) (7 8 9)))

;; should #t
(at-right-end? `(6 (5 4 3 2 1) ()))
;; should #f
(at-right-end? `(6 (5 4 3) (7 8 9)))

(move-to-left `(6 (5 4 3 2 1) (7 8 9)))
(move-to-right `(6 (5 4 3 2 1) (7 8 9)))

(insert-to-left 13 `(6 (5 4 3 2 1) (7 8 9)))
(insert-to-right 13 `(6 (5 4 3 2 1) (7 8 9)))