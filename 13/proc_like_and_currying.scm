#lang eopl



; sum using currying
(define sum
  (lambda (x)
    (lambda (y)
      (+ x y)
      )
    )
  )

; makemult using (lambda)
(define makemult 
  (lambda (maker)
    (lambda (x)
      (if (zero? x)
          0
          (- ((maker maker) (- x 1)) -4)
          )
      )
    )
  )

; times using (lambda) and currying 
(define times
  (lambda (maker)
    (lambda (number)
      (lambda (multiplier)
        (if (zero? number)
            0
            ;(+ (((maker maker) (- number 1)) multiplier) multiplier)
            (- (((maker maker) (- number 1)) multiplier) (- 0 multiplier))
            )
        )
      )
    )
  )

; factorial using times method
(define fact 
  (lambda (number)
    (if (zero? number)
        1
        (((times times) (fact (- number 1))) number)
        )
    )
  )

