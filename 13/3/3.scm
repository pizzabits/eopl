;; uses PROC language definitions
;; these tests exists in the DS-representation implementation's,
;; as well as in the procedures-representation implementation's tests.

;; 3.a
(run "let sum = proc(x)
                  proc(y) 
                    -(x,-(0,y))
                       in ((sum 3) 4)")

;; 3.b
;; As we know, -(x, -(0,y)) <=> x+y
;; With that in mind, lets observe the makemult definition:
;; it gets a maker as a parameter, which will be called with the same method,
;; to be able to call it again later and (using Currying technic) another parameter, x.

;; Now we check if x is zero, if it is then we return zero.
;; Otherwise, we actually sum up the result of maker(x-1, 4)
;; So what will happen is that we'll call ourself (*call maker*, which gets makemult proc)
;; from the bottom of the recursion with 0+4, and then 4+4, and thus further
;; until the number x is zeroed.
;; Having explained this, the program calls (times4 3) so the calculation will be:
;; 4 + 4 + 4 = 12
;; and the result is (of course) (num-val 12).

;; 3.c
(run "let mult = proc (maker)
                    proc (number)
                      proc (multiplier)
                        if zero? (number)
                        then 0
                        else -((((maker maker) -(number,1)) multiplier), -(0, multiplier))
        in let times = proc(x) proc(y) (((mult mult) x) y)
           in let factorial = proc (maker)
                           proc (number)
                             if zero? (number)
                             then 1
                             else ((times ((maker maker) -(number,1))) number)
               in ((factorial factorial) 6)")
