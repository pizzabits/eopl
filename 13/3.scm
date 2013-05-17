; uses PROC language definitions

;3.a
(run "let sum = proc(x)
                  proc(y) 
                    -(x,-(0,y))
                       in ((f 3) 4)")

;3.b
; 4 + 4 + 4 = 12

;3.c
(run "let mult = proc (maker)
                    proc (number)
                      proc (multiplier)
                        if zero? (number)
                        then 0
                        else -((((maker maker) -(number,1)) multiplier), -(0, multiplier))
        in let times = proc(x) proc(y) (((mult mult) x) y)
           in let fact = proc (maker)
                           proc (number)
                             if zero? (number)
                             then 1
                             else ((times ((maker maker) -(number,1))) number)
               in ((fact fact) 6)")
