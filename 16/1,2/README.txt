Exercise 7.5 [**] Extend the checker to handle multiple let declarations, multiargu-
ment procedures, and multiple letrec declarations. You will need to add types of
the form (t1 * t2 * ... * tn -> t) to handle multiargument procedures.

Exercise 7.6 [*] Extend the checker to handle assignments (section 4.3).


Answers to both questions within, and also their check-tests.
To run the checks, open top.scm and press Run.

Running examples:


;; Exercise 7.5

> (check "letrec 
           int f(x : int, y : int, z : int) = 1
           bool is_zero(a : int, b : int) = zero?(a)
        in (is_zero (f 80 20 10) 1)")
'bool
      
> (check "let
            f = proc(x : int, y : int, z: int)
                  proc (y : int) 
                      -(x,-(y, z)) 
        in (f 1 2 3)" )
'((int) -> int)

> (check "let 
            f1 = proc(x : int, y : int, z: int)
                     -(x, z)
            f2 = proc (y : int)
                     -(y, 30)
        in (f1 (f2 1) 2 3)")
'int
      

;; Exercise 7.6

> (check "set int x = -(5,4);")
'int
      
> (check  "let 
            f1 = proc(x : int, y : int, z: int)                     
                     -(x, z)
            f2 = proc (y : int)
                     -(y, 30)
          in set bool b = zero?((f1 (f2 1) 2 3));")
'bool