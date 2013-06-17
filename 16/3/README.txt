(let-exp v e1 b) :  

T(v) = T(e1)
T(let-exp v e1 b) = T(b)

The rest of the rules like in pages 249-250.


1. let x=4 in (x 3)
	T(x) = T(4) = int
	T(let x=4 in (x 3)) = T((x 3)) = T( (int int) )
	Type error - int not a procedure.

2. let f = proc (z) z in proc (x) -((f x), 1)
	T(f) = T(proc(z)) = T(T(z) -> T(z)) = T(z) -> T(z)
	T(let f = proc (z) z in proc (x) -((f x), 1)) = T(proc (x) -((f x), 1))
	= T(T(x) -> T(-((f x), 1))) = T(T(x) -> T(-(
3. let p = zero?(1) in if p then 88 else 99
4. let p = proc (z) z in if p then 88 else 99