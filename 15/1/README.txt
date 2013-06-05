Add an operation list to EXPLICIT-REFS language, similar to Exercise 3.10 in page 73.
This operation should take any number of arguments, 
and return an expressed value containing the list of their values.

For example,
let x = 4
in list(x, -(x,1), -(x,3))
should return an expressed value that represents the list (4 3 1).


a. Write an algebric definition for the list operation, similar to the definitions in pages 107-108.

(value-of list(exp1, exp2, ..., expn) p q) 
 = ( (val1, q1), (val2, q2), ..., (valn, qn) )

b. Extend the language accordingly.

   To test, open run.scm and run.