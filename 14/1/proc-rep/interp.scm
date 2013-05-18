(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the PROC language, using the procedural
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry. 

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (call-exp (rator rands)
                  ;(eopl:printf "call-exp rator= ~a\n" rator)
                  ;(eopl:printf "call-exp rands= ~a\n" rands)
          (let ((proc (expval->proc (value-of rator env))))
            ;(eopl:printf "mapped rands: ~a\n" (map (lambda (rand) (value-of rand env)) rands))
            (apply-procedure proc
                             (map (lambda (rand)
                                    (value-of rand env))
                                  rands))
            )
          )
        )))


  ;; procedure : {Var}* * Exp * Env -> Proc
  (define procedure
    (lambda (vars body env)
      (lambda (vals)
        ;(eopl:printf "procedure vars= ~a\nprocedure vals= ~a\n" vars vals)
        (value-of body (extend-all vars vals env)))))
  
  ;; apply-procedure : Proc * {ExpVal}* -> ExpVal
  (define apply-procedure
    (lambda (proc vals)
      ;(eopl:printf "apply-procedure proc= ~a\napply-procedure vals= ~a\n" proc vals)
      (proc vals)))

  ;; extend-all: extends the environment with multiple vars:vals,
  ;; such that each pair extends the recently-extended-environment,
  ;; so that extending parameters in a procedure is done like it was
  ;; done with the Currying technic ==> each nested lambda extends by one parameter.
  (define extend-all
    (lambda (vars vals oldenv)
      (if (and (pair? vars) (pair? vals) )
          (extend-all 
           (cdr vars) 
           (cdr vals) 
           (extended-env-record (car vars) (car vals) oldenv))
          oldenv)
          )
      )
  )