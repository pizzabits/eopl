(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
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

        ; original definition of let-exp:
        ;\commentbox{\ma{\theletspecsplit}}
       ; (let-exp (var exp1 body)
       ;          (eopl:printf "var: ~s\n" var)
       ;          (eopl:printf "exp1: ~s\n" exp1)
       ;          (eopl:printf "BODY: ~s\n" body)
       ;   (let ((val1 (value-of exp1 env)))
       ;     (value-of body
       ;       (extend-env var val1 env))))
    ;)
  ;)
   ; )
        
        ; multiple variables and bodies let-exp:
        (let-exp (vars exps body)
                ; (eopl:printf "VARS: ~s\n" vars)
                ; (eopl:printf "EXPS: ~s\n" exps)
                ; (eopl:printf "BODY: ~s\n" body)
                ; (eopl:printf "EXT ENV: ~s\n" (extend-all vars exps env env))
                 (value-of body (extend-all vars exps env env)))
        )
      )
    )
   
  (define extend-all
    (lambda (vars vals oldenv newenv)
      (if (and (pair? vars) (pair? vals) )
          (extend-all 
           (cdr vars) 
           (cdr vals) 
           oldenv 
           (extended-env-record
            (car vars)
            (value-of (car vals) oldenv)
            newenv))
          newenv)
          )
      )
  )
;