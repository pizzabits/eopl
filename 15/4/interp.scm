(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of-statement instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)
      (cases program pgm
        (a-program (statement)
          (value-of-statement statement (init-env))))))

  ;; value-of : Sta * Env -> ExpVal
  ;; Page: 122, 123
  (define value-of-statement
    (lambda (sta env)
      (cases statement sta

        (assign-statement (var exp)
          (begin
            (setref!
              (apply-env env var)
              (value-of-exp exp env))))
        
        (print-statement (exp)
          (eopl:printf "~a\n" (expval->num (value-of-exp exp env))))
        
        (block-statement (statements)
          (for-each 
            (lambda (statement)
              (value-of-statement statement env))
            statements))
        
        (if-statement (exp sta1 sta2)
          (let ((result (expval->bool (value-of-exp exp env))))
            (if (instrument-let)
                (eopl:printf "expression '~a' result: '~a'\n" exp result))
            (if result
              (value-of-statement sta1 env)
              (value-of-statement sta2 env))))
        
        (while-statement (exp sta)
          (while-statement-helper exp sta env))
        
        (var-statement (vars body-statement)
          (value-of-statement
           body-statement
           (foldr
            (lambda (var env)
              (extend-env var (newref (num-val 888)) env))
            env vars)))        
        )))
  
  (define while-statement-helper
    (lambda (exp sta env)
      (let ((result (value-of-exp exp env)))
        (if (instrument-let)
          (eopl:printf "expression '~a' result: '~a'\n" exp result))
        (if result
          (begin
            (value-of-statement sta env)
            (while-statement-helper exp sta env)))
        )))
  
  ;; Helper procedures - I LOVE FOLDR
  (define foldr
    (lambda (func accumulated lst)
      (if (null? lst)
        accumulated
        (func (car lst) (foldr func accumulated (cdr lst))))))
  
  (define foldr2
    (lambda (func accumulated lst1 lst2)
      (if (or (null? lst1) (null? lst2))
        accumulated
        (func (car lst1) (car lst2) (foldr2 func accumulated (cdr lst1) (cdr lst2))))))
  
  (define apply-two-operands-function
    (lambda (rand1 rand2 f env)
      (let ((v1 (value-of-exp rand1 env))
            (v2 (value-of-exp rand2 env)))
        (let ((num1 (expval->num v1))
              (num2 (expval->num v2)))
          (num-val
           (f num1 num2))))))
    
  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of-exp
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) 
        ;              = (deref (apply-env \r \x{}))}
        (var-exp (var) (deref (apply-env env var)))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of-exp exp1 env))
                (val2 (value-of-exp exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
        
        (add-exp (e1 e2)
          (apply-two-operands-function e1 e2 + env))
        
        (multiply-exp (e1 e2)
          (apply-two-operands-function e1 e2 * env))
        
        (not-exp (exp)
          (let ((result (expval->bool (value-of-exp exp env))))
            (if (instrument-let)
              (eopl:printf "expression '~a' result: '~a'\n" exp result))
            (if result
               #f
               #t)))
        
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of-exp exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of-exp exp1 env)))
            (if (expval->bool val1)
              (value-of-exp exp2 env)
              (value-of-exp exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((v1 (value-of-exp exp1 env)))
            (value-of-exp body
              (extend-env var (newref v1) env))))
        
        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (call-exp (rator rands)
          (let ((proc (expval->proc (value-of-exp rator env)))
                (arguments (map (lambda (rand) (value-of-exp rand env)) rands)))
            (apply-procedure proc arguments)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of-exp letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of-exp e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (assign-exp (var exp1)
          (begin
            (setref!
              (apply-env env var)
              (value-of-exp exp1 env))
            (num-val 27)))

        )
      ))


  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 119

  ;; uninstrumented version
  ;;  (define apply-procedure
  ;;    (lambda (proc1 val)
  ;;      (cases proc proc1
  ;;        (procedure (var body saved-env)
  ;;          (value-of body
  ;;            (extend-env var (newref val) saved-env))))))
  
  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arguments)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((locations_of_args (map newref arguments)))
            (let ((new-env (foldr2 extend-env saved-env vars locations_of_args)))
              (if (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    vars)
                  (pretty-print (env->list new-env)) 
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of-exp body new-env)))))))

  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (list
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
  


  
