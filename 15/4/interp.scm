(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the IMPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of-sta instrument-let instrument-newref)

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
          (value-of-sta statement (init-env))))))

  ;; value-of : Sta * Env -> ExpVal
  ;; Page: 122, 123
  (define value-of-sta
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
                         (if (pair? statements)
                             (execute-anything-within-block-of statements env)
                             (eopl:printf "Darn!")))
        
        (if-statement (exp sta1 sta2)
                      (if (value-of-sta sta1 env)
                          (value-of-sta sta1 env)
                          (value-of-sta sta2 env)))
        
        (while-statement (exp sta)
                         (while-statement-helper exp sta env))
        
        (var-statement (vars body-statement)
                       (if (pair? vars)
                           (var-statement-helper vars body-statement env)
                           (eopl:error "Define at least one var!")))
        )
      ))
  
  (define while-statement-helper
    (lambda (exp sta env)
      (let ((result (value-of-exp exp env)))
        (if (instrument-let)
            (eopl:printf "expression '~a' result: '~a'\n" exp result))
        (if result
            (begin
              (value-of-sta sta env)
              (while-statement-helper exp sta env)))
        )
      )
    )

  (define execute-anything-within-block-of
    (lambda (statements env)
      (if (pair? statements)
          (begin
             (value-of-sta (car statements) env)
             (execute-anything-within-block-of (cdr statements) env))
          )
      )
    )
  
  (define var-statement-helper
    (lambda (vars body-statement env)
      (if (pair? vars)
          (var-statement-helper
             (cdr vars)
             body-statement
             (extend-env (car vars) (newref (num-val 888)) env))
          (value-of-sta body-statement env))
      )
    )
      
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
        
        (add-exp (exp1 exp2)
                 (let ((val1 (value-of-exp exp1 env))
                       (val2 (value-of-exp exp2 env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (num-val
                      (+ num1 num2)))))

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
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of-exp rator env)))
                (arg (value-of-exp rand env)))
            (apply-procedure proc arg)))

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
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
          (let ((r (newref arg)))
            (let ((new-env (extend-env var r saved-env)))
              (if (instrument-let)
                (begin
                  (eopl:printf
                    "entering body of proc ~s with env =~%"
                    var)
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
  


  
