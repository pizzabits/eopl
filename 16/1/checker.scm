(module checker (lib "eopl.ss" "eopl")

  (require "drscheme-init.scm")
  (require "lang.scm")

  (provide type-of type-of-program)

  ;; check-equal-type! : Type * Type * Exp -> Unspecified
  ;; Page: 242
  (define check-equal-type!
    (lambda (ty1 ty2 exp)
      (if (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

  ;; report-unequal-types : Type * Type * Exp -> Unspecified
  ;; Page: 243
  (define report-unequal-types
    (lambda (ty1 ty2 exp)
      (eopl:printf "type1 = ~a\ntype2 = ~a\nexp = ~a\n" ty1 ty2 exp)
      (eopl:error 'check-equal-type!  
          "Types didn't match: ~s != ~a in~%~a"
          (type-to-external-form ty1)
          (type-to-external-form ty2)
          exp)))

  ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;
  
  ;; type-of-program : Program -> Type
  ;; Page: 244
  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1) (type-of exp1 (init-tenv))))))

  ;; type-of : Exp * Tenv -> Type
  ;; Page 244--246
  (define type-of
    (lambda (exp tenv)
      (cases expression exp

        ;; \commentbox{\hastype{\tenv}{\mv{num}}{\mathtt{int}}}
        (const-exp (num) (int-type))

        ;; \commentbox{\hastype{\tenv}{\var{}}{\tenv{}(\var{})}}
        (var-exp (var) (apply-tenv tenv var))

        ;; \commentbox{\diffrule}
        (diff-exp (exp1 exp2)
                  ;;(eopl:printf "env: ~a\n" tenv)
          (let ((ty1 (type-of exp1 tenv))
                (ty2 (type-of exp2 tenv)))
            (check-equal-type! ty1 (int-type) exp1)
            (check-equal-type! ty2 (int-type) exp2)
            (int-type)))

        ;; \commentbox{\zerorule}
        (zero?-exp (exp1)
          (let ((ty1 (type-of exp1 tenv)))
            (check-equal-type! ty1 (int-type) exp1)
            (bool-type)))

        ;; \commentbox{\condrule}
        (if-exp (exp1 exp2 exp3)
          (let ((ty1 (type-of exp1 tenv))
                (ty2 (type-of exp2 tenv))
                (ty3 (type-of exp3 tenv)))
            (check-equal-type! ty1 (bool-type) exp1)
            (check-equal-type! ty2 ty3 exp)
            ty2))

        ;; for example: let y = -(x,1) in -(x,y)"
        ;; \commentbox{\letrule}
        (let-exp (vars exps body)  ;; for example:  vars {y}, exps {-(x,1)}, body {-(x,y)}
                 (let ((exp-types (map (lambda (expression)
                                         (type-of expression tenv))
                                       exps)))
                   ;;(eopl:printf "body: ~a\n" body)
                   (type-of body (extend-all vars exp-types tenv))))

        ;; \commentbox{\procrulechurch}
        (proc-exp (vars var-types body)
                  ;;(eopl:printf "vars: ~a\nvar-types: ~a\nbody: ~a\n" vars var-types body)
          (let ((result-type
                  (type-of body
                           (extend-all vars var-types tenv))))
            ;;(eopl:printf "type of body is ~a" result-type)
                    ;;(extend-tenv vars var-types tenv))))
            (proc-type var-types result-type)))

        ;; \commentbox{\apprule}
        (call-exp (rator rands)
          (let ((rator-type (type-of rator tenv))
                (rand-types (map (lambda (rand)
                                   (type-of rand tenv))
                                 rands)))
           ;; (eopl:printf "rator-type ~a\nrand-types ~a\n" rator-type rand-types)
            (cases type rator-type
              (proc-type (arg-types result-type)
                (begin
                  (check-equal-type! arg-types rand-types rands)
                  result-type))
              (else
                (report-rator-not-a-proc-type rator-type rator)))))

        ;; \commentbox{\letrecrule}
        (letrec-exp (proc-result-types proc-names bound-vars-lists bound-vars-type-lists proc-bodies
                      letrec-body)
                  ;;  (eopl:printf "proc-result-types: ~a\nproc-names: ~a\nbound-vars-lists: ~a\nbound-var-types-lists: ~a\n"
                  ;;               proc-result-types             proc-names          bound-vars-lists       bound-vars-type-lists)
          (let ((tenv-for-letrec-body                                     
                 (extend-procs proc-names bound-vars-type-lists proc-result-types tenv)))
            (let ((proc-body-types
                   (map
                     (lambda (proc-body bound-vars bound-vars-types)
                       (type-of proc-body 
                                (extend-all bound-vars bound-vars-types tenv-for-letrec-body)))
                     proc-bodies bound-vars-lists bound-vars-type-lists)))
              (for-each 
                (lambda (proc-body-type proc-result-type proc-body)
                 ;; (eopl:printf "proc-body-type: ~a\nproc-result-type: ~a\nproc-body: ~a\n" proc-body-type proc-result-type proc-body)
                  (check-equal-type! proc-body-type proc-result-type proc-body))
                proc-body-types proc-result-types proc-bodies)
             ;; (eopl:printf "proc returns expected type.\ncontinue to check the type of letrec-body ~a\n" letrec-body)
              
              (type-of letrec-body tenv-for-letrec-body)))))))
          
  (define extend-procs
    (lambda (proc-names bound-vars-type-lists proc-result-types told-env)
       (if (null? proc-names)
           told-env
           (extend-procs (cdr proc-names) (cdr bound-vars-type-lists) (cdr proc-result-types)
                         (extend-tenv (car proc-names)
                                      (proc-type (car bound-vars-type-lists) (car proc-result-types))
                                      told-env)
     ))))
  
  (define extend-all
    (lambda (vars var-types told-env)
      (if (pair? vars)
          (extend-all (cdr vars) (cdr var-types)
                      (extend-tenv (car vars) (car var-types) told-env))
          told-env)))
  
;;  (define extend-procs2
;;    (lambda (proc-names bound-vars-type-lists proc-result-types old-env)
;;      (foldr 
;;       (lambda (proc-names bound-vars-type-lists proc-result-types old-env)
;;          (extend-tenv (car proc-names)
;;                       (proc-type (car bound-vars-type-lists) (car proc-result-types))))
;;        old-env
        

  (define foldr
    (lambda (func accumulated lst)
      (if (null? lst)
          accumulated
          (func (car lst) (foldr func accumulated (cdr lst))))))
  
  (define report-rator-not-a-proc-type
    (lambda (rator-type rator)
      (eopl:error 'type-of-expression
        "Rator not a proc type:~%~s~%had rator type ~s"   
           rator 
           (type-to-external-form rator-type))))

  ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;
    
  (define-datatype type-environment type-environment?
    (empty-tenv-record)
    (extended-tenv-record
      (sym symbol?)
      (type type?)
      (tenv type-environment?)))
    
  (define empty-tenv empty-tenv-record)
  (define extend-tenv extended-tenv-record)
    
  (define apply-tenv 
    (lambda (tenv sym)
      (cases type-environment tenv
        (empty-tenv-record ()
          (eopl:error 'apply-tenv "Unbound variable ~s" sym))
        (extended-tenv-record (sym1 val1 old-env)
          (if (eqv? sym sym1) 
            val1
            (apply-tenv old-env sym))))))
  
  (define init-tenv
    (lambda ()
      (extend-tenv 'x (int-type) 
        (extend-tenv 'v (int-type)
          (extend-tenv 'i (int-type)
            (empty-tenv))))))

  )
