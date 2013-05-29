(module environments (lib "eopl.ss" "eopl") 
  
  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm. 

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (var val saved-env)
                    (if (eqv? search-sym var)
                        val
                        (apply-env saved-env search-sym)))
        (extend-env-rec (proc-names-lst bound-vars-lst proc-bodies-lst saved-env)
                        (let ((pos (search-symbol-position-in-list proc-names-lst search-sym)))
                          (if (number? pos)
                              (proc-val
                               (procedure
                                (list-ref bound-vars-lst pos)
                                (list-ref proc-bodies-lst pos)
                                env)
                               )
                              (apply-env saved-env search-sym))
                          )
                        )
        )
      )
    )
  
  (define search-symbol-position-in-list
    (lambda (lst search-sym)
      (if (pair? lst)
          (if (eqv? search-sym (car lst))
              0
              (+ 1 (search-symbol-position-in-list (cdr lst) search-sym)))
          'NaN)
      )
    )
  )