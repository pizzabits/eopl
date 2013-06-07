(module array (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "store.scm")
  
  (provide (all-defined))
  
  ;;;;;;;;;;;;;;;; array ;;;;;;;;;;;;;;;;
  
  ;; model a mutable array as a consecutive sequence of locations which is zero-based,
  ;; and represent it with a reference to the first.
  
  ;; array? : SchemeVal -> Bool
  (define array?
    (lambda (v)
      (reference? v)))

  ;; make-array : ExpVal * ExpVal -> Array
  (define make-array
    (lambda (size initial-value)
      (if (< size 1)
          (eopl:error "An array must consist of at least one value!"))
      (let ((ref-to-first (newref initial-value)))
        (if (> size 1)
            (create-following-sequence ref-to-first (- size 1) initial-value))
        ref-to-first)))
  
  (define create-following-sequence
    (lambda (ref-to-first size initial-value)
      (if (> size 0)
          (begin
            (newref initial-value)
            (create-following-sequence ref-to-first (- size 1) initial-value)))
      ref-to-first))
  
  ;; dereference-array-index : Array -> ExpVal
  (define dereference-array-index
    (lambda (array-ref index)
      (deref (+ array-ref index))))
  
  ;; set-array-index : Array * ExpVal -> Unspecified
  (define set-array-index
    (lambda (array-ref index val)
      (setref! (+ array-ref index) val)))

  )