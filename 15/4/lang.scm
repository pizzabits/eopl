(module lang (lib "eopl.ss" "eopl")                
  
  ;; language for IMPLICIT-REFS

  (require "drscheme-init.scm")
  
  (provide (all-defined))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))
  
  (define the-grammar
    '((program (statement) a-program)
      
      (statement
       (identifier "=" expression)
       assign-statement)
      
      (statement
       ("print" expression)
       print-statement)
      
      (statement
       ("{" (separated-list statement ";") "}")
       block-statement)
      
      (statement
       ("if" expression statement statement)
       if-statement)
      
      (statement
       ("while" expression statement)
       while-statement)
      
      (statement
       ("var" (separated-list identifier ",") ";" statement)
       var-statement)

      (expression 
       (number) 
       const-exp)
      
      (expression
       ("-" "(" expression "," expression ")")
       diff-exp)
      
      (expression 
       ("+" "(" expression "," expression ")")
       add-exp)
      
      (expression 
       ("*" "(" expression "," expression ")")
       multiply-exp)
      
      (expression
       ("not" "(" expression ")" )
       not-exp)
      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression
       (identifier)
       var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)   

      (expression
       ("proc" "(" (separated-list identifier ",") ")" expression)
       proc-exp)

      (expression
       ("(" expression (arbno expression) ")")
       call-exp)

      (expression
       ("letrec"
        (arbno identifier "(" identifier ")" "=" expression)
        "in" expression)
       letrec-exp)
      
      (expression
       ("begin" expression (arbno ";" expression) "end")
       begin-exp)

      ;; new for implicit-refs
      
      (expression
       ("set" identifier "=" expression)
       assign-exp)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
  (sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))
  
  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))
  
  )
