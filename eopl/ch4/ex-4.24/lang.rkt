(module lang (lib "eopl.ss" "eopl")                
  
  ;; language for IMPLICIT-REFS

  (require "drscheme-init.rkt")
  
  (provide (all-defined-out))

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

      (expression (number) const-exp)

      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      
      (expression
        ("+" "(" expression "," expression ")")
        sum-exp)

      (expression
        ("*" "(" expression "," expression ")")
        prod-exp)

      (expression
       ("zero?" "(" expression ")")
       zero?-exp)

      (expression
        ("not" "(" expression ")")
        not-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

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
          (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)
           "in" expression)
        letrec-exp)
      
      (expression
        ("begin" expression (arbno ";" expression) "end")
        begin-exp)

      ;; new for implicit-refs

      (expression
        ("set" identifier "=" expression)
        assign-exp)

      ;; definition for statements
      (statement
        (identifier "=" expression)
        assign-stmt)

      (statement
        ("print" expression)
        print-stmt)

      (statement
        ("{" (separated-list statement ";") "}")
        chain-stmt)

      (statement
        ("if" expression statement statement)
        if-stmt)

      (statement
        ("while" expression statement)
        while-stmt)

      (statement
        ("do" statement "while" expression)
        do-while-stmt)

      (statement
        ("var" (separated-list identifier ",") ";" statement)
        var-stmt)

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
