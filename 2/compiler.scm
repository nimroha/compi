;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; const pattern rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Null
(define pattern-rule-nil
  (pattern-rule `() (lambda ()`(const ()))))

;;Number
(define pattern-rule-number
  (pattern-rule (? 'x number?) (lambda (e) `(const ,e))))

;;Boolean
(define pattern-rule-boolean
  (pattern-rule (? 'x boolean?) (lambda (e) `(const ,e))))

;;Char
(define pattern-rule-char
  (pattern-rule (? 'x char?) (lambda (e) `(const ,e))))

;;Stirng
(define pattern-rule-string
  (pattern-rule (? 'x string?) (lambda (e) `(const ,e))))

;;vector
(define pattern-rule-vector
  (pattern-rule (? 'x vector?) (lambda (e) `(const ,e))))

;;Fail - ToDo: delete
(define fail!
  (lambda () 'fail!))

;;;;;;;;;;;;;;;;;;;;;;;; quote ;;;;;;;;;;;;;;;;;;;
(pattern-rule '('unquote (? 'x)) (lambda (e)e))


;;Father of all const
(define compose-patterns-const
  (compose-patterns 
   pattern-rule-nil
   pattern-rule-number
   pattern-rule-boolean
   pattern-rule-char
   pattern-rule-string
   pattern-rule-vector
   ))

;;;;;;;;;;;;;;;;;; tag-parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag-parser
  (let ((run 
         (compose-patterns 
          compose-patterns-const
          )))
    (lambda (sexp)(run sexp (lambda () '(fail!))))))


