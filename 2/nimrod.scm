#lang scheme

;;; pattern-matcher.scm
;;; The pattern-matching package
;;;
;;; Programmer: Mayer Goldberg, 2016

(define match
  (letrec ((match
               (lambda (pat e ret-vals ret-fail)
                 (cond ((and (pair? pat) (pair? e))
                        (match (car pat) (car e)
                          (lambda (vals-car)
                            (match (cdr pat) (cdr e)
                              (lambda (vals-cdr)
                                (ret-vals
                                 (append vals-car vals-cdr)))
                              ret-fail))
                          ret-fail))
                       ((and (vector? pat) (vector? e)
                             (= (vector-length pat) (vector-length e))
                             (match (vector->list pat) (vector->list e)
                               ret-vals ret-fail)))
                       ((procedure? pat)
                        (let ((v (pat e)))
                          (if v (ret-vals v) (ret-fail))))
                       ((equal? pat e) (ret-vals '()))
                       (else (ret-fail))))))
    (lambda (pat e ret-with ret-fail)
      (match pat e
        (lambda (vals) (apply ret-with vals))
        ret-fail))))

(define ?
  (lambda (name . guards)
    (let ((guard?
           (lambda (e)
             (andmap 
              (lambda (g?) (g? e))
              guards))))
      (lambda (value)
        (if (guard? value)
            (list value)
            #f)))))

;;; composing patterns

(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

(define compose-patterns
  (letrec ((match-nothing
            (lambda (e failure)
              (failure)))
           (loop
            (lambda (s)
              (if (null? s)
                  match-nothing
                  (let ((match-rest
                         (loop (cdr s)))
                        (match-first (car s)))
                    (lambda (e failure)
                      (match-first e
                                   (lambda ()
                                     (match-rest e failure)))))))))
    (lambda patterns
      (loop patterns))))
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


;;;;;;;;;;;;;;;;;;;;;;;;; variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-var 
   (pattern-rule '(? 'v (not (member *reserved-words*))) (lambda (v) `(var ,v))))

;;;;;;;;;;;;;;;;;; tag-parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag-parser
  (let ((run 
         (compose-patterns 
          compose-patterns-const
          pattern-rule-var
          )))
    (lambda (sexp)(run sexp (lambda () '(fail!))))))





