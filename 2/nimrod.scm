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

(define var?
  (lambda (x)
    (and (symbol? x) (not (member x *reserved-words*)))))		

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

;;void
(define pattern-rule-void
  (pattern-rule (? 'p void?) (lambda (e) `(const ,e))))

;;Fail - ToDo: delete
(define fail!
  (lambda () 'fail!))

;;;;;;;;;;;;;;;;;;;;;;;; quote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-quotes
  (pattern-rule `(quote ,(? 'x))
                (lambda (e) `(const ,e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; quasiquote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define pattern-rule-quasiquote
;  (pattern-rule (list 'unquote (? 'x)) (lambda (e)  e)))

(define pattern-rule-quasiquote
  (compose-patterns
   (pattern-rule (list 'unquote (? 'x))
                 (lambda (e)  e))
   
   (pattern-rule `(,(list 'unquote-splicing (? 'x)) . ,(? 'y))
                 (lambda (e1 e2) `(append ,e1 ,(expand-qq e2))))
   
   (pattern-rule `(,(? 'x) . ,(list  'unquote-splicing (? 'y)))
                 (lambda (e1 e2) `(cons ,(expand-qq e1) ,e2)))
   
   (pattern-rule `(,(? 'x) . ,(? 'y))
                 (lambda (e1 e2) `(cons ,(expand-qq e1) ,(expand-qq e2))))
   
   (pattern-rule (? 'x vector?)
                 (lambda (e) vector-map expand-qq e))
   
   
   (pattern-rule (? 'x (lambda(a)(or (null? a) (symbol? a))))
                 (lambda (e) `',e))
   
   
   (pattern-rule (? 'x)
                 (lambda (e) e))
   
   
   ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;mayer quasiquote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
         (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
                 simple-sexprs-predicates)
          (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
            (pair? e)
            (symbol? e)
            (vector? e))
        `',e
        e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
        (cadr e)
        e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
         (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
            (lambda (e)
              (cond ((unquote? e) (cadr e))
                    ((unquote-splicing? e)
                     (error 'expand-qq
                            "unquote-splicing here makes no sense!"))
                    ((pair? e)
                     (let ((a (car e))
                           (b (cdr e)))
                       (cond ((unquote-splicing? a)
                              `(append ,(cadr a) ,(expand-qq b)))
                             ((unquote-splicing? b)
                              `(cons ,(expand-qq a) ,(cadr b)))
                             (else `(cons ,(expand-qq a) ,(expand-qq b))))))
                    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
                    ((or (null? e) (symbol? e)) `',e)
                    (else e))))
           (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
           (optimizer
            (compose-patterns
             
             
             (pattern-rule
              `(append ,(? 'e) '())
              (lambda (e) (optimize-qq-expansion e)))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
              (lambda (c1 c2)
                (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
                  c)))
             (pattern-rule
              `(append ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  `(append ,e1 ,e2))))
             (pattern-rule
              `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify (list (unquotify c1) (unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(cons ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  (if (and (const? e1) (const? e2))
                      (quotify (cons (unquotify e1) (unquotify e2)))
                      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Father of all const;;;;;;;;;;;;;;;;;;;;;;
(define compose-patterns-const
  (compose-patterns 
   pattern-rule-nil
   pattern-rule-number
   pattern-rule-boolean
   pattern-rule-quotes
   pattern-rule-char
   pattern-rule-string
   pattern-rule-vector
   pattern-rule-void
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-var 
  (pattern-rule (? 'v var?)
                (lambda (v) `(var ,v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; if ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define pattern-rule-if3
  (pattern-rule `(if ,(? 'test) ,(? 'true) ,(? 'false))
                (lambda (test true false)
                  `(if3 ,(parse test) ,(parse true) ,(parse false)))))

(define pattern-rule-if2
  (pattern-rule `(if ,(? 'test) ,(? 'true))
                (lambda (test true)
                  `(if3 ,(parse test) ,(parse true) ,(parse (void))))))

(define compose-patterns-if
  (compose-patterns
   pattern-rule-if3
   pattern-rule-if2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-cond3
  (pattern-rule `(cond  ,(? 'first) . ,(? 'second)) (lambda(e1 e2) (parse (cond->if e1 e2)))))


(define pattern-rule-cond1
  (pattern-rule `(cond  ,(? 'first)) (lambda(e) (parse (cond->if  e '())))))

(define cond->if
  (lambda ( e1 e2)
    (cond ((null? e2) `(if ,@e1 ,@e2))  
          ((= (length (car e2)) 1) `(if ,@e1 ,@(car e2)))
          (`(if ,@e1 ,(cond->if (car e2) (cdr e2))))
          )))

(define compose-patterns-cond
  (compose-patterns
   pattern-rule-cond3
   pattern-rule-cond1
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; and ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pattern-rule-and
  (pattern-rule `(and  ,(? 'args) . ,(? 'res)) (lambda(e1 e2) (parse (and->if (append (list e1) e2))))))

(define pattern-rule-and0
  (pattern-rule `(and) (lambda () (parse #t))))

(define make-if
  (lambda (predicate consequent alternative)
    `(if  ,predicate ,consequent ,alternative)))

(define and->if
  (lambda (args)
    (letrec ((and-if-helper (lambda (lst)                             
                              (if (empty? lst) '#t
                                  (if (empty? (cdr lst)) (car lst)
                                      (make-if (car lst) (and-if-helper (cdr lst)) '#f))))))
      (and-if-helper args))))

(define compose-patterns-and
  (compose-patterns
   pattern-rule-and
   pattern-rule-and0
   ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; or ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-or
  (pattern-rule `(or ,@(? 'exp-list))
                (lambda (exp-list)
                  `(or ,(map parse exp-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lambda forms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define identify-lambdas
  (lambda (arg)
    (letrec ((identify
              (lambda (argl ret-simple ret-opt ret-var)
                (cond 
                  ((null? argl) (ret-simple '()))
                  ((var? argl) (ret-var argl))    
                  (else (identify (cdr argl)
                                  (lambda (s) (ret-simple `(,(car argl) ,@s)))
                                  (lambda (s opt) (ret-opt `(,(car argl) ,@s) opt))
                                  (lambda (var) (ret-opt `(,(car argl)) var))))))))
      
      (identify arg (lambda (argl) `(lambda-simple ,argl)) (lambda (argl res) `(lambda-opt ,argl ,res)) (lambda (argl) `(lambda-var ,argl))))))

(define pattern-rule-lambda
  (pattern-rule `(lambda ,(? 'argl) ,(? 'exp)) (lambda (argl exp) `(,@(identify-lambdas argl) ,(tag-parser exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; define ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-def-MIT
  (pattern-rule `(define ,(? 'def (lambda (x)
                                    (or (list? x) (pair? x))))
                   ,(? 'body))
                (lambda (def body)
                  `(define ,(parse (car def)) ,(parse `(lambda ,(cdr def) ,body)))))) 

(define pattern-rule-def-REG
  (pattern-rule `(define ,(? 'v var?) ,(? 'e))
                (lambda (v e)
                  `(define ,(parse v) ,(parse e)))))


(define compose-patterns-define
  (compose-patterns
   pattern-rule-def-REG
   pattern-rule-def-MIT
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-applic
  (pattern-rule `(,@(? 'app (lambda (app)
                              (and (list? app)
                                   (not (empty? app))
                                   (not (member (car app) *reserved-words*))))))
                (lambda e
                  (let ((exps (car e)))
                    (if (empty? (cdr exps))
                        `(applic ,(parse (car exps)) '())
                        `(applic ,(parse (car exps)) ,(map parse (cdr exps)))
                        )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define pattern-rule-exp-seq 
  (pattern-rule `(begin ,@(? 'seq))
                (lambda (s)
                  `(seq (,(map parse s))))))

(define pattern-rule-imp-seq 'not-implemented)

(define compose-patterns-seq
  (compose-patterns
   pattern-rule-exp-seq
   ;pattern-rule-imp-seq
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let and friends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-rule-let1
  (pattern-rule `(let ((,(? 'id var?) ,(? 'val) )) ,(? 'body) )
                (lambda (id val body) 
                  `(,(parse `( (lambda (,id) ,(parse body)) ,val )))
                  )))

(define pattern-rule-let-defs
  (pattern-rule `( ? x) (lambda (x) 'not-implemented)))

(define compose-patterns-let
  (compose-patterns
   pattern-rule-let1
   ; pattern-rule-let-defs
   ;pattern-rule-letrec
   ;pattern-rule-let*
   ))



;;;;;;;;;;;;;;;;;; tag-parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tag-parser
  (let ((run 
         (compose-patterns 
          ;pattern-rule-quasiquote
          compose-patterns-if
          compose-patterns-cond
          compose-patterns-and
          pattern-rule-or
          compose-patterns-define
         ; compose-patterns-seq
          pattern-rule-applic
          pattern-rule-lambda
          compose-patterns-let
          
          
          pattern-rule-var
          compose-patterns-const
          )))
    (lambda (sexp)(run sexp (lambda () 'fail!)))))


(define parse tag-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-list
  '(()
    #(1 (2 3 4) 2 3)
    #f
    #\a
    24
    "abc"
    (quote a)
    (quote (a b c))
    (quote (quote a b c))
    'abc
    '123x
    (if a b c)
    (if (if a b c) 'x '(x y z))
    (if a b)
    (if a 4)
    (if #t 'abc)
    (or (zero? x) (zero? 9) (zero? z))
    (or (or (f1 x) (f2 y)))
    (lambda (x y z . rest) (if x y z))
    (lambda (x y z) (if x (lambda x x) z))
    (lambda args (if x y z))
    (define x 5)
    (define x (lambda (x) x))
    (define (id x) x)
    (define (foo x y z)(if x y z))
    (define (foo x y . z)(if x y z))
    (define (list . args) args)
    ((a b) (a c))
    (a)
    (a b c)
    ))

(define tests (lambda ()
                (map (lambda(x) `(,x :    ,(parse x))) test-list)))

(define qq-twst-list
  '((a b c)
    (a ,b c)
    (a ,b ,@c)
    (,@a ,b c)
    ))

(define qq-tests (lambda ()
                   (map (lambda(x) `(, x -> ,(parse x))) qq-twst-list)))

(define macro-list
  '((cond ((number? 1) 1))
    (cond ((zero? 1) 1) ('a))
    (cond ((number? 'a) 'a)((number? 1) 1))
    (cond ((number? 'a) 'a)((number? 1) 1) (2))
    (cond ((number? 'a) 'a)((number? 1) 1) ((number? 2) 2))
    (and)
    (and 1)
    (and 1 2)
    (and 1 2 3)
    
    ))

(define if-list
  '((if (number? 1) 1)
    (if (zero? 1) 1 'a)
    (if (number? 'a) 'a (if (number? 1) 1))
    (if (number? 'a) 'a (if (number? 1) 1 2))
    (if (number? 'a) 'a (if (number? 1) 1 (if (number? 2) 2)))
    #t
     1
    (if  1  2  #f)
    (if  1 (if  2  3 #f) #f)
    ))

(define macros-tests
  (lambda ()
    (map (lambda (x y) (equal? (parse x) (parse y))) macro-list if-list)))