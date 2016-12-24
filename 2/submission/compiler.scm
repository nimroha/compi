;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *reserved-words*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))



(define var?
  (lambda (x)
    (and (symbol? x) (not (member x *reserved-words*)))))

(define *void-object* (void));

(define void?
  (lambda (x) (eq? *void-object* x)))

(define empty? null?)

(define valid?
  (lambda (args)
    (letrec ((not-contain-duplicates
              (lambda (lst) 
                (if (null? lst) #t
                    (and (not (member (car lst) (cdr lst))) (not-contain-duplicates (cdr lst))))))
             (not-contain-duplicates-not-list (lambda (lst acc) 
                                                (if (not (pair? lst)) (and (var? lst)(and (not (member  lst acc))))
                                                    (and (not (member (car lst) acc)) (not-contain-duplicates-not-list (cdr lst) (cons (car lst) acc))))
                                                ))
             )
             (cond ((or (null? args) (var? args)) #t)
                    ((list? args) (and (not-contain-duplicates args) (andmap var? args)))
                    (else (not-contain-duplicates-not-list args '()))
                    )
                   )))
    
    
    (define first car)
    (define second cadr)
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
      (lambda () "ERROR"))
    
    ;;;;;;;;;;;;;;;;;;;;;;;; quote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define pattern-rule-quotes
      (pattern-rule `(quote ,(? 'x))
                    (lambda (e) `(const ,e))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; quasiquote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define pattern-rule-quasiquote
      (pattern-rule (list 'quasiquote (? 'x)) (lambda (e) (tag-parser (expand-qq e)))))
    
    (define pattern-rule-quasiquote-not-needed
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
                      `(if3 ,(tag-parser test) ,(tag-parser true) ,(tag-parser false)))))
    
    (define pattern-rule-if2
      (pattern-rule `(if ,(? 'test) ,(? 'true))
                    (lambda (test true)
                      `(if3 ,(tag-parser test) ,(tag-parser true) ,(tag-parser (void))))))
    
    (define compose-patterns-if
      (compose-patterns
       pattern-rule-if3
       pattern-rule-if2))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define pattern-rule-cond0
      (pattern-rule `(cond  ,(? 'first empty?)) (lambda(e) (tag-parser *void-object*))))
    
    (define pattern-rule-cond3
      (pattern-rule `(cond  ,(? 'condlst) . ,(? 'res)) (lambda (e1 e2) (tag-parser (cond->if e1 e2)))))
    
    (define pattern-rule-cond1
      (pattern-rule `(cond  ,(? 'first)) (lambda(e) (tag-parser (cond->if  e '())))))
    
    (define cond->if
      (lambda ( e1 e2)
        (cond ((null? e2) (if  (eq? (car e1) 'else)
                               `(begin ,@(cdr e1)) 
                               `(if ,(first e1) (begin ,@(cdr e1))))
                          )
              (`(if ,(first e1) (begin ,@(cdr e1)) ,(cond->if (car e2) (cdr e2))))          
              )))
    
    (define compose-patterns-cond
      (compose-patterns
       ;pattern-rule-cond0
       pattern-rule-cond3
       ;pattern-rule-cond1
       ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; and ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define pattern-rule-and
      (pattern-rule `(and  ,(? 'args) . ,(? 'res)) (lambda(e1 e2) (tag-parser (and->if (append (list e1) e2))))))
    
    (define pattern-rule-and0
      (pattern-rule `(and) (lambda () (tag-parser #t))))
    
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
    (define pattern-rule-or0
      (pattern-rule `(or ,@(? 'exp-list null?))
                    (lambda (exp-list)(tag-parser #f))))
    
    (define pattern-rule-or
      (pattern-rule `(or ,@(? 'exp-list))
                    (lambda (exp-list)
                      (cond ((= (length exp-list) 1) (tag-parser (car exp-list)))
                            (else `(or ,(map tag-parser exp-list)))
                            ))))
    
    (define compose-patterns-or
      (compose-patterns
       pattern-rule-or0
       pattern-rule-or
       ))
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
      (pattern-rule `(lambda ,(? 'argl) . ,(? 'exp)) (lambda (argl exp)
                                                       (cond ((null? argl) `(,@(identify-lambdas argl) ,(tag-parser (cons 'begin exp))))
                                                             ((not (valid? argl)) (error 'ERROR "ERROR"))
                                                             (else `(,@(identify-lambdas argl) ,(tag-parser (cons 'begin exp))))
                                                             ))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; define ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define pattern-rule-def-MIT
      (pattern-rule `(define ,(? 'def (lambda (x)
                                        (or (list? x) (pair? x))))
                       ,(? 'body))
                    (lambda (def body)
                      `(def ,(tag-parser (car def)) ,(tag-parser `(lambda ,(cdr def) ,body)))))) ;TODO test after implementing simple-lambda
    
    (define pattern-rule-def-REG
      (pattern-rule `(define ,(? 'v var?) ,(? 'e))
                    (lambda (v e)
                      `(def ,(tag-parser v) ,(tag-parser e)))))
    
    
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
                            `(applic ,(tag-parser (car exps)) ())
                            `(applic ,(tag-parser (car exps)) ,(map tag-parser (cdr exps)))
                            )))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define begin-killer
  (lambda (exps)
    (let* ((begin? (lambda (e) (eq? 'begin e)))
           (beginless-exps (fold-right (lambda (exp acc)
                             (if (and (list? exp) (begin? (car exp))) (append (cdr exp) acc) (cons exp acc)) )'() exps))
           )
      (if (equal? exps beginless-exps) beginless-exps (begin-killer beginless-exps))
      )))
                       


(define pattern-rule-exp-seq0 
  (pattern-rule `(begin ,@(? 'seq null?))
                (lambda (s)(tag-parser *void-object*))))

(define pattern-rule-exp-seq 
  (pattern-rule `(begin ,@(? 'seq list?))
                (lambda (s)
                  (cond ((= (length s) 1) (tag-parser (car s)))
                        (else `(seq ,(map tag-parser  (begin-killer s))))
                        ))))

(define pattern-rule-imp-seq 'not-implemented)

(define compose-patterns-seq
  (compose-patterns
   pattern-rule-exp-seq0
   pattern-rule-exp-seq
   ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define pattern-rule-set 
      (pattern-rule `(set! ,(? 'var var?) ,(? 'val)) (lambda (var val) `(set ,(tag-parser var) ,(tag-parser val)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define val-list (lambda (lst)
                       (if (null? lst) lst
                           (cons (second (first lst)) (val-list (cdr lst))))))
    (define var-list (lambda (lst)
                       (if (null? lst) lst
                           (cons (car (car lst)) (var-list (cdr lst))))))
    
    (define pattern-rule-let0
      (pattern-rule `(let ,(? 'lst list? empty?) ,(? 'body) )
                    (lambda (lst body) 
                      (tag-parser `((lambda () , body))))
                    ))
    
    (define pattern-rule-let1
      (pattern-rule `(let ((,(? 'id var?) ,(? 'val) )) ,(? 'body) )
                    (lambda (id val body) 
                      (tag-parser `((lambda (,id) ,body) ,val ))
                      )))
    
    (define pattern-rule-let+
      (pattern-rule `(let ,(? 'lst list?) . ,(? 'body) )
                    (lambda (lst body) 
                      (letrec (
                               (vars (var-list lst))
                               (vals (val-list lst)))
                        (tag-parser `((lambda (,@vars) ,@body) ,@vals)))
                      )))
    
    (define pattern-rule-let
      (compose-patterns
       pattern-rule-let0
       pattern-rule-let1
       pattern-rule-let+
       ))      
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define pattern-rule-let*0
      (pattern-rule `(let* ,(? 'lst list? empty?) ,(? 'body) )
                    (lambda (lst body) 
                      (tag-parser `(let () ,body)))))
    
    
    (define pattern-rule-let*+
      (pattern-rule `(let* ,(? 'lst list?) . ,(? 'body) )
                    (lambda (lst body) 
                      (letrec ((let*->let (lambda(lst body)
                                            (cond ((null? lst) `(let () (begin ,@body)))
                                                  ((null? (cdr lst)) `(let (,(car lst)) (begin ,@body)))
                                                  (`(let (,(car lst)) ,(let*->let (cdr lst) body)))))))
                        (tag-parser (let*->let lst body))))))
    
    (define pattern-rule-let*
      (compose-patterns
       pattern-rule-let*0
       pattern-rule-let*+
       ))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; letrec ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define box-list (lambda (vars vals)
                       (if (null? vars) '()
                           (cons `(set! ,(car vars) ,(car vals))(box-list(cdr vars)(cdr vals))))))
    
    (define fals-list (lambda (lst)
                        (if (null? lst) lst
                            (cons #f (fals-list (cdr lst))))))							  
    
    (define pattern-rule-letrec0
      (pattern-rule `(letrec ,(? 'lst list? empty?) . ,(? 'body) )
                    (lambda (lst body)
                      (let* (
                             (vars (var-list lst))
                             (vals (val-list lst))
                             (boxs (box-list vars vals))
                             (falses (fals-list vals))
                             )
                        (tag-parser `((lambda (,@vars) (begin  ((lambda () ,@body)))),@falses))))))
    
    (define pattern-rule-letrec+
      (pattern-rule `(letrec ,(? 'lst list?) . ,(? 'body) )
                    (lambda (lst body)
                      (let* (
                             (vars (var-list lst))
                             (vals (val-list lst))
                             (boxs (box-list vars vals))
                             (falses (fals-list vals))
                             )
                        (tag-parser `((lambda (,@vars) (begin ,@boxs ((lambda () ,@body)))),@falses))))))
    
    
    (define pattern-rule-letrec
      (compose-patterns
       pattern-rule-letrec0
       pattern-rule-letrec+
       ))
    ;;;;;;;;;;;;;;;;;; tag-parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define parse tag-parser)
	
    (define error?
      (lambda (exp)
        (eq? exp "ERROR")))
    
    (define tag-parser
      (let ((run 
             (compose-patterns 
              pattern-rule-set
              compose-patterns-if
              compose-patterns-cond
              compose-patterns-and
              compose-patterns-or
              compose-patterns-define
              compose-patterns-seq
              pattern-rule-applic      
              pattern-rule-lambda
              pattern-rule-let
              pattern-rule-let*
              pattern-rule-letrec
              pattern-rule-quasiquote
              pattern-rule-var
              compose-patterns-const
              )))
        (lambda (sexp)(run sexp (lambda () (fail!))))))
    
    