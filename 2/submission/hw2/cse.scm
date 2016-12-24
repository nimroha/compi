
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty (list))
(define empty? null?)

(define leaf?
  (lambda (x)
    (and (list? x) (null? (cdr x)))))

(define inc
  (lambda (n)
    (+ n 1)))

(define sexpr?
  (lambda (x)
    (and (list? x) (pair? x))))

(define append-item
  (lambda (p lst)
    (append (list p) lst)))

(define *reserved*
  '(and begin cond define do else if lambda
        let let* letrec or quasiquote unquote
        unquote-splicing quote set!))

(define depth
  (lambda (tree)
    (cond [(null? tree) 0]
          [(list? tree) (+ 1 (fold-left max 0 (map depth tree)))]
          [else -1]
          )))

(define depth>? ; deepest first
  (lambda (x y)
    (if (> (depth (cadr x)) (depth (cadr y)))
        #t
        #f)))

(define sort-by-depth
  (lambda (table)
    (sort depth>?  table))) ; TODO different argument order in scheme



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define count-table 'init) ; instances-expression pairs
(define sym-table 'init)   ; symbol-expression pairs
(define found-a-match #f)

(define init-globals
  (lambda ()
    (begin (set! count-table empty)
           (set! sym-table empty)
           (set! found-a-match #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; count common expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define inc-item
  (lambda (expr count)
    (let ((old (list count expr))
          (new (list (+ 1 count) expr)))
      (set! count-table (append (list new) (remove old count-table)))
      )))

(define new-item
  (lambda (expr)
    (set! count-table (append-item (list 1 expr) count-table))))

(define update-count
  (lambda (expr)
    (let ((found #f))
      (map (lambda (p)
             (if (equal? expr (cadr p))
                 (begin (set! found #t)
                        (inc-item expr (car p))) 
                 (void)))
           count-table)
      (if found
          (void)
          (new-item expr))
      )))

(define count-rec
  (lambda (tree)
    (if (sexpr? tree)
        (map (lambda (subtree)
               (if (and (sexpr? subtree) (not (member (car subtree) *reserved*)))
                   (begin (update-count subtree)
                          (count-rec subtree))
                   (void)))
             tree)
        (void))))

(define remove-singles
  (lambda ()
    (set! count-table (filter (lambda (p)
                                (< 1 (car p)))
                              count-table))))

(define ^count-table
  (lambda (expr)
    (begin (count-rec expr)
           (remove-singles)
           (set! count-table (sort-by-depth count-table)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; substitute common expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^sym-table
  (lambda ()
    (set! sym-table (map (lambda (p)
                           (list (gensym) (cadr p)))
                         count-table))))

(define sub-table
  (lambda (sym expr)
    (begin (map (lambda (p)
                  (list (car p) (sub-rec-wrap sym (cadr p) expr)))
                sym-table)
           #;(set! sym-table (sort-by-depth sym-table))) ; maybe not necessary
    ))

(define sub-rec-wrap ; first call here to avoid replacing the original expression
  (lambda (sym expr trgt)
    (if (sexpr? expr)
        (if (equal? trgt expr)
            expr ; return yourself instead of your symbol 
            (map (lambda (sub-exp)
                   (sub-rec sym sub-exp trgt))
                 expr))
        expr)
    ))

(define sub-rec
  (lambda (sym expr trgt)
    (if (sexpr? expr)
        (if (equal? trgt expr)
            (begin (set! found-a-match #t)
                   sym)
            (map (lambda (sub-exp)
                   (sub-rec sym sub-exp trgt))
                 expr))
        expr)
    ))

(define sub
  (lambda (tree index)
    (begin (set! found-a-match #f)
           (if (= index (length sym-table))
               tree
               (let* ((p (list-ref sym-table index))
                      (s (car p))
                      (e (cadr p))
                      (t (sub-rec s tree e)))
                 (if found-a-match
                     (begin (set! sym-table (sub-table s e))
                            (sub t (+ 1 index)))
                     (begin (set! sym-table (remove p sym-table))
                            (sub t index)))))
           )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; make let* expression ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^let*
  (lambda (expr)
    (cond [(null? sym-table) expr]
          [(= 1 (length sym-table))
           `(let ,(reverse sym-table) ,expr)]
          [else
           `(let* ,(reverse sym-table) ,expr)]
          )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cse
  (lambda (expr)
    (begin (init-globals)
           (^count-table expr)
           (^sym-table)
           (^let* (sub expr 0))
           )))
