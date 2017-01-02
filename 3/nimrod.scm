;;;;;;;;;;;;;;;;;;;;;; TODO: delete before submitting ;;;;;;;;;;;;;;;;;;;;;;;
#lang scheme
(define fold-left foldl)
(define fold-right foldr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mayer's code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pattern-matcher.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pc.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pc.scm
;;; A simple implementation of parsing combinators
;;;
;;; Programmer: Mayer Goldberg, 2016


(define *marker-length* 8)

(define with (lambda (s f) (apply f s)))

(define list-head
  (lambda (s n)
    (cond ((null? s) '())
          ((zero? n) '(#\space #\e #\t #\c))
          (else (cons (car s)
                      (list-head (cdr s) (- n 1)))))))

(define <end-of-input>
  (lambda (s ret-match ret-none)
    (if (null? s)
        (ret-match #t '())
        (ret-none '()))))

(define const
  (lambda (pred?)
    (lambda (s ret-match ret-none)
      (cond ((null? s) (ret-none '()))
            ((pred? (car s)) (ret-match (car s) (cdr s)))
            (else (ret-none '()))))))

(define <epsilon>
  (lambda (s ret-match ret-none)
    (ret-match '() s)))

(define caten
  (letrec ((binary-caten
            (lambda (p1 p2)
              (lambda (s ret-match ret-none)
                (p1 s
                    (lambda (e1 s)
                      (p2 s
                          (lambda (e2 s)
                            (ret-match (cons e1 e2) s))
                          ret-none))
                    ret-none))))
           (loop
            (lambda (ps)
              (if (null? ps)
                  <epsilon>
                  (binary-caten (car ps)
                                (loop (cdr ps)))))))
    (lambda ps
      (loop ps))))

(define <fail>
  (lambda (s ret-match ret-none)
    (ret-none '())))

(define disj
  (letrec ((binary-disj
            (lambda (p1 p2)
              (lambda (s ret-match ret-none)
                (p1 s ret-match
                    (lambda (w1)
                      (p2 s ret-match
                          (lambda (w2)
                            (ret-none `(,@w1 ,@w2)))))))))
           (loop
            (lambda (ps)
              (if (null? ps)
                  <fail>
                  (binary-disj (car ps)
                               (loop (cdr ps)))))))
    (lambda ps
      (loop ps))))

(define delay
  (lambda (thunk)
    (lambda (s ret-match ret-none)
      ((thunk) s ret-match ret-none))))

(define star
  (lambda (p)
    (disj (pack-with (caten p (delay (lambda () (star p))))
                     cons)
          <epsilon>)))

(define plus
  (lambda (p)
    (pack-with (caten p (star p))
               cons)))

(define times
  (lambda (<p> n)
    (if (zero? n)
        <epsilon>
        (pack-with
         (caten <p> (times <p> (- n 1)))
         cons))))

(define pack
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (f e) s)) ret-none))))

(define pack-with
  (lambda (p f)
    (lambda (s ret-match ret-none)
      (p s (lambda (e s) (ret-match (apply f e) s)) ret-none))))

(define diff
  (lambda (p1 p2)
    (lambda (s ret-match ret-none)
      (p1 s
          (lambda (e w)
            (p2 s (lambda _ (ret-none '()))
                (lambda (w1) (ret-match e w))))
          ret-none))))

(define maybe
  (lambda (p)
    (lambda (s ret-match ret-none)
      (p s
         (lambda (e s) (ret-match `(#t ,e) s))
         (lambda (w) (ret-match `(#f #f) s))))))

(define maybe?
  (lambda (?result)
    (car ?result)))

(define maybe->value
  (lambda (?result)
    (cadr ?result)))

(define fence
  (lambda (p pred?)
    (lambda (s ret-match ret-none)
      (p s
         (lambda (e s)
           (if (pred? e)
               (ret-match e s)
               (ret-none '())))
         ret-none))))

(define otherwise
  (lambda (p message)
    (lambda (s ret-match ret-none)
      (p s
         ret-match
         (let ((marker
                (format "-->[~a]"
                        (list->string
                         (list-head s *marker-length*)))))
           (lambda (w) (ret-none `(,@w ,message ,marker))))))))

;;;

(define ^char
  (lambda (char=?)
    (lambda (character)
      (const
       (lambda (ch)
         (char=? ch character))))))

(define char (^char char=?))

(define char-ci (^char char-ci=?))

(define ^word
  (lambda (char)
    (lambda (word)
      (apply caten (map char (string->list word))))))

(define word (^word char))

(define word-ci (^word char-ci))

(define ^word-suffixes
  (lambda (char)
    (letrec ((loop
              (lambda (s)
                (if (null? s)
                    <epsilon>
                    (maybe
                     (caten (char (car s))
                            (loop (cdr s))))))))
      (lambda (suffix)
        (loop (string->list suffix))))))

(define word-suffixes (^word-suffixes char))

(define word-suffixes-ci (^word-suffixes char-ci))

(define ^word+suffixes
  (lambda (word-suffixes)
    (lambda (prefix suffix)
      (caten (word prefix)
             (word-suffixes suffix)))))

(define word+suffixes (^word+suffixes word-suffixes))

(define word+suffixes-ci (^word+suffixes word-suffixes-ci))

(define ^one-of
  (lambda (char)
    (lambda (word)
      (apply disj (map char (string->list word))))))

(define one-of (^one-of char))

(define one-of-ci (^one-of char-ci))

(define ^range
  (lambda (char<=?)
    (lambda (char-from char-to)
      (const
       (lambda (ch)
         (and (char<=? char-from ch)
              (char<=? ch char-to)))))))

(define range (^range char<=?))

(define range-ci (^range char-ci<=?))

(define <any-char> (const (lambda (ch) #t)))

(define <any> <any-char>)

;;; <expr> {<sep> <expr>}*
(define ^<separated-exprs>
  (lambda (<expr> <sep>)
    (new (*parser <expr>)
         
         (*parser <sep>)
         (*parser <expr>)
         (*caten 2)
         (*pack-with (lambda (_sep expr) expr))
         *star
         
         (*caten 2)
         (*pack-with cons)
         done)))

;;;

(define continue
  (lambda (ds cs)
    (with cs
          (lambda (c . cs)
            (c ds cs)))))

(define new
  (lambda cs
    (continue '() cs)))

(define done
  (lambda (ds cs)
    (with ds
          (lambda (parser . ds)
            (if (null? ds)
                parser
                (error 'done
                       (format "The parser stack still contains ~a parsers!"
                               (length ds))))))))

(define *parser
  (lambda (p)
    (lambda (ds cs)
      (continue `(,p . ,ds) cs))))

(define unary
  (lambda (f-unary)
    (lambda (ds cs)
      (with ds
            (lambda (d . ds)
              (continue `(,(f-unary d) . ,ds) cs))))))

(define *delayed
  (lambda (thunk)
    (lambda (ds cs)
      (continue `(,(delay thunk) . ,ds) cs))))

(define binary
  (lambda (f-binary)
    (lambda (ds cs)
      (with ds
            (lambda (d2 d1 . ds)
              (continue `(,(f-binary d1 d2) . ,ds) cs))))))

(define *dup
  (lambda (ds cs)
    (with ds
          (lambda (d1 . ds)
            (continue `(,d1 ,d1 . ,ds) cs)))))

(define *swap
  (lambda (ds cs)
    (with ds
          (lambda (d1 d2 . ds)
            (continue `(,d2 ,d1 . ,ds) cs)))))

(define *star (unary star))

(define *plus (unary plus))

(define *diff (binary diff))

(define *pack (lambda (f) (unary (lambda (p) (pack p f)))))

(define *pack-with (lambda (f) (unary (lambda (p) (pack-with p f)))))

(define *fence (lambda (pred?) (unary (lambda (p) (fence p pred?)))))

(define *guard (lambda (pred?) (unary (lambda (p) (fence p pred?)))))

(define split-list
  (lambda (s n ret-s1+s2)
    (if (zero? n)
        (ret-s1+s2 '() s)
        (split-list (cdr s) (- n 1)
                    (lambda (s1 s2)
                      (ret-s1+s2 (cons (car s) s1) s2))))))

(define nary
  (lambda (f-n-ary n)
    (lambda (ds cs)
      (split-list ds n
                  (lambda (sgra ds)
                    (continue
                     `(,(apply f-n-ary (reverse sgra)) . ,ds) cs))))))

(define *caten (lambda (n) (nary caten n)))

(define *disj (lambda (n) (nary disj n)))

(define *maybe (unary maybe))

(define *otherwise
  (lambda (string)
    (unary
     (lambda (p)
       (otherwise p string)))))

(define *times
  (lambda (n)
    (unary
     (lambda (<p>)
       (times <p> n)))))

(define not-followed-by
  (lambda (<p1> <p2>)
    (new (*parser <p1>)
         (*parser <p2>) *maybe
         (*caten 2)
         (*pack-with
          (lambda (e1 ?e2)
            (with ?e2
                  (lambda (found-e2? _)
                    `(,e1 ,found-e2?)))))
         (*guard
          (lambda (e1+found-e2?)
            (with e1+found-e2?
                  (lambda (_ found-e2?)
                    (not found-e2?)))))
         (*pack-with
          (lambda (e1 _) e1))
         done)))

(define *not-followed-by (binary not-followed-by))

(define *transformer
  (lambda (^<p>)
    (unary (lambda (<p>) (^<p> <p>)))))

;;; 

(define test-string
  (lambda (parser string)
    (parser (string->list string)
            (lambda (e s)
              `((match ,e)
                (remaining ,(list->string s))))
            (lambda (w) `(failed with report: ,@w)))))

(define test
  (lambda (parser s)
    (parser s
            (lambda (e s)
              `((match ,e)
                (remaining ,s)))
            (lambda (w) `(failed with report: ,@w)))))

;;;

(define file->string
  (lambda (filename)
    (let ((input (open-input-file filename)))
      (letrec ((run
                (lambda ()
                  (let ((e (read-char input)))
                    (if (eof-object? e)
                        (begin
                          (close-input-port input)
                          '())
                        (cons e (run)))))))
        (list->string (run))))))

(define read-stdin-to
  (lambda (end-of-input)
    (let ((end-of-input-list (string->list end-of-input)))
      (letrec ((state-init
                (lambda (seen)
                  (let ((ch (read-char)))
                    (cond ((eof-object? ch)
                           (error 'read-stdin-to
                                  (format "Marker ~a not reached"
                                          end-of-input)))
                          ((char=? ch (car end-of-input-list))
                           (state-seen seen `(,ch) (cdr end-of-input-list)))
                          (else (state-init `(,ch ,@seen)))))))
               (state-seen
                (lambda (seen-before seen-now end-of-input-list-rest)
                  (if (null? end-of-input-list-rest)
                      (list->string
                       (reverse seen-before))
                      (let ((ch (read-char)))
                        (cond ((eof-object? ch)
                               (format "Marker ~a not reached"
                                       end-of-input))
                              ((char=? ch (car end-of-input-list-rest))
                               (state-seen seen-before
                                           `(,ch ,@seen-now)
                                           (cdr end-of-input-list-rest)))
                              (else (state-init
                                     `(,ch ,@seen-now ,@seen-before)))))))))
        (state-init '())))))

;;; end-of-input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; compiler.scm HW2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of assignment 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load "pc.scm")  TODO: uncomment
;(load "pattern-matcher.scm")



(define empty '())

(define (flatten lst)
  (cond 
    [(null? lst) empty]
    [(= (length lst) 0) empty]
    [(not (list? lst)) (if (pair? lst)
                           `(,(car lst) ,(cdr lst))
                           lst)]
    [(list? (car lst))
     (append (flatten (car lst)) (flatten (cdr lst)))]
    [else
     (cons (car lst) (flatten (cdr lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Number;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <digit-0-9> 
  (range #\0 #\9))

(define <digit-1-9> 
  (range #\1 #\9))

(define <Natural>
  (new  (*parser (char #\0))
        *star
        (*parser <digit-1-9> )
        (*parser <digit-0-9>) *star
        (*caten 3) 
        (*pack-with (lambda (zeros x y) (string->number
                                         (list->string `(,x ,@y)))))
        
        (*parser  (char #\0)) 
        (*pack (lambda (_) 0))
        
        (*disj 2)
        done))

(define <Integer>
  (new(*parser (char #\+))
      (*parser <Natural>)
      (*caten 2)
      (*pack-with (lambda (x y) y)) 
      
      (*parser (char #\-))
      (*parser <Natural>)
      (*caten 2)
      (*pack-with (lambda (x y) (- y))) 
      
      (*parser <Natural>) 
      
      (*disj 3)
      done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*caten 3)
       (*pack-with
        (lambda (num div den)
          (/ num den)))
       done))

(define <Number>
  (new (*parser <Fraction>)
       (*parser <Integer>)
       
       (*disj 2)
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Boolean>
  (new (*parser (char #\#))
       
       (*parser (char-ci #\t))
       (*pack (lambda (t) #t))
       (*parser (char-ci #\f))
       (*pack (lambda (f) #f))
       (*disj 2)
       
       (*caten 2)
       (*pack-with (lambda (hash val) val))
       
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CHAR ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <CharPrefix>
  (new (*parser (char #\#))
       (*parser (char #\\))
       (*caten 2)
       done))

(define <VisibleSimpleChar>
  (new (*parser <any-char>)
       (*guard (lambda (n)
                 (< (char->integer #\space) (char->integer n))))
       (*pack (lambda (char) char))
       done))

(define ^<named-char>
  (lambda (str ch)
    (new (*parser (word-ci str))
         (*pack (lambda (_) ch))
         done)))

(define <NamedChar>
  (new (*parser (^<named-char> "lambda" (integer->char 955)))
       (*parser (^<named-char> "newline" #\newline))
       (*parser (^<named-char> "nul" #\nul))
       (*parser (^<named-char> "page" #\page))
       (*parser (^<named-char> "return" #\return))
       (*parser (^<named-char> "space" #\space))
       (*parser (^<named-char> "tab" #\tab))
       (*disj 7)
       
       done))

(define <hex-dig>
  (let ((zero (char->integer #\0))
        (lc-a (char->integer #\a))
        (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
         (*pack
          (lambda (ch)
            (- (char->integer ch) zero)))
         
         (*parser (range #\a #\f))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) lc-a))))
         
         (*parser (range #\A #\F))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) uc-a))))
         
         (*disj 3)
         done)))

(define <HexChar>
  (new (*parser (range #\0 #\9))
       (*parser (range #\a #\f))
       (*parser (range #\A #\F))
       (*disj 3)
       done))


(define <HexUnicodeChar>  
  (new (*parser (word-ci "x"))
       
       (*parser <hex-dig>)
       *plus
       
       (*parser (char #\;))
       *maybe
       
       (*caten 3)
       (*pack-with (lambda (x chars semi)
                     (letrec ((build-hex (lambda (total lst)
                                           (if (null? lst)
                                               total
                                               (build-hex (+ (* total 16) (car lst)) (cdr lst))))))
                       (let ((n (build-hex 0 chars)))
                         (if (> n 1114111)
                             1114112
                             (integer->char n))
                         ))))
       done))

(define <Char>
  (new (*parser <CharPrefix>)
       
       (*parser <HexUnicodeChar>)
       (*guard (lambda (char)
                 (not (eq? char 1114112))))
       
       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)      
       (*disj 3)
       
       (*delayed (lambda () <SymbolChar>)) ; TODO is this enough?
       *not-followed-by
       
       (*caten 2)
       (*pack-with (lambda (pre char) char))
       done))


(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
         (*pack (lambda (_) ch))
         done)))

(define <StringMetaChar>
  (new (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       
       (*disj 6)
       done))	 

(define <StringHexChar>
  (new (*parser (char #\\))
       (*parser <HexUnicodeChar>)
       (*guard (lambda (char)
                 (not (eq? char 1114112))))
       
       (*caten 2)
       (*pack-with (lambda (slash char)
                     char))
       done))

(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*guard (lambda (x)
                 (not (eq? x #\\))))
       done))

(define <StringChar>
  (new (*parser <StringMetaChar>)
       (*parser <StringHexChar>)
       (*parser <StringLiteralChar>)
       (*disj 3)
       
       (*parser (char #\"))
       
       *diff
       done))


(define <String>
  (new (*parser (char #\"))
       
       (*parser <StringChar>)
       *star
       
       (*parser (char #\"))
       
       (*caten 3)
       (*pack-with (lambda (del1 chars del2)
                     (list->string (flatten chars))))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SYMBOL  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <special-char>
  (new (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\/))
       (*parser (char #\!))
       (*parser (char #\^))
       (*parser (char #\_))
       (*parser (char #\*))
       (*parser (char #\+))
       (*parser (char #\-))
       (*parser (range #\< #\?))
       (*disj 10)
       done))

(define <SymbolChar>
  (new (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*parser <special-char>)
       (*disj 3)
       (*pack (lambda (char)
                (let ((val  (char->integer char))
                      (uc-a (char->integer #\A))
                      (uc-z (char->integer #\Z))
                      (diff-case (- (char->integer #\A) (char->integer #\a))))
                  (if (and (>= val uc-a) (<= val uc-z))
                      (integer->char (- val diff-case))
                      char))))
       done))

(define <not-symbol>
  (new (*parser <Number>)
       (*parser <Fraction>)
       (*disj 2)
       
       (*parser <SymbolChar>)
       *not-followed-by
       done))


(define <Symbol> 
  (new (*parser <SymbolChar>)
       (*parser <digit-0-9>) 
       (*disj 2)
       (*parser <SymbolChar>)
       (*parser <digit-0-9>)
       (*disj 2)
       *plus
       (*caten 2)
       
       (*parser <not-symbol>)
       *diff
       (*pack-with (lambda (char lst)
                     (string->symbol (list->string (append (list char) lst)))))
       
       (*parser <SymbolChar>)
       (*delayed (lambda () <Symbol>))
       *not-followed-by
       (*pack (lambda (char)
                (string->symbol (list->string (list char)))))
       (*disj 2)
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ProperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ProperList>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr>)) *star
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (_x exp _y)
                     `(,@exp)))
       
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ImproperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ImproperList>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr>)) *plus
       (*parser (char #\.))
       (*delayed (lambda () <sexpr>))
       (*parser (char #\)))
       (*caten 5)
       (*pack-with (lambda (_p1 exp1 _dot exp2 _p2)
                     `( ,@exp1 . ,exp2)))
       
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Vector;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Vector>
  (new (*parser (char #\#))
       (*parser (char #\())
       (*delayed (lambda () <sexpr>)) *star
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (_x _y exp _z)
                     (list->vector `( ,@exp))))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Quoted>
  (new (*parser (char #\'))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quote exp)))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QuasiQuoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quasiquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Unquoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Unquoted>
  (new (*parser (char #\,))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'unquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UnquoteAndSpliced;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <UnquoteAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@))
       (*delayed (lambda () <sexpr>))
       (*caten 3)
       (*pack-with (lambda (_x _y exp )
                     (list 'unquote-splicing exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mayer  COMMENT  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser <end-of-input>)
              (*disj 2)
              done)))
    
    (new (*parser (char #\;))
         
         (*parser <any-char>)
         (*parser <end-of-line-comment>)
         *diff *star
         
         (*parser <end-of-line-comment>)
         (*caten 3)
         done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda ()  <sexpr>))
       (*caten 2)
       done))

(define <comment>
  (new (*parser <line-comment>)
       (*parser <sexpr-comment>)
       (*disj 2)
       done))

(define <skip>
  (new (*parser <comment>)
       (*parser <whitespace>)
       (*disj 2)
       done))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
           (*parser <p>)
           (*parser <wrapper>)
           (*caten 3)
           (*pack-with
            (lambda (_left e _right) e))
           done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INFIX COMMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <infix-comment>
  (new (*parser <line-comment>)
       
       (*parser (word "#;"))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       
       (*disj 2)
       done))


(define <infix-skip>
  (new (*parser <infix-comment>)
       (*parser <whitespace>)
       (*disj 2)
       done))



(define ^<infix-skipped*> (^^<wrapped> (star <infix-skip>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INFIX  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define <operator-symbol>
  (new (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\+))
       (*parser (char #\^))
       (*parser (char #\/))
       (*parser (word "**"))
       (*disj 6)
       done))

(define <InfixSymbol>
  (new (*parser <digit-0-9>)
       *star
       
       (*parser <SymbolChar>)
       (*parser <operator-symbol>)
       *diff
       
       (*parser <SymbolChar>)
       (*parser <digit-0-9>)
       (*disj 2)
       (*parser <operator-symbol>)
       *diff
       *star
       
       (*caten 3)
       (*pack-with (lambda args
                     (string->symbol (list->string (flatten args)))))
       done))

(define <PowerSymbol>
  (^<infix-skipped*>
   (new (*parser (char #\^))
        (*parser (word "**"))
        (*disj 2)
        done)))     



(define <InfixPrefixExtensionPrefix>
  (new (*parser (word "##"))
       (*parser (word "#%"))
       (*disj 2)
       done))


;;;;;;;;;;;;;;;;;;;;;;;;;Infix Op;;;;;;;;;;;;;;;;;

(define <InfixPow>
  (^<infix-skipped*>
   (new 
    (*delayed (lambda () <InfixFuncall>))
    
    (*parser <PowerSymbol>) 
    (*delayed (lambda () <InfixFuncall>))  
    (*caten 2)
    (*pack-with (lambda (sign exp) exp))
    *star
    
    (*caten 2)
    (*pack-with (lambda (firstExp restExp) 
                  (letrec ((lst  `(,firstExp ,@restExp))
                           (res (lambda (resLst)
                                  (if (= (length resLst)1) (car resLst)
                                      `(expt ,(car resLst) ,(res (cdr resLst)))))))
                    
                    (res lst))))
    
    
    done)))



(define <InfixDiv-Mul>
  (new (*delayed (lambda () <InfixNeg>))
       
       (*parser (char #\/))
       (*parser (char #\*))
       (*disj 2)
       (*pack (lambda (x)
                (string->symbol 
                 (list->string `(,x))))) 
       
       (*delayed (lambda () <InfixNeg>))
       (*caten 2) 
       (*pack-with (lambda (sign exp)(cons sign exp)))
       *star
       
       (*caten 2)
       (*pack-with (lambda (firstExp restExp) 
                     (letrec ((lst (reverse `(,firstExp ,@restExp)))
                              (res (lambda (resLst)
                                     (if (= (length  resLst) 1) (car resLst)
                                         `(,(car (car resLst)) ,(res (cdr resLst)) ,(cdr (car resLst)))))))
                       (res lst))))
       
       done))


(define <InfixSub-Add>
  (new (*parser <InfixDiv-Mul>)
       
       (*parser (char #\-))
       (*parser (char #\+))
       (*disj 2)
       (*pack (lambda (x)
                (string->symbol 
                 (list->string `(,x))))) 
       
       (*parser <InfixDiv-Mul>) 
       (*caten 2) 
       (*pack-with (lambda (sign exp)(cons sign exp)))
       *star
       
       (*caten 2)
       (*pack-with (lambda (firstExp restExp) 
                     (letrec ((lst (reverse `(,firstExp ,@restExp)))
                              (res (lambda (resLst)
                                     (if (= (length  resLst) 1) (car resLst)
                                         `(,(car (car resLst)) ,(res (cdr resLst)) ,(cdr (car resLst)))))))
                       (res lst))))
       
       
       done))


(define <InfixNeg>
  (new (*parser <InfixPow>)
       
       (*parser (char #\-))
       (*delayed (lambda () <InfixPow>))
       (*caten 2)
       (*pack-with (lambda (op exp)
                     `(- ,exp)))
       (*disj 2)
       done))



(define <InfixArrayGet>
  (new (*delayed (lambda () <InfixParen>))
       (*parser <whitespace>) *star
       (*parser (char #\[))    
       (*delayed (lambda () <InfixExpression>))
       (*parser (char #\]))
       (*parser <whitespace>) *star
       (*caten 5)
       (*pack-with (lambda (_x _z exp _y _w) exp))
       *star
       
       (*caten 2)
       (*pack-with (lambda (exp exps)
                     (letrec ((lst (reverse `(,exp ,@exps)))
                              (ret (lambda (result)
                                     (if (= (length result) 1) (car result)
                                         `(vector-ref ,(ret (cdr result)) ,(car result))))))
                       (ret lst))))
       done))



(define <InfixArgList>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\,))
       (*caten 2)
       (*pack-with (lambda (arg comma) arg))
       *star
       
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       (*pack-with (lambda (args arg)
                     (append args (list arg))))
       
       (*delayed (lambda () <InfixExpression>))
       
       (*parser <epsilon>)
       (*disj 3)
       done))

(define <InfixFuncall>
  (new (*delayed (lambda ()  (^<infix-skipped*> <InfixArrayGet>)))
       (*parser (char #\())
       (*parser (^<infix-skipped*> <InfixArgList>))
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (f p1 args p2)
                     `(,f ,@args)))
       
       (*delayed (lambda () (^<infix-skipped*> <InfixArrayGet>)))
       (*disj 2)
       done))

(define <InfixParen>
  (new (*parser <InfixSymbol>)
       (*parser <Number>)
       (*delayed (lambda () <InfixSexprEscape>))
       
       
       (*parser (char #\())
       (*delayed (lambda () <InfixExpression>))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (p1 exp p2) exp))
       
       (*disj 4)
       done))

(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       (*pack-with (lambda (pre exp)
                     exp))      
       done))


(define <InfixExpression>
  (^<infix-skipped*>
   <InfixSub-Add>))

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (pre exp) exp)) ;TODO is this how we want to pass this?
       
       done)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEXPR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <sexpr>
  (^<skipped*>
   (new  
    (*parser <Boolean>)
    
    (*parser <Char>)
    (*parser <Number>)
    *not-followed-by
    
    (*parser <Number>)
    (*parser <Symbol>)
    *not-followed-by
    
    (*parser <Symbol>)
    (*parser <String>)
    (*parser <ProperList>)
    (*parser <ImproperList>)
    (*parser <Vector>)
    (*parser <Quoted>)
    (*parser <QuasiQuoted>)
    (*parser <Unquoted>)
    (*parser <UnquoteAndSpliced>)
    (*parser <InfixExtension>)
    (*disj 13)
    done)))

(define <Sexpr> <sexpr>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of assignment 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
                  `(def ,(tag-parser (car def)) ,(tag-parser `(lambda ,(cdr def) ,body))))))

(define pattern-rule-def-MIT-list
  (pattern-rule `(define ,(? 'def (lambda (x)(or (list? x) (pair? x)))) .  ,(? 'body))
                (lambda (def body)
                  `(def ,(tag-parser (car def)) ,(tag-parser `(lambda ,(cdr def) ,(append '(begin) body))))))) 				  

(define pattern-rule-def-REG
  (pattern-rule `(define ,(? 'v var?) ,(? 'e))
                (lambda (v e)
                  `(def ,(tag-parser v) ,(tag-parser e)))))


(define pattern-rule-def-REG-list
  (pattern-rule `(define ,(? 'v var?) . ,(? 'e list?)) (lambda (v e) `(def ,(tag-parser v) ,(tag-parser (append '(begin) e))))))

(define compose-patterns-define
  (compose-patterns
   pattern-rule-def-REG
   pattern-rule-def-REG-list
   pattern-rule-def-MIT
   pattern-rule-def-MIT-list
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

(define parse tag-parser)

;;;;;;;;;;;;;;;;;;;;;;;; end compiler.scm HW2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of assignment 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elliminate nested defines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lambda-words '(lambda-simple lambda-opt lambda-var))

(define ellim-internal
  (lambda (seq)
    (cond [(eq? (car seq) 'seq) (if (not (eq? (caaadr seq) 'def))
                                    seq
                                    (let* ((defs (map (lambda (x) `(set ,@(cdr x))) (filter (lambda (e) (eq? (car e) 'def)) (cadr seq))))
                                           (vars (map (lambda (e) (cadadr e)) defs))
                                           (rest (filter (lambda (e) (not (eq? (car e) 'def))) (cadr seq)))
                                           (init (make-list (length defs) '(const #f))))
                                      `(applic (lambda-simple ,vars (seq ,(append defs rest))) ,init)))]
          [(eq? (car seq) 'def) `(applic (lambda-simple ,(cdadr seq) (set ,(cdr seq))) ((const #f)))]
          [else seq])))

(define eliminate-nested-defines
  (lambda (exp)
    (cond [(not (list? exp)) exp]
          [(member (car exp) lambda-words) (if (eq? (car exp) 'lambda-opt)
                                               `(,(car exp) ,(cadr exp) ,(caddr exp)
                                                            ,(ellim-internal (cadddr exp)))
                                               `(,(car exp) ,(cadr exp)
                                                            ,(ellim-internal (caddr exp))))]
          [else (map (lambda (sub-exp) (eliminate-nested-defines sub-exp)) exp)]
          )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; remove-applic-lambda-nil ;;;;;;;;;;;;;;;;;;;;;;;
(define empty-lambda?
  (lambda (e)
    (eq? #t ((pattern-rule `(applic (lambda-simple ()  . ,(? 'exp)) ()) (lambda (exp) #t)) e fail!))))

(define empty-lambda-pattern
  (pattern-rule `(applic (lambda-simple ()  . ,(? 'exp)) ()) (lambda (exp) (car exp))))


(define remove-applic-lambda-nil
  (lambda (exps)
    
    (letrec ((empty-lambda->exp (lambda (e) (empty-lambda-pattern e fail!)))
             (remove-lambda-empty (lambda (e)
                                    (cond((empty-lambda? e) (remove-lambda-empty (empty-lambda->exp e)))
                                         ((null? e) '())
                                         ((list? e) (cons (remove-lambda-empty (car e)) (remove-lambda-empty (cdr e))))
                                         (else e)
                                         ))))
      
      (remove-lambda-empty exps)
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pe->lex-pe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lambda-simple-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(lambda-simple ,(? 'args list?)  . ,(? 'exp)) (lambda (args exp) #t)) e fail!))
    ))

(define lambda-simple->args
  (lambda (e)
    ((pattern-rule `(lambda-simple ,(? 'args list?)  . ,(? 'exps)) (lambda (args exps) args)) e fail!)
    ))

(define lambda-simple->exps
  (lambda (e)
    ((pattern-rule `(lambda-simple ,(? 'args list?)  . ,(? 'exps)) (lambda (args exps) exps)) e fail!)
    ))

(define lambda-var-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(lambda-var ,(? 'args)  . ,(? 'exp)) (lambda (args exp) #t)) e fail!))
    ))

(define lambda-var->args
  (lambda (e)
    ((pattern-rule `(lambda-var ,(? 'args)  . ,(? 'exps)) (lambda (args exps) args)) e fail!)
    ))

(define lambda-var->exps
  (lambda (e)
    ((pattern-rule `(lambda-var ,(? 'args)  . ,(? 'exps)) (lambda (args exps) exps)) e fail!)
    ))

(define lambda-opt-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exp) #t)) e fail!))
    ))

(define lambda-opt->args
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps)  args )) e fail!)
    ))

(define lambda-opt->rest
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps)  rest )) e fail!)
    ))

(define lambda-opt->args-and-rest
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps)  (append args (list rest)))) e fail!)
    ))

(define lambda-opt->exps
  (lambda (e)
    ((pattern-rule `(lambda-opt ,(? 'args list?) ,(? 'rest) . ,(? 'exp)) (lambda (args rest exps) exps)) e fail!)
    ))
(define var-pattern?
  (lambda (e)
    (eq? #t ((pattern-rule `(var ,(? 'var)) (lambda (var) #t)) e fail!))
    ))

(define var->exp
  (lambda (e)
    ((pattern-rule `(var ,(? 'var)) (lambda (var) var)) e fail!)
    ))

(define index-of-arg
  (lambda (arg args)
    (letrec ((get-index (lambda (lst acc)
                          (if (eq? arg (car lst)) acc
                              (get-index (cdr lst) (+ 1 acc)))
                          )))
      (get-index args 0))
    ))

(define contain-in-previus-args?
  (lambda (e lst)
    (if (null? lst) #f
        (or (eq? e (caar lst))  (contain-in-previus-args? e (cdr lst))))
    ))

(define get-var-place
  (lambda (e lst)
    (if (eq? e (caar lst)) (cadar lst)
        (get-var-place e (cdr lst)))
    ))

(define replace-old-var
  (lambda (e lst)
    (cond ((pair? (get-var-place e lst)) `(bvar ,e ,@(get-var-place e lst)))
          (else `(pvar ,e ,(get-var-place e lst))))
    ))

(define update-previus-args
  (lambda (args previus-args)
    (let* ((lst1 (map (lambda (arg) (list arg (index-of-arg arg args))) (filter (lambda (arg)(not (contain-in-previus-args? arg previus-args)))args)))
           (lst2 (fold-right (lambda (argv acc)
                               (let* ((arg (car argv))
                                      (arg-place (get-var-place arg previus-args)))                    
                                 (cond ((member arg args) (cons (list arg (index-of-arg arg args)) acc))         
                                       ((pair? arg-place) (cons (list arg (cons (+ 1 (car arg-place)) (cdr arg-place))) acc))
                                       (else (cons (list arg (list 0  arg-place)) acc)))
                                 
                                 ))'() previus-args)))
      (append lst1 lst2))
    ))


(define pe->lex-pe
  (lambda (exps) 
    (letrec ((replace-var (lambda (e previus-args)
                            (cond ((var-pattern? e) (if (contain-in-previus-args? (var->exp e) previus-args) (replace-old-var (var->exp e) previus-args) 
                                                        `(fvar ,(var->exp e))))
                                  ((lambda-simple-pattern? e)  `(lambda-simple ,(lambda-simple->args e) 
                                                                               ,@(replace-var (lambda-simple->exps e) (update-previus-args (lambda-simple->args e) previus-args))))
                                  ((lambda-var-pattern? e)  `(lambda-var ,(lambda-var->args e) 
                                                                         ,@(replace-var (lambda-var->exps e) (update-previus-args (list(lambda-var->args e)) previus-args))))
                                  ((lambda-opt-pattern? e)  `(lambda-opt ,(lambda-opt->args e) ,(lambda-opt->rest e) 
                                                                         ,@(replace-var (lambda-opt->exps e) (update-previus-args (lambda-opt->args-and-rest e) previus-args))))
                                  ((null? e) '())
                                  ((list? e) (cons (replace-var (car e) previus-args) (replace-var (cdr e) previus-args)))                    
                                  (else e)
                                  ))))
      (replace-var exps '()))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; box-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define member?
  (lambda (e lst)
    (if (null? lst) #f
        (or (eq? e (car lst)) (member? e (cdr lst))))
    ))

(define var->box-get-var
  (lambda (e)
    ((pattern-rule `(var ,(? 'var)) (lambda (var) `(box-get (var ,var)))) e fail!)
    ))

(define set-var?
  (lambda (e)
    ( (pattern-rule `(set (var ,(? 'var)) ,(? 'val)) (lambda (var val) #t)) e (lambda() #f))
    ))

(define set-var->exp
  (lambda (e)
    ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) var)) e fail!)
    ))

(define set-var->val
  (lambda (e)
    ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) val)) e fail!)
    ))

(define set-pvar?
  (lambda (e)
    ( (pattern-rule `(set (pvar ,(? 'var) ,(? 'minor)) ,(? 'val)) (lambda (var minor val) #t)) e (lambda() #f))
    ))

(define set-pvar->exp
  (lambda (e)
    ((pattern-rule `(set (pvar ,(? 'var)  ,(? 'minor))  ,(? 'val)) (lambda (var minor val) var)) e fail!)
    ))

(define set-pvar->val
  (lambda (e)
    ((pattern-rule `(set (pvar ,(? 'var)  ,(? 'minor))  ,(? 'val)) (lambda (var minor val) val)) e fail!)
    ))

(define set-bvar?
  (lambda (e)
    ( (pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor)) ,(? 'val)) (lambda (var major minor val) #t)) e (lambda() #f))
    ))

(define set-bvar->exp
  (lambda (e)
    ((pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) var)) e fail!)
    ))

(define set-bvar->val
  (lambda (e)
    ((pattern-rule `(set (bvar ,(? 'var) ,(? 'major) ,(? 'minor))  ,(? 'val)) (lambda (var major minor val) val)) e fail!)
    ))

(define set-fvar?
  (lambda (e)
    ( (pattern-rule `(set (fvar ,(? 'var)) ,(? 'val)) (lambda (var val) #t)) e (lambda() #f))
    ))

(define set-fvar->exp
  (lambda (e)
    ((pattern-rule `(set (fvar ,(? 'var))  ,(? 'val)) (lambda (var val) var)) e fail!)
    ))

(define set-fvar->val
  (lambda (e)
    ((pattern-rule `(set (fvar ,(? 'var))  ,(? 'val)) (lambda (var val) val)) e fail!)

    ))

(define bvar-pattern?
  (lambda (e)
    ((pattern-rule `(bvar ,(? 'var) ,(? 'major) ,(? 'minor)) (lambda (var major minor) #t)) e (lambda() #f))
    ))

(define bvar->exp
  (lambda (e)
    ((pattern-rule `(bvar ,(? 'var) ,(? 'major) ,(? 'minor)) (lambda (var major minor) var)) e (lambda() #f))
    ))

(define pvar-pattern?
  (lambda (e)
    ((pattern-rule `(pvar ,(? 'var)  ,(? 'minor)) (lambda (var  minor) #t)) e (lambda() #f))
    ))


(define pvar->exp
  (lambda (e)
    ((pattern-rule `(pvar ,(? 'var)  ,(? 'minor)) (lambda (var  minor) var)) e (lambda() #f))

    ))


(define set->box-set
  (lambda (e)
    ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) `(box-set (var ,var) ,val))) e (lambda() #f))
    ))

(define filter-list
  (lambda (lst lst-to-filter)
    (filter (lambda (e)(not (member? e lst))) lst-to-filter)
    ))

(define set?
  (lambda (el exp)
    (cond ((and (set-var? exp)(eq? (set-var->exp exp) el)) #t)
          ((lambda-simple-pattern? exp) (set? el (lambda-simple->exps exp)))
          ((lambda-var-pattern? exp) (set? el (lambda-var->exps exp)))
          ((lambda-opt-pattern? exp) (set? el (lambda-opt->exps exp)))
          ((null? exp) #f)
          ((list? exp) (or (set? el (car exp)) (set? el (cdr exp))))                    
          (else #f))
    ))


(define bound?
  (lambda (el exp)
    (cond ((set-bvar? exp)(bound? el (set-bvar->val exp)))
          ((set-pvar? exp)(bound? el (set-pvar->val exp)))
          ((set-fvar? exp)(bound? el (set-fvar->val exp)))
          ((and (bvar-pattern? exp)(eq? (bvar->exp exp) el) #t))
          ((lambda-simple-pattern? exp)(if (member? el (lambda-simple->args exp)) #f (bound? el (lambda-simple->exps exp))))
          ((lambda-var-pattern? exp)(if (member? el (list (lambda-var->args exp))) #f (bound? el (lambda-var->exps exp))))
          ((lambda-opt-pattern? exp)(if (member? el (lambda-opt->args-and-rest exp)) #f (bound? el (lambda-opt->exps exp))))
          ((null? exp) #f)
          ((list? exp) (or (bound? el (car exp)) (bound? el (cdr exp))))                    
          (else #f))
    ))

(define get?
  (lambda (el exp)
    (cond  ((set-bvar? exp)(get? el (set-bvar->val exp)))
           ((set-pvar? exp)(get? el (set-pvar->val exp)))
           ((set-fvar? exp)(get? el (set-fvar->val exp)))
           ((and (bvar-pattern? exp)(eq? (bvar->exp exp) el) #t))
           ((and (pvar-pattern? exp)(eq? (pvar->exp exp) el) #t))
           ((lambda-simple-pattern? exp)(if (member? el (lambda-simple->args exp)) #f (get? el (lambda-simple->exps exp))))
           ((lambda-var-pattern? exp)(if (member? el (list (lambda-var->args exp))) #f (get? el (lambda-var->exps exp))))
           ((lambda-opt-pattern? exp)(if (member? el (lambda-opt->args-and-rest exp)) #f (get? el (lambda-opt->exps exp))))
           ((null? exp) #f)
           ((list? exp) (or (get? el (car exp)) (get? el (cdr exp))))                    
           (else #f))
    ))

(define set-arg?
  (lambda (el exps)
    (let ((lex-pe-exp (pe->lex-pe exps)))
      (cond ((lambda-simple-pattern? exps)
             (and (set? el (lambda-simple->exps  exps)) (bound? el (lambda-simple->exps lex-pe-exp)) (get? el (lambda-simple->exps lex-pe-exp))))
            ((lambda-var-pattern? exps)
             (and (set? el (lambda-var->exps  exps)) (bound? el (lambda-var->exps lex-pe-exp)) (get? el (lambda-var->exps lex-pe-exp))))
            ((lambda-opt-pattern? exps)
             (and (set? el (lambda-opt->exps  exps)) (bound? el (lambda-opt->exps lex-pe-exp)) (get? el (lambda-opt->exps lex-pe-exp))))
            ))))

(define set-args
  (lambda (args exps)
    (let ((lex-pe-exp (pe->lex-pe exps)))
      (cond ((lambda-simple-pattern? exps)
             (filter (lambda (el)(and (set? el (lambda-simple->exps  exps)) (bound? el (lambda-simple->exps lex-pe-exp)) (get? el (lambda-simple->exps lex-pe-exp)))) args))
            ((lambda-var-pattern? exps)
             (filter (lambda (el)(and (set? el (lambda-var->exps  exps)) (bound? el (lambda-var->exps lex-pe-exp)) (get? el (lambda-var->exps lex-pe-exp)))) args))
            ((lambda-opt-pattern? exps)
             (filter (lambda (el)(and (set? el (lambda-opt->exps  exps)) (bound? el (lambda-opt->exps lex-pe-exp)) (get? el (lambda-opt->exps lex-pe-exp)))) args))
            ))))





(define update-set-list
  (lambda (args old-set-list exp)
    (let((new-set-list (filter-list args old-set-list)))
      (append new-set-list (filter (lambda (e)(set-arg? e exp)) args)))


    ))

(define make-seq-list
  (lambda (lst)
    (fold-left (lambda (e acc) (append acc `((set (var ,e) (box (var ,e)))))) '() lst)
    ))

(define box-set
  (lambda (exps) 
    (letrec ((replace-var (lambda (e set-list)
                            (cond ((set-var? e) (if (member? (set-var->exp e) set-list)
                                                    ((pattern-rule `(set (var ,(? 'var))  ,(? 'val)) (lambda (var val) `(box-set (var ,var) ,(replace-var val set-list)))) e (lambda() #f))  e))
                                  ((var-pattern? e) (if (member? (var->exp e) set-list) (var->box-get-var e) e))
                                  ((lambda-simple-pattern? e) (if (null? (set-args (lambda-simple->args e) e)) `(lambda-simple ,(lambda-simple->args e) 
                                                                                                                               ,@(replace-var (lambda-simple->exps e) (update-set-list (lambda-simple->args e) set-list e)))
                                                                  `(lambda-simple ,(lambda-simple->args e) (seq (,@(make-seq-list (set-args (lambda-simple->args e) e))
                                                                                                                 ,@(replace-var (lambda-simple->exps e) (update-set-list (lambda-simple->args e) set-list e)))))))
                                  ((lambda-var-pattern? e) (if (null? (set-args (list (lambda-var->args e)) e)) `(lambda-var ,(lambda-var->args e) 
                                                                                                                             ,@(replace-var (lambda-var->exps e) (update-set-list (list (lambda-var->args e)) set-list e)))
                                                               `(lambda-simple ,(lambda-var->args e) (seq (,@(make-seq-list (set-args (list (lambda-var->args e)) e))
                                                                                                           ,@(replace-var (lambda-var->exps e) (update-set-list (list (lambda-var->args e)) set-list e)))))))
                                  ((lambda-opt-pattern? e) (if (null? (set-args (lambda-opt->args-and-rest e) e)) `(lambda-opt ,(lambda-opt->args e) ,(lambda-opt->rest e) 
                                                                                                                               ,@(replace-var (lambda-opt->exps e) (update-set-list (lambda-opt->args-and-rest e) set-list e)))
                                                                  `(lambda-opt ,(lambda-opt->args e) ,(lambda-opt->rest e) (seq (,@(make-seq-list (set-args (lambda-opt->args-and-rest e) e))
                                                                                                                 ,@(replace-var (lambda-opt->exps e) (update-set-list (lambda-opt->args-and-rest e) set-list e)))))))
                                  ((null? e) '())
                                  ((list? e) (cons (replace-var (car e) set-list) (replace-var (cdr e) set-list)))                    
                                  (else e)
                                  ))))


      (replace-var exps '()))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; annotate tail calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define leaf-words '(const var bvar fvar pvar))

(define minus1
  (lambda (n)
    (- n 1)))

(define tc-map
  (lambda (exp bools)
    (map (lambda (sub-exp t?) (tc-internal sub-exp t?)) exp bools)))

(define tc-internal
  (lambda (exp tc?)
    (let ((tag (car exp))
          (body (cdr exp)))
      (cond [(member tag leaf-words) exp]
            [(eq? tag 'applic) (let ((bools (make-list (length body) #f)))
                                 (if tc?
                                     `(tc-applic ,(tc-map body bools))
                                     `(applic ,(tc-map body bools))))]
            [(eq? tag  'or) (let ((bools (append (make-list (minus1 (length body)) #f) `(,tc?))))
                              (cons tag (tc-map body bools)))]
            [(eq? tag 'if3) (let ((bools `(#f ,tc? ,tc?)))
                              (cons tag (tc-map body bools)))]
            [(eq? tag 'def) (let ((bools '(#f #f)))
                              (cons tag (tc-map body bools)))]
            [(eq? tag 'seq) (let ((bools (append (make-list (minus1 (length body)) #f) `(,tc?))))
                              (cons tag (tc-map body bools)))]
            [(member tag lambda-words) (if (eq? tag 'lambda-opt)
                                           `(,tag ,(car body) ,(cadr body) ,(tc-internal (caddr body) #t))
                                           `(,tag ,(car body) ,(tc-internal (cadr body) #t)))]
            [(list? tag) (if (null? body)
                             `(,(tc-internal tag tc?))
                             (cons (tc-internal tag tc?) (tc-internal body tc?)))]
            [else `(ERROR in tc-internal. case not covered: ,tag ,exp)] ; reached if we missed something
    ))))

(define annotate-tc
  (lambda (exp)
    (tc-internal exp #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x1 '(lambda(x)
              (begin
                (begin (define a 4) (define b 3))
                (begin (define c 2) (define d 1))
                (+ a b c d))))

(define p1 '(lambda-simple
             (x)
             (seq
              ((def (var a) (const 4))
               (def (var b) (const 3))
               (def (var c) (const 2))
               (def (var d) (const 1))
               (applic (var +) ((var a) (var b) (var c) (var d)))))))
(define seq (caddr p1))
(define r1 '(lambda-simple
             (x)
             (applic
              (lambda-simple
               (a b c d)
               (seq ((set (var a) (const 4))
                     (set (var b) (const 3))
                     (set (var c) (const 2))
                     (set (var d) (const 1))
                     (applic (var +) ((var a) (var b) (var c) (var d))))))
              ((const #f) (const #f) (const #f) (const #f)))))

(define x2 '(define my-even?
              (lambda (e)
                (define even? (lambda (n) (or (zero? n) (odd? (- n 1)))))
                (define odd?
                  (lambda (n) (and (positive? n) (even? (- n 1)))))
                (even? e))))

(define x3 '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))))

(define x4 '(x (lambda (x) (x (lambda () (x (lambda () (x x))))))))

(define x5 '(lambda (f) ((lambda (x) (f (lambda s (apply (x x) s)))) (lambda (x) (f (lambda s (apply (x x) s)))))))

