;;; pc.scm
;;; A simple implementation of parsing combinators
;;;
;;; Programmer: Mayer Goldberg, 2016

#lang scheme ;TODO delete this when running in chez

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of compiler.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Number;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <digit-0-9> 
  (range #\0 #\9))

(define <digit-1-9> 
  (range #\1 #\9))

(define <Natural>
  (new  (*parser  (char #\0)) 
        (*pack (lambda (_) 0))
        
        (*parser <digit-1-9> )
        (*parser <digit-0-9>) *star
        (*caten 2) 
        (*pack-with (lambda (x y) (string->number
                                   (list->string `(,x ,@y)))))
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
       (*parser (char-ci #\f))
       (*disj 2)
       
       (*caten 2)
       (*pack-with (lambda (hash val)
                     (list->string (list hash val))))
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
       (*pack (lambda (char)
                (list char)))
       done))

(define ^<named-char>
  (lambda (str)
    (new (*parser (word-ci str))
         (*pack (lambda (lst)
                  (list->string lst)))
         done)))

(define <NamedChar>
  (new (*parser (^<named-char> "lambda"))
       (*parser (^<named-char> "newline"))
       (*parser (^<named-char> "nul"))
       (*parser (^<named-char> "page"))
       (*parser (^<named-char> "return"))
       (*parser (^<named-char> "space"))
       (*parser (^<named-char> "tab"))
       (*disj 7)
       (*pack (lambda (str)
                (string->list str)))
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
       
       (*caten 2)
       (*pack-with (lambda (x chars)
                     (list (integer->char
                            (foldl (lambda (byte total)
                                     (+ (* total 16) byte))
                                   0
                                   chars)))))
       done))

(define <Char>
  (new (*parser <CharPrefix>)
       
       (*parser <HexUnicodeChar>)
       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)      
       (*disj 3)
       
       (*caten 2)
       (*pack-with (lambda (pre char)
                     (list->string `(,@pre ,@char)))) ; TODO do we want to pass the prefix?
       done))


;(define <StringVisibleChar> no one uses this anymore
;  (new (*parser <any-char>)
;       (*guard (lambda (n)
;                 (< (- (char->integer #\space) 1) (char->integer n))))
;       (*pack (lambda (char)
;                (list char)))
;       done))

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
  (new (*parser (range #\0 #\9))
       (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*parser <special-char>)
       (*disj 4)
       done))

(define <Symbol>
  (new (*parser <SymbolChar>)
       *plus
       (*pack (lambda (chars)
                (list->string chars)))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ProperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ProperList>
  (new (*parser (char #\())
       (*delayed (lambda () <Sexpr>)) *star
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (_x exp _y)
                     `(,@exp)))
       
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ImproperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ImproperList>
  (new (*parser (char #\())
       (*delayed (lambda () <Sexpr>)) *plus
       (*parser (char #\.))
       (*delayed (lambda () <Sexpr>))
       (*parser (char #\)))
       (*caten 5)
       (*pack-with (lambda (_x exp1 _y exp2 _z)
                     `(,@exp1  ,@exp2)))
       
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Vector;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Vector>
  (new (*parser (char #\#))
       (*parser (char #\())
       (*delayed (lambda () <Sexpr>)) *star
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (_x _y exp _z)
                     (list->vector `( ,@exp))))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Quoted>
  (new (*parser (char #\'))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quote exp)))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QuasiQuoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quasiquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Unquoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Unquoted>
  (new (*parser (char #\,))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'unquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UnquoteAndSpliced;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <UnquoteAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@))
       (*delayed (lambda () <Sexpr>))
       (*caten 3)
       (*pack-with (lambda (_x _y exp )
                     (list 'unquote-splicing exp)))
       done))

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
  (new (*parser <Symbol>)
       (*parser <operator-symbol>)
       *diff
       ; TODO this also fails when the string starts
       ; with an <operator-symbol> 
       ; Example: "+23" fails, "23+" doesn't
       done))

(define <PowerSymbol>
  (new (*parser (char #\^))
       (*parser (word "**"))
       (*disj 2)
       done))        

(define <InfixPrefixExtensionPrefix>
  (new (*parser (word "##"))
       (*parser (word "#%"))
       (*disj 2)
       done))


(define <InfixAdd>
  (new (*parser <Number>)
       (*parser <InfixSymbol>)
       (*delayed (lambda () <InfixExpression>))
       (*disj 3)
       
       (*parser (char #\+))
       (*delayed (lambda () <InfixExpression>))       
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(+ ,exp1 ,exp2)))
       done))

(define <InfixNeg>
  (new (*parser (char #\-))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       (*pack-with (lambda (op exp end)
                     `(- ,exp)))
       done))

(define <InfixSub>
  (new (*parser <Number>)
       (*parser <InfixSymbol>)
       (*delayed (lambda () <InfixExpression>))
       (*disj 3)
       
       (*parser (char #\-))
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(- ,exp1 ,exp2)))
       done))

(define <InfixMul>
  (new (*parser <Number>)
       (*parser <InfixSymbol>)
       (*delayed (lambda () <InfixExpression>))
       (*disj 3)
       
       (*parser (char #\*))
       (*delayed (lambda () <InfixExpression>)) 
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(* ,exp1 ,exp2)))
       done))

(define <InfixDiv>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Integer>)
       (*caten 3)
       (*pack-with (lambda (n1 div n2)
                     `(/ ,n1 ,n2)))
       
       (*parser <Number>)
       (*parser <InfixSymbol>)
       (*delayed (lambda () <InfixExpression>))
       (*disj 3)
       
       (*parser (char #\/))
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(/ ,exp1 ,exp2)))
       
       (*disj 2)
       done))

(define <InfixPow>
  (new (*parser <Number>)
       (*parser <InfixSymbol>)
       (*delayed (lambda () <InfixExpression>))
       (*disj 3)
       
       (*parser <PowerSymbol>)
       
       (*parser <Number>)
       (*parser <InfixSymbol>)
       (*delayed (lambda () <InfixParen>))
       (*disj 3)
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(expt ,exp1 ,exp2)))
       done))

(define <infix-not-number>
  (new (*delayed (lambda ()
                   (new (*parser <InfixExpression>)
                        (*parser <Number>)
                        *diff
                        done)))
       done))

(define <InfixArrayGet>
  (new (*delayed (lambda ()
                   (new (*parser <InfixExpression>)
                        (*parser <Number>)
                        *diff
                        done)))
       
       (*parser (char #\[))
       (*delayed (lambda ()
                   (new (*parser <Number>)
                        (*parser <InfixExpression>)
                        (*disj 2)
                        done)))
       
       (*parser (char #\]))
       (*caten 4)
       (*pack-with (lambda (arr p1 index p2)
                     (cons arr index))) ; send as pair (list . index) let compiler deal with this
       done))

(define <InfixArgList>
  (new (*delayed (lambda () <InfixExpression>))
       *star
       done))

(define <InfixFuncall>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\())
       (*parser <InfixArgList>)
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (f p1 args p2)
                     `(,f ,@args)))
       done))

(define <InfixParen>
  (new (*parser (char #\())
       (*delayed (lambda () <InfixExpression>))
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (p1 exp p2)
                     `(,@exp)))
       done))

(define <InfixSexprEscape>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda (pre exp)
                     exp)) ; TODO what should we return here?       
       done))


(define <InfixExpression>
  (new (*parser <InfixParen>)
       (*parser <InfixPow>)
       (*parser <InfixMul>)
       (*parser <InfixDiv>)
       (*parser <InfixAdd>)
       (*parser <InfixNeg>)
       (*parser <InfixSub>)
       ;(*parser <InfixArrayGet>)
       ;(*parser <InfixFuncall>)
       ;(*parser <InfixSexprEscape>)
       (*parser <Number>)
       (*parser <InfixSymbol>)
       (*disj 9) ;12)
       done))

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       done)) ;TODO how do we want to pass this?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  COMMENT  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define <Whitespace>
  (new (*parser <any-char>)
       (*parser <VisibleSimpleChar>)
       *diff
       (*pack (lambda (a) null))
       done))


(define end-line-chars
  (list #\newline #\page #\return))

(define <line-comment>
  (new (*parser (char #\;))
       
       (*parser <any-char>)
       (*guard (lambda (x)
                 (andmap (lambda (char)
                           (not (eq? x char)))
                         end-line-chars)))
       *star
       (*caten 2)
       (*pack-with (lambda (a b) null))
       done))

(define <expr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <Sexpr>))
       (*caten 2)
       (*pack-with (lambda (a b) null))
       done))

(define <Comment> ; TODO where do we push this?
  (new (*parser <line-comment>)
       (*parser <expr-comment>)
       (*disj 2)
       done))

(define <Skip>
  (new (*parser <Comment>)
       (*parser <Whitespace>)
       (*disj 2)
       (*pack (lambda (a) null))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEXPR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Sexpr>
  (new (*parser <Boolean>)
       (*parser <Char>)
       (*parser <Number>)
       (*parser <String>)
       (*parser <Symbol>)
       (*parser <ProperList>)
       (*parser <ImproperList>)
       (*parser <Vector>)
       (*parser <Quoted>)
       (*parser <QuasiQuoted>)
       (*parser <Unquoted>)
       (*parser <UnquoteAndSpliced>)
       ;       (*parser <InfixExtension>)
       (*parser <Skip>) *star
       (*disj 13);14)
       done))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;STRING OLD;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(define <string-char>
;  (new (*parser <string-meta-char>)
;       (*parser <any-char>)
;	   
;       (*parser (char #\"))
;       (*parser (char #\\))
;	   
;       (*disj 2)
;       *diff
;	   
;       (*disj 2)
;       done))
;
;(define <string>
;  (new (*parser (char #\"))
;       (*parser <string-char>) *star
;       (*parser (char #\"))
;       (*caten 3)
;       (*pack-with (lambda(_x chars _y) (list->string chars)))
;       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Symbol-old;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define <symbol-a-z>
; (range #\a #\z))
; (define <symbol-A-Z>
; (range #\A #\Z))

; (define <symbol-meta-char>
; (new (*parser (char #\!))
; (*parser (char #\$))
; (*parser (char #\^))
; (*parser (char #\*))
; (*parser (char #\-))
; (*parser (char #\_))
; (*parser (char #\=))
; (*parser (char #\+))
; (*parser (char #\<))
; (*parser (char #\>))
; (*parser (char #\?))
; (*parser (char #\/))

; (*disj 12)
; done))

; (define <SymbolChar>
; (new (*parser <digit-0-9>)
; (*pack (lambda (x)  (string->symbol (list->string (list x)))))
; (*parser <symbol-a-z>)
; (*pack (lambda (x) (string->symbol  (list->string (list x)))))
; (*parser <symbol-A-Z>)
; (*pack (lambda (x) (string->symbol  (list->string (list x)))))
; (*parser <symbol-meta-char>)
; (*pack (lambda (x) (string->symbol  (list->string (list x)))))
; (*disj 4)
; done))

; (define <Symbol>
; (new (*parser <SymbolChar>)*plus
; (*pack (lambda (x)   x))
; done))
