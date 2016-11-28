;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of compiler.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/compi/ass1/pc.scm")

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
                     (letrec ((build-hex (lambda (total lst)
                                           (if (null? lst)
                                               total
                                               (build-hex (+ (* total 16) (car lst)) (cdr lst))))))
                       (list (integer->char (build-hex 0 chars))))))
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
  (new (*parser (range #\a #\z))
       (*parser (range #\A #\Z))
       (*parser <special-char>)
       (*disj 3)
       done))

(define <Symbol> ; currently not supporting -3a
  (new (*parser <digit-0-9>)
       *plus
       (*parser <SymbolChar>)
       (*parser (char #\/))
       *diff
       (*parser <SymbolChar>)
       (*parser <digit-0-9>)
       (*disj 2)
       *star
       (*caten 3)
       
       (*parser <SymbolChar>)
       (*parser <digit-0-9>)
       (*disj 2)
       *star      
       (*parser <SymbolChar>)
       (*parser (char #\/))
       (*parser (char #\+))
       (*parser (char #\-))
       (*disj 3)
       *diff
       (*parser <SymbolChar>)
       (*parser <digit-0-9>)
       (*disj 2)
       *star 
       (*caten 3)
       
       (*parser <SymbolChar>)
       *plus
       
       (*parser (char #\+))
       (*parser (char #\-))
       (*disj 2)
       (*parser <digit-0-9>)
       *not-followed-by
       *plus
       
       (*disj 4)
       (*pack-with (lambda args
                     (string->symbol (list->string (flatten args)))))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ProperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ProperList>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr-2>)) *star
       (*parser (char #\)))
       (*caten 3)
       (*pack-with (lambda (_x exp _y)
                     `(,@exp)))
       
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ImproperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ImproperList>
  (new (*parser (char #\())
       (*delayed (lambda () <sexpr-2>)) *plus
       (*parser (char #\.))
       (*delayed (lambda () <sexpr-2>))
       (*parser (char #\)))
       (*caten 5)
       (*pack-with (lambda (_p1 exp1 _dot exp2 _p2)
                     `( ,@(exp1) . ,(exp2))))
       
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Vector;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Vector>
  (new (*parser (char #\#))
       (*parser (char #\())
       (*delayed (lambda () <sexpr-2>)) *star
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (_x _y exp _z)
                     (list->vector `( ,@exp))))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Quoted>
  (new (*parser (char #\'))
       (*delayed (lambda () <sexpr-2>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quote exp)))
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QuasiQuoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*delayed (lambda () <sexpr-2>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'quasiquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Unquoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Unquoted>
  (new (*parser (char #\,))
       (*delayed (lambda () <sexpr-2>))
       (*caten 2)
       (*pack-with (lambda (_x exp )
                     (list 'unquote exp)))
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UnquoteAndSpliced;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <UnquoteAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@))
       (*delayed (lambda () <sexpr-2>))
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
       (*delayed (lambda () <sexpr-2>))
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
  (^<skipped*>
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
  (^<skipped*>
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
       
       (*parser (char #\[))
       (*delayed (lambda () <InfixExpression>))
       (*parser (char #\]))
       (*caten 3)
       (*pack-with (lambda (_x exp _y) exp))
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
       *star
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       (*pack-with (lambda args
                     (letrec ((drop-commas (lambda (lst acc)
                                             (if (null? lst)
                                                 acc
                                                 (if (odd? (length lst))
                                                     (drop-commas (cdr lst) (append acc (list (car lst))))
                                                     (drop-commas (cdr lst) acc))))))
                       (drop-commas (flatten args) empty))))
       
       
       (*delayed (lambda () <InfixExpression>))
       
       (*parser <epsilon>)
       (*disj 3)
       done))

(define <InfixFuncall>
  (new (*delayed (lambda () <InfixArrayGet>))
       (*parser (char #\())
       (*parser <InfixArgList>)
       (*parser (char #\)))
       (*caten 4)
       (*pack-with (lambda (f p1 args p2)
                     `(,f ,@args)))
       
       (*delayed (lambda () <InfixArrayGet>))
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
       (*delayed (lambda () <sexpr-2>))
       (*caten 2)
       (*pack-with (lambda (pre exp)
                     exp))      
       done))


(define <InfixExpression>
  (^<skipped*>
   <InfixSub-Add>))

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (pre exp) exp)) ;TODO is this how we want to pass this?
       
       done)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEXPR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <sexpr-2>
  (^<skipped*>
   (new  
    (*parser <Boolean>)
    (*parser <Char>)
    (*parser <Symbol>)
    (*parser <Number>)
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


