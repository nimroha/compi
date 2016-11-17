

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of compiler.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  SEXPR  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <Sexpr>
  <fail>)
;  (new (*parser <Boolean>)
;       (*parser <Char>)
;       (*parser <Number>)
;       (*parser <String>)
;       (*parser <Symbol>)
;       (*parser <ProperList>)
;       (*parser <ImproperList>)
;       (*parser <Vector>)
;       (*parser <Quoted>)
;       (*parser <QuasiQuoted>)
;       (*parser <Unquoted>)
;       (*parser <UnquoteAndSplice>)
;       (*parser <InfixExtension>)
;       (*disj 13)
;       done))

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
       (*parser <Sexpr>) *star
       (*parser (char #\)))
       
       (*caten 3)
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ImproperList;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <ImproperList>
  (new (*parser (char #\())
       (*parser <Sexpr>) *plus
       (*parser (char #\.))
       (*parser <Sexpr>)
       (*parser (char #\)))
       
       (*caten 5)
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Vector;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Vector>
  (new (*parser (char #\#))
       (*parser (char #\())
       (*parser <Sexpr>) *star
       (*parser (char #\)))
       
       (*caten 4)
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Quoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Quoted>
  (new (*parser (char #\'))
       (*parser <Sexpr>)
       
       (*caten 2)
       done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;QuasiQuoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*parser <Sexpr>)
       
       (*caten 2)
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Unquoted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <Unquoted>
  (new (*parser (char #\,))
       (*parser <Sexpr>)
       
       (*caten 2)
       done))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UnquoteAndSpliced;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <UnquoteAndSpliced>
  (new (*parser (char #\,))
       (*parser (char #\@))
       (*parser <Sexpr>)
       
       (*caten 3)
       done))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  INFIX  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\+))
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(+ ,exp1 ,exp2)))
       done))

(define <InfixNeg>
  (new (*parser (char #\-))
       (*delayed (lambda () <InfixExpression>))
       (*parser <end-of-input>)
       (*caten 3)
       (*pack-with (lambda (op exp end)
                     `(- ,exp)))
       done))

(define <InfixSub>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\-))
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(- ,exp1 ,exp2)))
       done))

(define <InfixMul>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\*))
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(* ,exp1 ,exp2)))
       done))

(define <InfixDiv>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\/))
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(/ ,exp1 ,exp2)))
       done))

(define <InfixPow>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser <PowerSymbol>)
       (*delayed (lambda () <InfixExpression>))
       (*caten 3)
       (*pack-with (lambda (exp1 op exp2)
                     `(expt ,exp1 ,exp2)))
       done))

(define <InfixArrayGet>
  (new (*delayed (lambda () <InfixExpression>))
       (*parser (char #\[))
       (*delayed (lambda () <InfixExpression>))
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
       (*parser <Sexpr>)
       (*caten 2)
       (*pack-with (lambda (pre exp)
                     exp)) ; TODO what should we return here?       
       done))

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


(define <InfixExpression>
  (new ;(*parser <InfixPow>)
   ;(*parser <InfixMul>)
   ;(*parser <InfixDiv>)
   (*parser <InfixAdd>)
   (*parser <InfixNeg>)
   ;(*parser <InfixSub>)
   ;(*parser <InfixParen>)
   ;(*parser <InfixArrayGet>)
   ;(*parser <InfixFuncall>)
   ;(*parser <InfixSexprEscape>)
   (*parser <InfixSymbol>)
   (*parser <Number>)
   (*disj 4); 12)
   done))

(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       done)) ;TODO how do we want to pass this?


 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end of compiler.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;mayer's code:
(define <hex-digit>
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

(define <XX>
  (new (*parser <hex-digit>)
       (*parser <hex-digit>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* h 16))))
       done))

(define <XXXX>
  (new (*parser <XX>)
       (*parser <XX>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* 256 h))))
       done))

(define <hex-char>
  (new (*parser (word-ci "\\{0x"))

       (*parser <XXXX>)
       (*parser <XX>)
       (*disj 2)
       (*pack integer->char)

       (*parser (char #\}))
       (*caten 3)
       (*pack-with (lambda (_< ch _>) ch))
       done))

(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
	 (*pack (lambda (_) ch))
	 done)))
	 
	 
(define <string-meta-char>
  (new (*parser <hex-char>)
       (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       (*parser (^<meta-char> "\\{alef}" (integer->char 1488)))
       (*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))
       (*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))

       (*disj 11)
       done))	 
       





