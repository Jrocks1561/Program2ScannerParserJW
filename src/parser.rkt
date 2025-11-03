#lang racket
(require "scanner.rkt" racket/match)
(provide parse parse-file)

;;I had GPT generate code for me after having my scanner working completley some things modified for easier use.
(struct PT (label children) #:transparent)

;;helps make prettier output for easier reading
(define (print-pt tree [indent 0])
  (define (spaces n) (make-string n #\space))
  (match tree
    [(PT lbl kids)
     (printf "~a~a\n" (spaces indent) lbl)
     (for ([k kids]) (print-pt k (+ indent 2)))]))

;;hold scanner obj & cur token
(struct P (sc current) #:mutable)

;;----Helpers--
(define (make-parser src)
  (define sc (make-scanner src))
  (P sc (next-token sc)))

(define (peek p)       (P-current p))
(define (tt p)         (token-type (peek p)))
(define (lex p)        (token-lexeme (peek p)))
(define (advance! p)   (set-P-current! p (next-token (P-sc p))))
(define (type-is? p s) (eq? (tt p) s))

;;Helpers
(define (match! p s)
  (if (type-is? p s) (begin (advance! p) #t) #f))

(define (expect! p s [msg #f])
  (unless (match! p s)
    (define t (peek p))
    (error 'parser
           (format "Expected ~a at ~a:~a, found ~a"
                   (or msg s) (token-line t) (token-col t) (token-type t)))))

(define (skip-newlines! p)
  (let loop () (when (type-is? p 'NEWLINE) (advance! p) (loop))))

;; ---------- Entry points ----------
(define (parse-file path)
  (define src (file->string path))
  (define p   (make-parser src))
  (define tree (parse-program p))
  (skip-newlines! p)
  (expect! p 'EOF "end-of-file")
  tree)

(define (parse filename)
;;wrap so doesnt crash on fail
  (with-handlers ([exn:fail?
                   (lambda (e)
                   ;;error message
                     (define m (exn-message e))
                     ;;getting the line # of error 
                     (define line
                       (cond
                         [(regexp-match #px"line\\s+([0-9]+)" m) => (lambda (mm) (second mm))]
                         [(regexp-match #px"\\sat\\s+([0-9]+):([0-9]+)" m) => (lambda (mm) (second mm))]
                         [else "??"]))
                         ;;print error as requested on assighment
                     (printf "Syntax error on line ~a\n" line)
                     (void))])
     ;;parse tree                
    (define tree (parse-file filename))
    (printf "Accept\n")
    (print-pt tree)
    ;;Needed void stment as Ai didnt and it would double print
    (void))) 

;; ---------- Grammar ----------
(define (parse-program p)
  (skip-newlines! p)
  (PT "program" (list (parse-stmt-list p))))

;;helps with ending a statement
(define (stmt-list-stop? p)
  (or (type-is? p 'END) (type-is? p 'ELSE) (type-is? p 'EOF)))

;;parse staments seprated by ; or newlines
(define (parse-stmt-list p)
  (let loop ([kids '()])
    (skip-newlines! p)
    (cond
      [(stmt-list-stop? p)
       (if (null? kids)
           (PT "stmt-list" (list (PT "ε" '())))
           (PT "stmt-list" (reverse kids)))]
      [else
       (define st (parse-stmt p))
       (let sloop () (when (match! p 'SEMI) (sloop))) ; eat 0+ ';'
       (loop (cons (PT "stmt" (list st)) kids))])))

;;decided which stament to parse
(define (parse-stmt p)
  (cond
    [(type-is? p 'IF)     (PT "if-statement"       (list (parse-if p)))]
    [(type-is? p 'WHILE)  (PT "while-statement"    (list (parse-while p)))]
    [(type-is? p 'READ)   (PT "read-statement"     (list (parse-read p)))]
    [(type-is? p 'PRINT)  (PT "print-statementt"    (list (parse-print p)))]
    [(type-is? p 'BEGIN)  (PT "compound-statement" (list (parse-block p)))]
    [(type-is? p 'ID)     (PT "assign-statement"   (list (parse-assign p)))]
    [else
     (define t (peek p))
     (error 'parser
            (format "Expected statement at ~a:~a, found ~a"
                    (token-line t) (token-col t) (token-type t)))]))

;;begins staemnt and finds matching END
(define (parse-block p)
  (expect! p 'BEGIN)
  (skip-newlines! p)
  (define sl (parse-stmt-list p))
  (expect! p 'END)
  (PT "BEGIN...END" (list (PT "BEGIN" '()) sl (PT "END" '()))))

;parse If statement
(define (parse-if p)
  (expect! p 'IF)
  ;;left hand
  (define e1 (parse-expr p))         
  (define op (parse-comp-op-pt p)) 
  ;;right hand 
  (define e2 (parse-expr p))
  ;;looing for     
  (expect! p 'THEN)
  (expect! p 'BEGIN)
  (skip-newlines! p)
  (define tblock (parse-stmt-list p))
  (expect! p 'END)
  (skip-newlines! p)

;;Else
  (define else-part
    (if (match! p 'ELSE)
        (begin
          (expect! p 'BEGIN)
          (skip-newlines! p)
          (let ([eblock (parse-stmt-list p)])
            (expect! p 'END)
            (PT "ELSE" (list (PT "BEGIN" '()) eblock (PT "END" '())))))
        (PT "ε" '())))
  ;;parse tree for if 
  (PT "IF"
      (list (PT "IF" '()) e1 op e2 (PT "THEN" '())
            (PT "BEGIN" '()) tblock (PT "END" '())
            else-part)))
;;parse while loop 
(define (parse-while p)
  (expect! p 'WHILE)
  ;;left expression
  (define e1 (parse-expr p))  
  ;;compare      
  (define op (parse-comp-op-pt p))
  (define e2 (parse-expr p))
  (expect! p 'BEGIN)
  (skip-newlines! p)
  (define body (parse-stmt-list p))
  (expect! p 'END)
  (PT "WHILE"
      (list (PT "WHILE" '()) e1 op e2 (PT "BEGIN" '()) body (PT "END" '()))))

;parse assighment operator
(define (parse-assign p)
  (define id (parse-id p))
  (expect! p 'ASSIGN)
  (define e (parse-expr p))
  (PT ":=" (list id (PT ":=" '()) e)))

;;my brain hates this but just ensrues read is correct
(define (parse-read p)
  (expect! p 'READ)
  (PT "READ" (list (PT "READ" '()) (parse-id p))))

(define (parse-print p)
  (expect! p 'PRINT)
  (PT "PRINT" (list (PT "PRINT" '()) (parse-expr p))))

;; ---------- Expressions 


(define (parse-expr p)
  (define a (parse-additive p))
  ;;parse tre node
  (PT "expr" (list a)))

;;parse + or - exp
(define (parse-additive p)
  (define t (parse-term p))
  (let loop ([kids (list t)])
    (cond
      [(match! p 'PLUS)
       (define rhs (parse-term p))
       (loop (append kids (list (PT "+" '()) rhs)))]
      [(match! p 'MINUS)
       (define rhs (parse-term p))
       (loop (append kids (list (PT "-" '()) rhs)))]
      [else (PT "additive" kids)])))

;;parse terms * and /
(define (parse-term p)
  (define f (parse-factor p))
  ;;get pieces of the term
  (let loop ([kids (list f)])
    (cond
      [(match! p 'STAR)
       (define rhs (parse-factor p))
       (loop (append kids (list (PT "*" '()) rhs)))]
      [(match! p 'SLASH)
       (define rhs (parse-factor p))
       (loop (append kids (list (PT "/" '()) rhs)))]
      [else (PT "term" kids)])))

(define (parse-factor p)
  (cond
  ;;if id consume and return as factor
    [(type-is? p 'ID)   (PT "factor" (list (parse-id p)))]
    ;;if num consume and return as factor
    [(type-is? p 'NUM)  (PT "factor" (list (parse-num p)))]
    ;;if left paren require matchign right paren
    [(match! p 'LPAREN)
     (define e (parse-expr p))
     (expect! p 'RPAREN)
     (PT "factor" (list (PT "(" '()) e (PT ")" '())))]
     ;;erroe for factorr
    [else
     (define t (peek p))
     (error 'parser
            (format "Expected factor at ~a:~a, found ~a"
                    (token-line t) (token-col t) (token-type t)))]))

;; ---------- terminals 

;;parse identifier and make sur eits ID token
(define (parse-id p)
  (define t (peek p))
  (expect! p 'ID "identifier")
  (PT (format "ID → ~a" (token-lexeme t)) '()))

;parse number token make sure its num and consue
(define (parse-num p)
  (define t (peek p))
  (expect! p 'NUM "number")
  (PT (format "NUM → ~a" (token-lexeme t)) '()))

;;comparesion operators
(define (comp-op-token? sym)
;;return t if found
  (memq sym '(EQ NE LT LE GT GE)))

;;parse comparison operators
(define (parse-comp-op-pt p)
;;looking at current token
  (match (tt p)
  ;;match it consume it && return node
    ['EQ (advance! p) (PT "="  '())]
    ['NE (advance! p) (PT "<>" '())]
    ['LT (advance! p) (PT "<"  '())]
    ['LE (advance! p) (PT "<=" '())]
    ['GT (advance! p) (PT ">"  '())]
    ['GE (advance! p) (PT ">=" '())]
    [else
     (define t (peek p))
     (error 'parser
            (format "Expected comparison operator at ~a:~a, found ~a"
                    (token-line t) (token-col t) (token-type t)))]))
