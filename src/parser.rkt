#lang racket
(require "scanner.rkt" "tokens.rkt")

(provide parse-file)          ; main entry (file -> AST)
;; (provide parse-program)    ; optional export if you want

;; ===================== PARSER STATE =========================
(struct P (sc current) #:mutable)

;; Construct parser over a source string
(define (make-parser src)
  (define sc (make-scanner src))
  (P sc (next-token sc)))

(define (peek p)       (P-current p))
(define (tt p)         (token-type (peek p))) ; current token type
(define (advance! p)   (set-P-current! p (next-token (P-sc p))))
(define (type-is? p s) (eq? (tt p) s))

;; consume if matches; return #t when consumed else #f
(define (match! p s)
  (when (type-is? p s) (advance! p) #t))

;; expect a specific token or raise a labeled error
(define (expect! p s [msg #f])
  (unless (match! p s)
    (define t (peek p))
    (error 'parser (format "Expected ~a at ~a:~a, found ~a"
                           (or msg s) (token-line t) (token-col t) (token-type t)))))

;; ===================== ENTRY POINTS =========================
(define (parse-file path)
  ;; TODO: file->string, make-parser, parse-program, expect EOF, return AST
  )

(define (parse-program p)
  ;; program -> stmt-list EOF
  ;; TODO: (Prog (parse-stmt-list p))
  )

;; ===================== STMT LIST ============================
;; stmt-list -> stmt stmt-list | ε
(define (stmt-list-stop? p)
  ;; stop when lookahead in FOLLOW(stmt-list): END, EOF, RBRACE, ELSE
  ;; TODO
  )

(define (parse-stmt-list p)
  ;; TODO: loop until stop; accumulate list of stmts; return (reverse acc)
  )

;; ===================== STATEMENTS ===========================
;; stmt -> if-stmt | while-stmt | assign-stmt | read-stmt | print-stmt | compound-stmt
(define (parse-stmt p)
  ;; TODO: dispatch based on current token type
  )

;; compound-stmt -> BEGIN stmt-list END {; stmt}*
(define (parse-compound p)
  ;; TODO:
  ;;   BEGIN
  ;;   inner := parse-stmt-list
  ;;   END
  ;;   optionally: while next is ';' then parse more stmt and collect
  ;;   return (Block inner) or a Seq chain if extra stmts were collected
  )

;; if-stmt -> IF add comp-op add THEN BEGIN stmt-list END [ELSE BEGIN stmt-list END]
(define (parse-if p)
  ;; TODO: consume IF; lhs:=parse-add; op:=parse-comp-op; rhs:=parse-add
  ;;       THEN BEGIN tblock END
  ;;       optional ELSE BEGIN eblock END
  ;;       return (IfS lhs op rhs tblock eblock-or-#f)
  )

;; while-stmt -> WHILE add comp-op add BEGIN stmt-list END
(define (parse-while p)
  ;; TODO
  )

;; assign-stmt -> ID := expr
(define (parse-assign p)
  ;; TODO:
  ;;   remember id lexeme
  ;;   expect 'ID then 'ASSIGN then parse-expr
  ;;   return (AssignS id expr)
  )

;; read-stmt -> READ ID
(define (parse-read p)
  ;; TODO: READ then ID; return (ReadS id-lexeme)
  )

;; print-stmt -> PRINT expr
(define (parse-print p)
  ;; TODO
  )

;; ===================== EXPRESSIONS & PRECEDENCE =============
;; expr := add (comp-op add)?
(define (parse-expr p)
  ;; TODO:
  ;;   left := parse-add
  ;;   if next is comp-op: op := parse-comp-op; right := parse-add; return (BinOpE op left right)
  ;;   else: left
  )

;; add  := term { (+|-) term }*
(define (parse-add p)
  ;; TODO: loop; on PLUS/MINUS build (BinOpE 'PLUS node (parse-term p)) etc.
  )

;; term := factor { (*|/) factor }*
(define (parse-term p)
  ;; TODO
  )

;; factor := ID | NUM | '(' expr ')'
(define (parse-factor p)
  ;; TODO:
  ;;   if ID -> (advance) (IdE (token-lexeme token))
  ;;   if NUM -> (advance) (NumE (token-lexeme token))
  ;;   if LPAREN -> parse expr then expect RPAREN
  ;;   else -> error "Expected factor ..."
  )

(define (parse-comp-op p)
  ;; TODO:
  ;;   if tt in '(EQ NE LT LE GT GE) then advance and return token type
  ;;   else error
  )
