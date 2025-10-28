#lang racket

;; ===================== AST NODES ============================
;; add more only when needed

;; Program / blocks
(struct Prog   (stmts)        #:transparent) ; list of stmt
(struct Block  (stmts)        #:transparent)
(struct Seq    (first rest)   #:transparent) ; optional chain form

;; Statements
(struct IfS    (lhs op rhs then else) #:transparent) ; else may be #f
(struct WhileS (lhs op rhs body)      #:transparent)
(struct AssignS(id expr)              #:transparent)
(struct ReadS  (id)                   #:transparent)
(struct PrintS (expr)                 #:transparent)

;; Expressions
(struct BinOpE (op left right)        #:transparent) ; '+ '- '* '/ 'EQ 'NE 'LT 'LE 'GT 'GE
(struct IdE    (name)                 #:transparent)
(struct NumE   (lexeme)               #:transparent)

;; ===================== HELPERS ==============================
(define COMP-OPS '(EQ NE LT LE GT GE))
(define (comp-op? s) (member s COMP-OPS))

(provide
  Prog Block Seq
  IfS WhileS AssignS ReadS PrintS
  BinOpE IdE NumE
  COMP-OPS comp-op?)
