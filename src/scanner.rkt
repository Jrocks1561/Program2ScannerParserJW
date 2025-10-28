#lang racket

;; ===================== TOKEN MODEL ==========================
(struct token (type lexeme line col) #:transparent)
;; type  : symbol (e.g., 'ID 'NUM 'IF 'PLUS 'EOF ...)
;; lexeme: original source slice
;; line/col: start position (1-based)

;; Export constructor + accessors + API
(provide (struct-out token) make-scanner next-token)

;; ===================== SCANNER STATE ========================
(struct scanner (src len pos line col eof?) #:mutable)

;; Make a new scanner over a whole file/string
(define (make-scanner src)
  ;; TODO: initialize fields appropriately
  (scanner src (string-length src) 0 1 1 #f))

;; ===================== CHAR HELPERS =========================
;; Look at current (k=0) or future char, or #\nul if past end
(define (peek sc [k 0])
  ;; TODO
  )

;; Move forward by 1, updating line/col
(define (advance sc)
  ;; TODO: bump pos; if char was '\n', inc line & reset col, else inc col
  )

;; True when position is at/after len
(define (at-eof? sc)
  ;; TODO
  )

;; Does upcoming source start with string s?
(define (starts-with? sc s)
  ;; TODO
  )

;; ===================== SKIP / IGNORE ========================
;; Skip spaces and tabs (leave '\n' to be handled separately)
(define (skip-spaces-and-tabs sc)
  ;; TODO: while ' ' or '\t', advance
  )

;; If "/*" present, consume until first "*/" (handle multi-line)
(define (skip-comment-if-present sc)
  ;; TODO:
  ;;   if starts-with? "/*" then advance twice
  ;;   loop: error if EOF before "*/"; stop when sees "*/"
  )

;; If current char is '\n', consume it and emit a NEWLINE token
(define (emit-newline-if-present sc)
  ;; TODO: if newline, advance once and return (token 'NEWLINE "\n" line col)
  ;; else return #f
  )

;; ===================== LEXEME SCANNERS ======================
;; Identifier/keyword: alpha, then (alnum | '_' | '-')*
(define (scan-identifier-or-keyword sc)
  ;; TODO:
  ;;   remember start line/col & start index
  ;;   consume first alpha (assume caller checked)
  ;;   loop remaining allowed chars
  ;;   slice lexeme; if in KEYWORDS => keyword type else 'ID
  )

;; Number per your grammar:
;;   nonzero digit* | nonzero digit* '.' digit digit*
;; (no leading 0; if '.', require >=2 digits)
(define (scan-number sc)
  ;; TODO:
  ;;   remember start line/col & start index
  ;;   require first char 1-9
  ;;   consume more digits
  ;;   if '.', consume it and then require at least two digits; consume any more
  ;;   return (token 'NUM lexeme line col)
  )

;; ===================== TABLES / PREDICATES ==================
(define KEYWORDS
  (hash "if" 'IF "then" 'THEN "else" 'ELSE
        "while" 'WHILE "begin" 'BEGIN "end" 'END
        "read" 'READ "print" 'PRINT))

(define (alpha? ch)  (regexp-match? #px"[A-Za-z]" (string ch)))
(define (alnum? ch)  (regexp-match? #px"[A-Za-z0-9]" (string ch)))
(define (digit? ch)  (regexp-match? #px"[0-9]" (string ch)))
(define (nonzero? ch)(regexp-match? #px"[1-9]" (string ch)))

;; ===================== MAIN API: next-token =================
(define (next-token sc)
  ;; 0) if eof? flag set, always return EOF
  (when (scanner-eof? sc)
    (return (token 'EOF "" (scanner-line sc) (scanner-col sc))))

  ;; 1) Repeatedly skip spaces/tabs and comments
  ;;    (comments can appear back-to-back)
  ;; TODO: loop calling skip-spaces-and-tabs and skip-comment-if-present

  ;; 2) If newline present, emit NEWLINE (helps if you want it)
  ;; TODO: call emit-newline-if-present; if token returned, return it

  ;; 3) If at actual end, set eof? and return EOF
  (when (at-eof? sc)
    (set-scanner-eof?! sc #t)
    (return (token 'EOF "" (scanner-line sc) (scanner-col sc))))

  ;; 4) Decide token at (line,col)
  (define ln  (scanner-line sc))
  (define col (scanner-col sc))

  ;; 4a) Multi-char ops: <= >= <> :=
  (cond
    ;; TODO: check starts-with? for each, advance twice, emit token
    )

  ;; 4b) Single-char ops/punct: + - * / ( ) { } ; = < >
  (define ch (peek sc))
  (cond
    ;; TODO: match each char, advance, emit token
    )

  ;; 4c) Identifier/keyword
  (when (alpha? ch)
    (return (scan-identifier-or-keyword sc)))

  ;; 4d) Number (no leading 0)
  (when (nonzero? ch)
    (return (scan-number sc)))

  ;; 4e) Leading '0' should be an error under your grammar
  (when (char=? ch #\0)
    (error 'scanner (format "Leading 0 not allowed at ~a:~a" ln col)))

  ;; 4f) Otherwise unexpected character
  (error 'scanner (format "Unexpected character '~a' at ~a:~a" ch ln col)))

;; tiny `return` helper
(define-syntax-rule (return v) v)
