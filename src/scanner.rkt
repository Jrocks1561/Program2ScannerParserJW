#lang racket
;; ===================== TOKEN MODEL ==========================
(struct token (type lexeme line col) #:transparent)
(provide (struct-out token) (struct-out input-textanner) make-input-textanner make-scanner next-token)

;; ===================== input-textANNER STATE ========================
(struct input-textanner (input-text len pos line col eof?) #:mutable)

;; Factory to build a Scanner (don't shadow the struct constructor)
(define (make-input-textanner input-text)
  (input-textanner input-text (string-length input-text) 0 1 1 #f))

;; Optional alias so existing code that calls make-scanner still works
(define make-scanner make-input-textanner)

;; ===================== Basic HELPERS =========================
;; Look at current (k=0) or future char, or #\nul if past end
(define (lookahead scanner [k 0])
  (define i (+ (input-textanner-pos scanner) k))
  (if (or (>= i (input-textanner-len scanner))
          (< i 0))
      #\u0000
      (string-ref (input-textanner-input-text scanner) i)))

;; Move forward by 1, updating line/col
(define (advance scanner)
  (when (< (input-textanner-pos scanner) (input-textanner-len scanner))
    (define ch (lookahead scanner))
    (set-input-textanner-pos! scanner (add1 (input-textanner-pos scanner)))
    (if (char=? ch #\newline)
        (begin
          (set-input-textanner-line! scanner (add1 (input-textanner-line scanner)))
          (set-input-textanner-col!  scanner 1))
        (set-input-textanner-col! scanner (add1 (input-textanner-col scanner)))))
  (void))

;; True when position is at/after len
(define (at-eof? scanner)
  (>= (input-textanner-pos scanner) (input-textanner-len scanner)))

;; Does upcoming source start with string s?
(define (starts-with? scanner s)
  (define n (string-length s))
  (and (<= (+ (input-textanner-pos scanner) n) (input-textanner-len scanner))
       (string=? (substring (input-textanner-input-text scanner)
                            (input-textanner-pos scanner)
                            (+ (input-textanner-pos scanner) n))
                 s)))

;; ===================== SKIP / IGNORE ========================
;; Skip spaces and tabs (leave '\n' to be handled separately)
(define (skip-spaces-and-tabs scanner)
  ;; loop while not at EOF and current char is space or tab
  (let loop ()
    (when (and (not (input-textanner-eof? scanner))
               (or (char=? (lookahead scanner) #\space)
                   (char=? (lookahead scanner) #\tab)))
      (advance scanner)
      (loop))))

;; If "/*" present, consume until first "*/" (handle multi-line)
(define (skip-comment-if-present scanner)
  (when (and (not (at-eof? scanner))
             (char=? (lookahead scanner) #\/)
             (char=? (lookahead scanner 1) #\*))
    ;; Record starting position for error message
    (define start-line (input-textanner-line scanner))
    (define start-col (input-textanner-col scanner))
    ;; advance past opening /*
    (advance scanner) 
    (advance scanner)
    ;; Start in comment mode
    (let loop ()
      (define current-char (lookahead scanner))
      ;; If we hit EOF or null char, it's an error
      (when (or (>= (input-textanner-pos scanner) (input-textanner-len scanner))
                (char=? current-char #\u0000))
        (error (format "Invalid: Not an end to comment at ~a:~a"
                      start-line start-col)))
      
      ;; Look for closing */
      (if (and (char=? current-char #\*)
               (char=? (lookahead scanner 1) #\/))
          (begin
            (advance scanner) ; move past *
            (advance scanner)) ; move past /
          (begin
            (advance scanner)
            (loop))))))

;; If current char is '\n', consume it and emit a NEWLINE token
(define (emit-newline-if-present scanner)
  (when (and (not (input-textanner-eof? scanner))
             (char=? (lookahead scanner) #\newline))
    ;; store current line and column before advancing
    (define line (input-textanner-line scanner))
    (define col  (input-textanner-col scanner))
    (advance scanner)
    ;; return NEWLINE token
    (token 'NEWLINE "\n" line col)))

;; ===================== LEXEME SCANNERS (TODO for Step 4/5) =========
;; Identifier/keyword: alpha, then (alnum | '_' | '-')*
;; TODO: implement later
(define (input-textan-identifier-or-keyword scanner)
  (error 'todo "input-textan-identifier-or-keyword not implemented yet"))

;; Number per grammar:
;;   nonzero digit* | nonzero digit* '.' digit digit*
;; (no leading 0; if '.', require >=2 digits)
;; TODO: implement later
(define (input-textan-number scanner)
  (error 'todo "input-textan-number not implemented yet"))

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
(define (next-token scanner)
  ;; 0) if eof? flag set, always return EOF
  (when (input-textanner-eof? scanner)
    (return (token 'EOF "" (input-textanner-line scanner) (input-textanner-col scanner))))

  ;; 1) Repeatedly skip spaces/tabs and comments (comments can appear back-to-back)
  (let outer-loop ([last-pos -1])
    (skip-spaces-and-tabs scanner)
    (if (at-eof? scanner)
        (void) ; done processing
        (let ([current-pos (input-textanner-pos scanner)])
          ;; Check if we're making progress
          (when (= current-pos last-pos)
            (error "Scanner stuck - not advancing through input"))
          
          (cond
            ;; Check for unmatched closing comment
            [(and (char=? (lookahead scanner) #\*)
                  (char=? (lookahead scanner 1) #\/))
             (error (format "Invalid: Unmatched end of comment */ at ~a:~a"
                          (input-textanner-line scanner)
                          (input-textanner-col scanner)))]
            
            ;; Check for start of any comment
            [(and (char=? (lookahead scanner) #\/)
                  (char=? (lookahead scanner 1) #\*))
             (let ([comment-start-line (input-textanner-line scanner)]
                   [comment-start-col (input-textanner-col scanner)])
               ;; advance past opening /*
               (advance scanner) 
               (advance scanner)
               
               (let scan-comment ()
                 (cond
                   ;; EOF before comment end
                   [(>= (input-textanner-pos scanner) (input-textanner-len scanner))
                    (error (format "Invalid: Not an end to comment at ~a:~a"
                                 comment-start-line comment-start-col))]
                   
                   ;; Found */
                   [(and (char=? (lookahead scanner) #\*)
                         (char=? (lookahead scanner 1) #\/))
                    (advance scanner)
                    (advance scanner)
                    (outer-loop current-pos)] ; Look for more comments
                   
                   ;; Keep scanning - any /* inside is just part of comment
                   [else
                    (advance scanner)
                    (scan-comment)])))]
            
            ;; No special tokens, advance and continue
            [else
             (advance scanner)
             (outer-loop current-pos)])))) ; Fixed brackets/parens

  ;; 2) If newline present, emit NEWLINE
  (let ([newline-token (emit-newline-if-present scanner)])
    (when newline-token (return newline-token)))

  ;; 3) If at actual end, set eof? and return EOF
  (when (at-eof? scanner)
    (set-input-textanner-eof?! scanner #t)
    (return (token 'EOF "" (input-textanner-line scanner) (input-textanner-col scanner))))

  ;; (Step 3 will add real tokens here)
  (set-input-textanner-eof?! scanner #t)
  (token 'EOF "" (input-textanner-line scanner) (input-textanner-col scanner)))

;; tiny `return` helper
(define-syntax-rule (return v) v)
