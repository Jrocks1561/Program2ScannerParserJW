#lang racket
;;ChatGPT created an outline for a scanner helped with organization most of these comments will be reomoved in this push
;;SOME comments were modified for clairty and also some deleted

;;I finished debugging the scanner and i believe it is now fully functional

;;trasnsparent allows easier debugging can remove later
;;Token struct object
(struct token (type lexeme line col) #:transparent)


;; NOTE: added last-tt to support - numbers as copilot suggested.. (talk about in video)
;;wasnt sure if it was required but i see - nums in some of the correct.txt files that it is needed 
(struct input-textanner (input-text len pos line col eof? last-tt) #:mutable)

;;tracker
(define (make-input-textanner input-text)
  (input-textanner input-text (string-length input-text) 0 1 1 #f 'BOF))

;; --- emit that updates last-tt
;;how scanner tells what the last token typpe was NOTE: important for - numbers
(define (emit scanner type lexeme line col)
  (set-input-textanner-last-tt! scanner type)
  (token type lexeme line col))

;; shorten name 
(define make-scanner make-input-textanner)

;; exports what we are allowed to use in other files
(provide (struct-out token)
         (struct-out input-textanner)
         make-input-textanner
         make-scanner
         next-token)


;; ===================== BASIC HELPERS ========================

;; Look at current (k=0) or future char essentiallt the 1 lookahead as required
;; #u\0000 == null char -- Thank you copilot
(define (lookahead scanner [k 0])
  (define i (+ (input-textanner-pos scanner) k))
  (if (or (>= i (input-textanner-len scanner)) (< i 0))
      #\u0000
      (string-ref (input-textanner-input-text scanner) i)))

;; Move forward by 1 updating line/col
(define (advance scanner)
  (when (< (input-textanner-pos scanner) (input-textanner-len scanner))
    (define ch (lookahead scanner))
    (set-input-textanner-pos! scanner (add1 (input-textanner-pos scanner)))
    (if (char=? ch #\newline)
        (begin
          (set-input-textanner-line! scanner (add1 (input-textanner-line scanner)))
          (set-input-textanner-col!  scanner 1))
        (set-input-textanner-col! scanner (add1 (input-textanner-col scanner))))))


;; ensures not at EOF
(define (at-eof? scanner)
  (>= (input-textanner-pos scanner) (input-textanner-len scanner)))


;; ===================== SKIP / IGNORE ========================

;; Skip whitespace except NEWLINE (so parser can see line breaks)
(define (skip-spaces-and-tabs scanner)
  (let loop ()
    (define c (lookahead scanner))
    (when (and (not (char=? c #\u0000))
               (char-whitespace? c)
               (not (char=? c #\newline)))
      (advance scanner)
      (loop))))

;; If "/*" present, consume until first "*/" (non-nested, may span lines)
;;notes for pantropto video -neededed to include logic that ignored a /* in a commnet /* 
(define (skip-comment-if-present scanner)
  (when (and (not (at-eof? scanner))
             (char=? (lookahead scanner) #\/)
             (char=? (lookahead scanner 1) #\*))
    (define start-line (input-textanner-line scanner))
    (define start-col  (input-textanner-col scanner))
    ;; past opening /*
    (advance scanner) (advance scanner)
    (let loop ()
      (cond
        [(at-eof? scanner)
         (error 'scanner
                (format "ERROR: comment started at ~a:~a and not finished before EOF"
                        start-line start-col))]
        [(char=? (lookahead scanner) #\*)
         (advance scanner)
         (if (char=? (lookahead scanner) #\/)
             (advance scanner)     
             (loop))]                 
        [else
         (advance scanner)
         (loop)]))))

;; Repeatedly skip spaces/tabs and block comments (never eats NEWLINE)
(define (skip-ignorable! scanner)
  (let loop ()
    (define before (input-textanner-pos scanner))
    (skip-spaces-and-tabs scanner)
    (when (and (not (at-eof? scanner))
               (char=? (lookahead scanner) #\/)
               (char=? (lookahead scanner 1) #\*))
      (skip-comment-if-present scanner))
    (define after (input-textanner-pos scanner))
    (when (< before after) (loop))))

;; If current char is '\n' or '\r', consume it and emit a NEWLINE token
;; Returns a token when newline consumed returns #f otherwise 
;;This function also checks for line breaks mid statement was very confusing copilot stered me here
;;bringing a whole new amount of bugs to fix
(define (emit-newline-if-present scanner)
  (if (and (not (input-textanner-eof? scanner))
           (or (char=? (lookahead scanner) #\newline)
               (char=? (lookahead scanner) #\return)))
      (let ([line (input-textanner-line scanner)]
            [col  (input-textanner-col scanner)]
            [last (input-textanner-last-tt scanner)])
        ;; If a newline appears right after something that
        ;; cannot legally end a statement, flag it.
        (when (memq last '(ID NUM PLUS MINUS STAR SLASH ASSIGN EQ NE LT LE GT GE LPAREN))
          (printf "ERROR: line break mid statement at ~a:~a\n" line col))
        (advance scanner)
        (emit scanner 'NEWLINE "\n" line col))
      #f))


;; ===================== TABLES / PREDICATES ==================

;;A large portion of this code was outlines for me by ChatGPT
;;required helper functions for scanning tokens
(define KEYWORDS
  (hash "if" 'IF "then" 'THEN "else" 'ELSE
        "while" 'WHILE "begin" 'BEGIN "end" 'END
        "read" 'READ "print" 'PRINT))

(define (alpha? ch)   (regexp-match? #px"[A-Za-z]" (string ch)))
(define (alnum? ch)   (regexp-match? #px"[A-Za-z0-9]" (string ch)))
(define (digit? ch)   (regexp-match? #px"[0-9]" (string ch)))
(define (nonzero? ch) (regexp-match? #px"[1-9]" (string ch)))

;; Where a neg number will be one token with - in front.
(define (num-may-start-after? last-tt)
  (memq last-tt
        '(BOF NEWLINE LPAREN ASSIGN EQ NE LT LE GT GE THEN BEGIN READ PRINT SEMI
          PLUS MINUS STAR SLASH)))

;; ===================== LEXEME SCANNERS ======================


;; Identifier/keyword: alpha, then (alnum | '_' | '-')*
;;as Required by assignment
(define (scan-id-or-kw scanner)
  (define ln (input-textanner-line scanner))
  (define cl (input-textanner-col scanner))
  (define buf (open-output-string))
  (define (push! ch) (write-char ch buf))

  (define ch0 (lookahead scanner))
  (unless (alpha? ch0)
    (error 'scanner (format "ERROR: Invalid identifier start at ~a:~a" ln cl)))
  (push! ch0) (advance scanner)

  (let loop ()
    (define c (lookahead scanner))
    (when (and (not (char=? c #\u0000))
               (or (alnum? c) (char=? c #\_) (char=? c #\-)))
      (push! c) (advance scanner) (loop)))

  (define lex (get-output-string buf))
  (define kw (hash-ref KEYWORDS lex #f))
  (emit scanner (or kw 'ID) lex ln cl))

;; num -> sign? int | sign? int '.' digit+
;; int: '0' | nonzero digit*
;; sign -> '+' | '-' | ε
(define (scan-number scanner #:has-sign? [has-sign? #f])
  (define ln (input-textanner-line scanner))
  (define cl (input-textanner-col scanner))
  (define buf (open-output-string))
  (define (push! ch) (write-char ch buf))

  ;; optional contextual sign (expects sign still at current position)
  (when has-sign?
    (define s (lookahead scanner))
    (when (or (char=? s #\+) (char=? s #\-))
      (push! s) (advance scanner)))

  ;; ------- integer part -------
  (define started-with-zero? #f)
  (define first (lookahead scanner))
  (cond
    [(char=? first #\0)
     (set! started-with-zero? #t)
     (push! first) (advance scanner)
     ;; not allowd multi-digit integers starting with 0
     (when (digit? (lookahead scanner))
       (error 'scanner
              (format "ERROR: Leading zeros not allowed at ~a:~a" ln cl)))]
    [(nonzero? first)
     (push! first) (advance scanner)
     (let loop ()
       (define d (lookahead scanner))
       (when (digit? d)
         (push! d) (advance scanner) (loop)))]
   ;;generic 
    [else
     (error 'scanner (format "ERROR: Invalid number start at ~a:~a" ln cl))])

  ;; Reject 0x / 0b / 0o after a leading 0 (hex, binary, octal)
  ;;Ai only chnage was the ERROR message
  (when (and started-with-zero?
             (memq (lookahead scanner) '(#\x #\X #\b #\B #\o #\O)))
    (error 'scanner
           (format "ERROR: Hexadecimal, binary, or octal notation not supported at ~a:~a" ln cl)))

  ;; ------- fractional part (floating point) -------
  (when (char=? (lookahead scanner) #\.)
    (push! #\.) (advance scanner)
    (unless (digit? (lookahead scanner))
      (error 'scanner (format "ERROR: Fractional part requires digit after '.' at ~a:~a" ln cl)))
    (let loop ()
      (define d (lookahead scanner))
      (when (digit? d)
        (push! d) (advance scanner) (loop))))

  ;; ------- reject scientific notation -------
  (when (or (char=? (lookahead scanner) #\e)
            (char=? (lookahead scanner) #\E))
    ;; If it looks like an exponent (e/E then optional sign then digit), reject
    (define s1 (lookahead scanner 1))
    (define d0 (if (or (char=? s1 #\+) (char=? s1 #\-))
                   (lookahead scanner 2)
                   s1))
    (when (digit? d0)
      (error 'scanner
             (format "ERROR: Scientific notation not allowed at ~a:~a" ln cl))))

  (emit scanner 'NUM (get-output-string buf) ln cl))



;; ===================== MAIN API: next-token =================


(define (next-token scanner)
  (let/ec return
    ;; if eof always return EOF
    (when (input-textanner-eof? scanner)
      (return (token 'EOF "" (input-textanner-line scanner) (input-textanner-col scanner))))

    ;;Skip ignorable (spaces/tabs and /*…*/), but leave NEWLINE for parser rules
    ;;**note to self might need ot update logic here for parser
    (skip-ignorable! scanner)

    ;; Unmatched closing comment check 
    (when (and (not (at-eof? scanner))
           (char=? (lookahead scanner) #\*)
           (char=? (lookahead scanner 1) #\/))
  (error 'scanner
         (format "ERROR: Unmatched end of comment */ at ~a:~a"
                 (input-textanner-line scanner)
                 (input-textanner-col scanner))))

    

    ;; EOF after skipping? return EOF and set flag
    (when (at-eof? scanner)
      (set-input-textanner-eof?! scanner #t)
      (return (token 'EOF "" (input-textanner-line scanner) (input-textanner-col scanner))))

    ;;Emit NEWLINE token if present, and return it immediately
    (define nl (emit-newline-if-present scanner))
    (when nl (return nl))

    ;; Dispatch by next characters
    (define ln (input-textanner-line scanner))
    (define cl (input-textanner-col scanner))
    (define ch (lookahead scanner))

    (cond
      ;; Two-char operators first
      ;;**busy work after you write one copilot cna make the rest note in overiew video
      [(and (char=? ch #\:) (char=? (lookahead scanner 1) #\=))
       (advance scanner) (advance scanner)
       (emit scanner 'ASSIGN ":=" ln cl)]

      [(and (char=? ch #\<) (char=? (lookahead scanner 1) #\=))
       (advance scanner) (advance scanner)
       (emit scanner 'LE "<=" ln cl)]

      [(and (char=? ch #\>) (char=? (lookahead scanner 1) #\=))
       (advance scanner) (advance scanner)
       (emit scanner 'GE ">=" ln cl)]

      [(and (char=? ch #\<) (char=? (lookahead scanner 1) #\>))
       (advance scanner) (advance scanner)
       (emit scanner 'NE "<>" ln cl)]

      ;; Single-char operators / punctuation (with contextual signed number)
      [(char=? ch #\=) (advance scanner) (emit scanner 'EQ "=" ln cl)]
      [(char=? ch #\<) (advance scanner) (emit scanner 'LT "<" ln cl)]
      [(char=? ch #\>) (advance scanner) (emit scanner 'GT ">" ln cl)]

       [(char=? ch #\-)
       (if (and (digit? (lookahead scanner 1))
                (num-may-start-after? (input-textanner-last-tt scanner)))
           (scan-number scanner #:has-sign? #t)
           (begin (advance scanner) (emit scanner 'MINUS "-" ln cl)))]


      ;;dont know if we will ever need this but since we need the - for signed numbers i added it
      [(char=? ch #\+)
       (if (and (digit? (lookahead scanner 1))
                (num-may-start-after? (input-textanner-last-tt scanner)))
           ;; Do not advance; scan-number will consume the sign.
           (scan-number scanner #:has-sign? #t)
           (begin (advance scanner) (emit scanner 'PLUS "+" ln cl)))]
           

      [(char=? ch #\*) (advance scanner) (emit scanner 'STAR "*" ln cl)]
      [(char=? ch #\/) (advance scanner) (emit scanner 'SLASH "/" ln cl)]
      [(char=? ch #\() (advance scanner) (emit scanner 'LPAREN "(" ln cl)]
      [(char=? ch #\)) (advance scanner) (emit scanner 'RPAREN ")" ln cl)]
      [(char=? ch #\;) (advance scanner) (emit scanner 'SEMI ";" ln cl)]

      ;; Identifiers / keywords
      [(alpha? ch)
       (scan-id-or-kw scanner)]

      ;; Illegal number start: '.' not preceded by a digit
      [(char=? ch #\.)
        (error 'scanner
         (format "ERROR: Number must start with a digit before '.' at ~a:~a"
                (input-textanner-line scanner)
                (input-textanner-col scanner)))]


      ;; Numbers (unsigned)
      [(digit? ch)
       (scan-number scanner)]

      ;; Anything else is illegal, but treat #\u0000 as EOF
      [else
       (if (char=? ch #\u0000)
           (token 'EOF "" ln cl)
           (error 'scanner (format "Illegal character '~a' at ~a:~a" ch ln cl)))])))
