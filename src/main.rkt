#lang racket
(require "scanner.rkt" "parser.rkt")

;; Flip as you go
(define SHOW-TOKENS? #t)
(define RUN-PARSER?  #f)

;; Pretty-print tokens for scanner testing
(define (pp-token t)
  (printf "~a(~a) @ ~a:~a\n"
          (token-type t) (token-lexeme t) (token-line t) (token-col t)))

;; Run one file: optionally token-dump, optionally parse
(define (process-one path)
  (printf "==> %s\n" path)
  (define src (file->string path))
  (with-handlers ([exn:fail? (λ(e) (printf "ERROR: ~a\n" (exn-message e)))])
    (when SHOW-TOKENS?
      (define sc (make-scanner src))
      (let loop ([t (next-token sc)])
        (pp-token t)
        (unless (eq? (token-type t) 'EOF) (loop (next-token sc)))))
    (when RUN-PARSER?
      (void (parse-file path))
      (printf "OK (parsed)\n"))))

(module+ main
  ;; Start with one known-good file, then switch to batch mode
  (process-one "tests/valid/Correct01.txt")

  #; ; when ready:
  (for ([dir '("tests/valid" "tests/invalid")] #:when (directory-exists? dir))
    (for ([p (in-directory dir)])
      (when (regexp-match? #px"\\.txt$" (path->string p))
        (process-one (path->string p))))))
