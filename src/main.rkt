#lang racket
(require "scanner.rkt"         ; same folder
         racket/runtime-path)

(define-runtime-path HERE ".")
(define (rel . parts) (apply build-path HERE parts))

(define (pp-token t)
  (printf "~a(~a) @ ~a:~a\n"
          (token-type t) (token-lexeme t) (token-line t) (token-col t)))

(define (process-one path)
  (printf "==> ~a\n" path)
  (define src (file->string path))
  (define sc (make-scanner src))
  (let loop ([t (next-token sc)])
    (pp-token t)
    (unless (eq? (token-type t) 'EOF)
      (loop (next-token sc)))))

(module+ main
  ;; run a single test file:
  (process-one (rel ".." "tests" "valid" "SkipComment01.txt"))

  #; ;; uncomment to scan all .txt under tests/{valid,invalid}
  (for ([dir (list (rel ".." "tests" "valid")
                   (rel ".." "tests" "invalid"))]
        #:when (directory-exists? dir))
    (for ([p (in-directory dir)]
          #:when (regexp-match? #px"\\.txt$" (path->string p)))
      (process-one p))))