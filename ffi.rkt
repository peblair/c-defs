#lang racket/base
(require racket/string
         racket/match
         racket/contract
         racket/port
         racket/system
         racket/path
         racket/file)
(provide c-constant-src)

;; Contains definitions for compiling,
;; running, and collecting output

(define DEBUG #f)
(define (displayln/dbg . args)
  (when DEBUG
    (apply displayln args)))

(define (find-windows-compiler)
  (let ((gcc (find-executable-path "gcc"))
        (clang (find-executable-path "clang"))
        (cl  (find-executable-path "cl")))
    (or (C-COMPILER/override)
        clang
        gcc
        cl
        (error "No C compiler found.\nPlease install gcc, clang, or Visual C++"))))

(define (find-*nix-compiler)
  (let ((clang (find-executable-path "clang"))
        (gcc (find-executable-path "gcc"))
        (cc (find-executable-path "cc")))
    (or (C-COMPILER/override)
        clang
        gcc
        cc
        (error "No C compiler found.\nPlease install clang or gcc"))))

(define C-COMPILER/override (make-parameter #f))
(define (C-COMPILER)
  (match (system-type)
    ['windows (find-windows-compiler)]
    [else     (find-*nix-compiler   )]))

(define (compile-c file (outname #f))
  (let* ((outstr (if outname (format " -o ~a" outname) ""))
         (command (string-append (path->string (C-COMPILER))
                                 " "
                                 (path->string file) outstr)))
    (displayln/dbg command)
    (parameterize ((current-input-port (open-input-string "")))
      (with-output-to-string
       (λ()(system command))))))

(define/contract (get-output file)
  (path? . -> . string?)
  (let ((command (path->string file)))
    (parameterize ((current-input-port (open-input-string "")))
      (with-output-to-string
       (λ()(system command))))))

;; e.g. (print-vals "%d" "FOO" "BAR") prints
;; definitions of FOO and BAR (which are ints)
;; (this function would return
;;  "printf(\"%d\\n%d", FOO, BAR);" in that case)
(define (val-str printf-val . vals)
  (define printfstr
    (let ((lst (map (λ _ printf-val) vals)))
      (string-join lst "\\n")))
  (define printfvals
    (string-join (map (λ(v)(format "~a" v)) vals) ", "))
  (format "printf(\"~a\", ~a);" printfstr printfvals))

(define (make-includes files)
  (string-join (map (λ(f)(format "#include \"~a\"" f)) files) "\n"))

(define (make-main print-stmt)
  (format "int main(void){ ~a }" print-stmt))

(define (make-and-run-file includes print-stmt)
  (define file (make-temporary-file "cconsttmp-~a.c" #f #f))
  (define fileport 
    (open-output-file file  #:exists 'replace))
  (define filename (file-name-from-path file))
  (displayln/dbg fileport)
  (displayln/dbg filename)
  (displayln/dbg includes)
  (displayln/dbg (make-main print-stmt))
  
  (displayln "#include <stdio.h>" fileport)
  (displayln includes fileport)
  (displayln (make-main print-stmt) fileport)
  (close-output-port fileport)
  (define compiled-path (path-replace-suffix file ".o"))
  (define compiled-name (file-name-from-path compiled-path))
  (displayln/dbg (compile-c file (path->string compiled-path)))
  (begin0 (string-split (get-output compiled-path) "\n")
          ;; Cleanup
          (delete-file file)
          (delete-file compiled-path)))

(define (maybe-converter formatstr)
  (define (matches-spec? spec)
    (let ((rx (regexp
               (string-append 
                "^%[-+ #0]?(?:[*]|[0-9]*)(?:(\\.\\*)|(\\.[0-9]+))?" 
                spec "$"))))
      (regexp-match? rx formatstr)))
  (cond [(matches-spec? "[diufFeEgG]") string->number]
        [(matches-spec? "o") (λ(s)(string->number s 8))]
        [(matches-spec? "[xXaA]") (λ(s)(string->number s 16))]
        [(matches-spec? "c") (λ(s)(car(string->list s)))]
        [else values]))

(define/contract (c-constant-src #:compiler (comp #f) . headers)
  (->* () (#:compiler (or/c #f path?)) #:rest (listof string?)
       (->* (string?) #:rest (listof string?) any))
  (parameterize ((C-COMPILER/override comp))
    (λ (fmt . args)
      (apply values (map (maybe-converter fmt)
                         (make-and-run-file (make-includes headers)
                                            (apply val-str fmt args)))))))
                       
  