#lang info
(define collection "c-defs")
(define compile-omit-paths (list "examples"))
(define version "1.0.1")
(define scribblings '(("docs/c-defs.scrbl")))
(define deps '("base"))
(define build-deps '("sandbox-lib" "scribble-lib"))