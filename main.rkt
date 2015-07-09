#lang racket/base
;; For now, this just re-exports the c-constant-src function
(require "ffi.rkt")
(provide (rename-out (c-constant-src c-defs)))