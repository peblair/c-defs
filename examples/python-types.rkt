#lang racket
(require "../ffi.rkt")

(define PYTHON-CONSTANTS (c-constant-src 
                          "python3.4/Python.h" 
                          "python3.4/structmember.h"))

(define-values (T-SHORT T-OBJ) 
  (PYTHON-CONSTANTS "%d" 
                    "T_SHORT" "T_OBJECT"))