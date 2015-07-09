#lang scribble/manual
@(require scribble/racket
          racket/sandbox
          racket/system
          scribble/eval
          c-defs)
@declare-exporting[c-defs]

@(define evaluator
   (let ((rootpath (if (equal? (system-type) 'windows) "C:\\" "/")))
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-path-permissions (list* (list 'execute rootpath)
                                                     (sandbox-path-permissions))])
       (make-evaluator 'racket/base))))
@title{C Constants}

@section{Introduction}

This is a simple package for fetching @tt{#define}'d values
from C files. It simply creates a temporary file with the
appropriate @tt{#include}s, throws a @tt{printf} call into 
@tt{main}, compiles and runs. The only dependency as of now
is a working C compiler.

@section{Usage}

Usage of this package revolves around one function:

@defproc[(c-defs [header string?] ...
                 [#:compiler compiler (or/c path? #f) #f])
         ((string?) #:rest (listof string?) . ->* . (values any/c ...))
         ]{
           Creates a function which can be used to access C constants.
           
           The @racket[header] inputs are a sequence of strings which
           will be wrapped in @tt{#include} statements at the top of the
           C file (note that these statements will be in the same order
           as the provided @racket[header ...] sequence), in the same
           way that one would write them in C (e.g. @racket{escheme.h})
           
           The optional @racket[compiler] argument is a path to the C
           compiler to use when creating the temporary file. If omitted,
           the package will attempt to locate a C compiler on the system's
           @tt{PATH}.
           
           The resulting function takes a series of strings as input.
           
           The first string corresponds to a C @tt{printf} format string
           which will be used to print @bold{all} constants in the 
           @bold{current} function call. The remaining strings are the 
           names of the constants to resolve.
           
           The return value of this function is a @racket{values} expression
           which contains the values of the desired constants. The formatting
           string is parsed by the package, so the return values are of the
           appropriate type as well.
           
           @examples[#:eval evaluator
                            (require c-defs)
                            ;; Fetches some magic numbers from
                            ;; the Python 3.4 C headers
                            (define PYTHON-CONSTANTS (c-defs "python3.4/Python.h" 
                                                             "python3.4/structmember.h"))
                            
                            (define-values (T-SHORT T-OBJ) 
                              (PYTHON-CONSTANTS "%d" 
                                                "T_SHORT" "T_OBJECT"))
                            T-OBJ]
           }