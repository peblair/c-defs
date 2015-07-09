# C Constants

## 1. Introduction

This is a simple package for fetching `#define`’d values from C files.
It simply creates a temporary file with the appropriate `#include`s,
throws a `printf` call into `main`, compiles and runs. The only
dependency as of now is a working C compiler.

## 2. Usage

Usage of this package revolves around one function:

```racket
(c-defs header ... [#:compiler compiler])                         
 -> ((string?) #:rest (listof string?) . ->* . (values any/c ...))
  header : string?                                                
  compiler : (or/c path? #f) = #f                                 
```

Creates a function which can be used to access C constants.

The `header` inputs are a sequence of strings which will be wrapped in
`#include` statements at the top of the C file (note that these
statements will be in the same order as the provided `header ...`
sequence), in the same way that one would write them in C (e.g.
`"escheme.h"`)

The optional `compiler` argument is a path to the C compiler to use when
creating the temporary file. If omitted, the package will attempt to
locate a C compiler on the system’s `PATH`.

The resulting function takes a series of strings as input.

The first string corresponds to a C `printf` format string which will be
used to print **all** constants in the **current** function call. The
remaining strings are the names of the constants to resolve.

The return value of this function is a `"values"` expression which
contains the values of the desired constants. The formatting string is
parsed by the package, so the return values are of the appropriate type
as well.

```racket
Examples:                                                      
> (require c-defs)                                             
> (define PYTHON-CONSTANTS (c-defs "python3.4/Python.h"        
                                   "python3.4/structmember.h"))
> (define-values (T-SHORT T-OBJ)                               
    (PYTHON-CONSTANTS "%d"                                     
                      "T_SHORT" "T_OBJECT"))                   
> T-OBJ                                                        
6                                                              
```
