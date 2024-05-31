; SLIME 2.24
CL-USER> (hello-world)
; in: HELLO-WORLD
;     (HELLO-WORLD)
; 
; caught STYLE-WARNING:
;   undefined function: COMMON-LISP-USER::HELLO-WORLD
; 
; compilation unit finished
;   Undefined function:
;     HELLO-WORLD
;   caught 1 STYLE-WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION HELLO-WORLD {7002BFF343}>.
CL-USER> (load "hello.lisp")
; Evaluation aborted on #<SB-INT:SIMPLE-FILE-ERROR "~@<Couldn't load ~S: file does not exist.~@:>" {700321E213}>.
CL-USER> (load "hello.lisp")
T
CL-USER> (hello-world)
hello, world
NIL
CL-USER> (load (compile-file "hello.lisp"))
; compiling file "/Applications/portacle/hello.lisp" (written 08 FEB 2024 11:24:11 PM):
; compiling (DEFUN HELLO-WORLD ...)

; wrote /Applications/portacle/hello.fasl
; compilation finished in 0:00:00.014
T
CL-USER> 