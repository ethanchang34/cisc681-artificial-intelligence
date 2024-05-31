; SLIME 2.24
CL-USER> 10
10
CL-USER> (+ 2 3)
5
CL-USER> "hello, world"
"hello, world"
CL-USER> (format t "hello, world")
hello, world
NIL
CL-USER> (defun hello-world () (format t "hello, world"))
HELLO-WORLD
CL-USER> (hello-world)
hello, world
NIL
; compiling (DEFUN HELLO-WORLD ...)
WARNING: redefining COMMON-LISP-USER::HELLO-WORLD in DEFUN

CL-USER> (hello-world)
hello, world
NIL
CL-USER> 