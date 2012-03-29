(in-package :cl-user)

(defun optimized ()
  (declare (optimize (speed 3) (safety 0)))
  (random 1d0))

(defun not-one ()
  (declare (optimize (speed 3) (safety 0)))
  (/ (random 2d0) 2d0))

(defun non-optimized ()
  (random 1d0))

(defun use-single-float ()
  (declare (optimize (speed 3) (safety 0)))
  (random 1f0))

(defun average-of-random (fn)
  (/ (loop repeat 10000 summing (funcall fn)) 10000))

(loop for f in '(optimized not-one non-optimized use-single-float) do
      (format t "Function name: ~A~%" f)
      (format t "Non-compiled answer: ~D~%" (average-of-random f))
      (compile f)
      (format t "Compiled answer: ~D~%" (average-of-random f))
      (format t "Disassemble:~%")
      (disassemble (symbol-function f))
      (format t "~%~% ----------------------------- ~%~%"))

#|
RAND(320): :ld random-bug
; Loading /home/roy/rand/random-bug.lisp
Function name: OPTIMIZED
Non-compiled answer: 0.4955351875975676d0
Compiled answer: 0.25039299680278637d0
Disassemble:
;; disassembly of #<Function OPTIMIZED>
;; formals: 
;; constant vector:
0: *RANDOM-STATE*
1: EXCL::FAST-RANDOM-DOUBLE-FLOAT

;; code start: #x1000ff9678:
   0: 48 83 ec 78    sub	rsp,$120
   4: 4c 89 74 24 08 movq	[rsp+8],r14
   9: 49 8b 46 36    movq	rax,[r14+54] ; *RANDOM-STATE*
  13: 4c 8b 68 15    movq	r13,[rax+21]
  17: 4c 8b 93 28 01 movq	r10,[rbx+296]
      00 00 
  24: 4d 3b 55 f6    cmpq	r10,[r13-10]
  28: 7d 05          jnl	35
  30: 4b 8b 44 15 fe movq	rax,[r13+r10-2]
  35: 4c 8b 68 ed    movq	r13,[rax-19]
  39: 49 8b fd       movq	rdi,r13
  42: 49 8b 6e 3e    movq	rbp,[r14+62] ; EXCL::FAST-RANDOM-DOUBLE-FLOAT
  46: 4c 8b 75 fd    movq	r14,[rbp-3]
  50: 49 8b 6e f6    movq	rbp,[r14-10]
  54: 48 c7 c0 01 00 movq	rax,$1
      00 00 
  61: 48 8d 6d 08    leaq	rbp,[rbp+8]
  65: 48 89 44 24 18 movq	[rsp+24],rax
  70: ff d5          call	*ebp
  72: f2 44 0f 10 e8 movsd	xmm13,xmm0
  77: f2 45 0f 10 fd movsd	xmm15,xmm13
  82: 33 c0          xorl	eax,eax
  84: 41 ff 97 d7 03 call	*[r15+983]   ; SYS::NEW-DOUBLE-FLOAT
      00 00 
  91: f8             clc
  92: 48 8d 64 24 78 leaq	rsp,[rsp+120]
  97: 4c 8b 74 24 10 movq	r14,[rsp+16]
 102: c3             ret
 103: 90             nop


 ----------------------------- 

Function name: NOT-ONE
Non-compiled answer: 0.5041140986466367d0
Compiled answer: 0.501668725881355d0
Disassemble:
;; disassembly of #<Function NOT-ONE>
;; formals: 
;; constant vector:
0: 2.0d0
1: *RANDOM-STATE*
2: EXCL::NEW-RANDOM-DOUBLE-FLOAT

;; code start: #x1000c319e8:
   0: 48 81 ec 88 00 subq	rsp,$136      ; 17
      00 00 
   7: 4c 89 74 24 08 movq	[rsp+8],r14
  12: 4d 8b 6e 36    movq	r13,[r14+54]  ; 2.0d0
  16: f2 45 0f 10 6d movsd	xmm13,[r13-10]
      f6 
  22: 49 8b 46 3e    movq	rax,[r14+62]  ; *RANDOM-STATE*
  26: 4c 8b 68 15    movq	r13,[rax+21]
  30: 4c 8b 93 28 01 movq	r10,[rbx+296]
      00 00 
  37: 4d 3b 55 f6    cmpq	r10,[r13-10]
  41: 7d 05          jnl	48
  43: 4b 8b 44 15 fe movq	rax,[r13+r10-2]
  48: 4c 8b 68 ed    movq	r13,[rax-19]
  52: f2 44 0f 11 ac movsd	[rsp+128],xmm13
      24 80 00 00 00 
  62: 49 8b 6e 46    movq	rbp,[r14+70]  ; EXCL::NEW-RANDOM-DOUBLE-FLOAT
  66: 49 8b fd       movq	rdi,r13
  69: 4c 8b 75 fd    movq	r14,[rbp-3]
  73: 49 8b 6e f6    movq	rbp,[r14-10]
  77: 48 c7 c0 01 00 movq	rax,$1
      00 00 
  84: 48 8d 6d 08    leaq	rbp,[rbp+8]
  88: 48 89 44 24 18 movq	[rsp+24],rax
  93: ff d5          call	*ebp
  95: f2 44 0f 10 e8 movsd	xmm13,xmm0
 100: f2 44 0f 10 a4 movsd	xmm12,[rsp+128]
      24 80 00 00 00 
 110: f2 45 0f 59 ec mulsd	xmm13,xmm12
 115: 4d 8b 6e 36    movq	r13,[r14+54]  ; 2.0d0
 119: f2 45 0f 10 65 movsd	xmm12,[r13-10]
      f6 
 125: f2 45 0f 5e ec divsd	xmm13,xmm12
 130: f2 45 0f 10 fd movsd	xmm15,xmm13
 135: 33 c0          xorl	eax,eax
 137: 41 ff 97 d7 03 call	*[r15+983]    ; SYS::NEW-DOUBLE-FLOAT
      00 00 
 144: f8             clc
 145: 48 8d a4 24 88 leaq	rsp,[rsp+136]
      00 00 00 
 153: 4c 8b 74 24 10 movq	r14,[rsp+16]
 158: c3             ret
 159: 90             nop


 ----------------------------- 

Function name: NON-OPTIMIZED
Non-compiled answer: 0.49497054207189933d0
Compiled answer: 0.4985955412722851d0
Disassemble:
;; disassembly of #<Function NON-OPTIMIZED>
;; formals: 
;; constant vector:
0: *RANDOM-STATE*
1: EXCL::NEW-RANDOM-DOUBLE-FLOAT

;; code start: #x1000ff96c8:
   0: 48 83 ec 78 sub	rsp,$120
   4: 4c 89 74 24 08 movq	[rsp+8],r14
   9: 48 83 f8 00 cmp	rax,$0
  13: 74 01        jz	16
  15: 06            (push es)     ; SYS::TRAP-ARGERR
  16: 41 80 7f a7 00 cmpb	[r15-89],$0       ; SYS::C_INTERRUPT-PENDING
  21: 74 01        jz	24
  23: 17            (pop ss)      ; SYS::TRAP-SIGNAL-HIT
  24: 49 8b 46 36 movq	rax,[r14+54]      ; *RANDOM-STATE*
  28: 4c 8b 68 15 movq	r13,[rax+21]
  32: 4c 8b 93 28 01 movq	r10,[rbx+296]
      00 00 
  39: 4d 3b 55 f6 cmpq	r10,[r13-10]
  43: 7d 05        jnl	50
  45: 4b 8b 44 15 fe movq	rax,[r13+r10-2]
  50: 4c 8b 68 ed movq	r13,[rax-19]
  54: 49 8b fd     movq	rdi,r13
  57: 49 8b 6e 3e movq	rbp,[r14+62]      ; EXCL::NEW-RANDOM-DOUBLE-FLOAT
  61: 4c 8b 75 fd movq	r14,[rbp-3]
  65: 49 8b 6e f6 movq	rbp,[r14-10]
  69: 48 c7 c0 01 00 movq	rax,$1
      00 00 
  76: 48 8d 6d 08 leaq	rbp,[rbp+8]
  80: 48 89 44 24 18 movq	[rsp+24],rax
  85: ff d5        call	*ebp
  87: f2 44 0f 10 e8 movsd	xmm13,xmm0
  92: f2 45 0f 10 fd movsd	xmm15,xmm13
  97: 33 c0        xorl	eax,eax
  99: 41 ff 97 d7 03 call	*[r15+983]        ; SYS::NEW-DOUBLE-FLOAT
      00 00 
 106: f8            clc
 107: 48 8d 64 24 78 leaq	rsp,[rsp+120]
 112: 4c 8b 74 24 10 movq	r14,[rsp+16]
 117: c3            ret


 ----------------------------- 

Function name: USE-SINGLE-FLOAT
Non-compiled answer: 0.49572182
Compiled answer: 0.50142384
Disassemble:
;; disassembly of #<Function USE-SINGLE-FLOAT>
;; formals: 
;; constant vector:
0: *RANDOM-STATE*
1: EXCL::NEW-RANDOM-FLOAT

;; code start: #x1000c31678:
   0: 48 83 ec 78    sub	rsp,$120
   4: 4c 89 74 24 08 movq	[rsp+8],r14
   9: 49 8b 46 36    movq	rax,[r14+54]  ; *RANDOM-STATE*
  13: 4c 8b 68 15    movq	r13,[rax+21]
  17: 4c 8b 93 28 01 movq	r10,[rbx+296]
      00 00 
  24: 4d 3b 55 f6    cmpq	r10,[r13-10]
  28: 7d 05          jnl	35
  30: 4b 8b 44 15 fe movq	rax,[r13+r10-2]
  35: 4c 8b 68 ed    movq	r13,[rax-19]
  39: 49 8b fd       movq	rdi,r13
  42: 49 8b 6e 3e    movq	rbp,[r14+62]  ; EXCL::NEW-RANDOM-FLOAT
  46: 4c 8b 75 fd    movq	r14,[rbp-3]
  50: 49 8b 6e f6    movq	rbp,[r14-10]
  54: 48 c7 c0 01 00 movq	rax,$1
      00 00 
  61: 48 8d 6d 08    leaq	rbp,[rbp+8]
  65: 48 89 44 24 18 movq	[rsp+24],rax
  70: ff d5          call	*ebp
  72: f3 44 0f 10 e8 movss	xmm13,xmm0
  77: f3 45 0f 10 fd movss	xmm15,xmm13
  82: 33 c0          xorl	eax,eax
  84: 41 ff 97 cf 03 call	*[r15+975]    ; SYS::NEW-SINGLE-FLOAT
      00 00 
  91: f8             clc
  92: 48 8d 64 24 78 leaq	rsp,[rsp+120]
  97: 4c 8b 74 24 10 movq	r14,[rsp+16]
 102: c3             ret
 103: 90             nop


 ----------------------------- 
