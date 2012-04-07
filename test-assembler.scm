;; very simple test suite

(load "assembler.scm")
(format #t "running tests")

(define-syntax test
  (syntax-rules ()
    ((test assembly hex)
     (if (assert (equal? (assemble 'assembly) hex))
	 (format #t ".")))))

(define-syntax test-one
  (syntax-rules ()
    ((test-one assembly hex ...)
     (if (assert (equal? (assemble (list 'assembly)) (list hex ...)))
	 (format #t ".")
	 (format #t "error in ~a" assembly)))))

;; notch's test program
(test ((set a #x30)
       (set (ref #x1000) #x20)
       (sub a (ref #x1000))
       (ifn a #x10)
       (set pc crash) ;; this is assembled to a single word, instead of notch's (inefficient) two words
       (set i 10)
       (set a #x2000)
       loop
       (set (ref (+ i #x2000)) (ref a))
       (sub i 1)
       (ifn i 0)
       ;; use the following line instead of the jump to get location-independent code (a relative jump)
       ;; (sub pc (relative loop))
       (set pc loop)
       (set x #x4) ;; this is assembled to a single word, instead of notch's (inefficient) two words
       (jsr testsub) ;; this is assembled to a single word, instead of notch's (inefficient) two words
       (set pc crash) ;; this is assembled to a single word, instead of notch's (inefficient) two words
       testsub
       (shl x 4)
       (set pc pop)
       crash
       (set pc crash) ;; this is assembled to a single word, instead of notch's (inefficient) two words
       )
      '(#x7C01 #x30 #x7DE1 #x1000 #x20 #x7803 #x1000
	   #xC00D #xD9C1 #xA861 #x7C01 #x2000 #x2161
	   #x2000 #x8463 #x806D #xB1C1 #x9031 #xD010
	   #xD9C1 #x9037 #x61C1 #xD9C1))

(define fibonacci '((set a 0)
		    (set b 1)
		    (set i 0)
		    loop
		    (add a b)
		    (set x a)
		    (set a b)
		    (set b x)
		    (add i 1)
		    (ifg 15 i)
		    (jb loop)
		    (brk)))

(define video-output 
  '((set a #xbeef)
    (set (ref #x1000) a)
    (ifn a (ref #x1000))
    (set pc end)
    (set i 0)
    nextchar
    (ife (ref (+ data i)) 0)
    (set pc end)
    (set (ref (+ #x8000 i)) (ref (+ data i)))
    (add i 1)
    (set pc nextchar)
    data
    "Hello World!"
    0
    end
    (sub pc 1)))

(define read-input '((set push i)
		     (set i (ref keypointer))
		     (add i #x9000)
		     (set target (ref i))
		     (ife target 0)
;		     (set pc end)
		     (jf end)
		     (set (ref i) 0)
		     (add (ref keypointer) 1)
		     (and (ref keypointer) #xf)
		     end
		     (set i pop)
		     (brk)
		     keypointer
		     0
		     target 
		     0))



(define reg-a #x00)
(define reg-b #x01)
(define reg-c #x02)
(define reg-x #x03)
(define reg-y #x04)
(define reg-z #x05)
(define reg-i #x06)
(define reg-j #x07)
(define reg-a-ind #x08)
(define reg-b-ind #x09)
(define reg-c-ind #x0a)
(define reg-x-ind #x0b)
(define reg-y-ind #x0c)
(define reg-z-ind #x0d)
(define reg-i-ind #x0e)
(define reg-j-ind #x0f)
(define reg-a-ind-offset #x10)
(define reg-b-ind-offset #x11)
(define reg-c-ind-offset #x12)
(define reg-x-ind-offset #x13)
(define reg-y-ind-offset #x14)
(define reg-z-ind-offset #x15)
(define reg-i-ind-offset #x16)
(define reg-j-ind-offset #x17)
(define reg-pop #x18)
(define reg-peek #x19)
(define reg-push #x1a)
(define reg-sp #x1b)
(define reg-pc #x1c)
(define reg-o #x1d)
(define ind-next-word #x1e)
(define lit-next-word #x1f)
(define (small-literal x)
  (assert (<= x #x1f))
  (assert (>= x 0))
  (+ #x20 x))

(define op-non-basic #x0)
(define op-set #x1)
(define op-add #x2)
(define op-sub #x3)
(define op-mul #x4)
(define op-div #x5)
(define op-mod #x6)
(define op-shl #x7)
(define op-shr #x8)
(define op-and #x9)
(define op-bor #xa)
(define op-xor #xb)
(define op-ife #xc)
(define op-ifn #xd)
(define op-ifg #xe)
(define op-ifb #xf)

(define op-jsr #x01)
(define (bi op a b)
  (+ (bitwise-arithmetic-shift-left b 10)
     (bitwise-arithmetic-shift-left a 4)
     op))

(define (nbi op a)
  (+ (bitwise-arithmetic-shift-left a 10)
     (bitwise-arithmetic-shift-left op 4)))

;; general purpose registers direct and indirect
(test-one (set a (ref a))
	  (bi op-set reg-a reg-a-ind))
(test-one (set b (ref b))
	  (bi op-set reg-b reg-b-ind))
(test-one (set c (ref c)) 
	  (bi op-set reg-c reg-c-ind))
(test-one (set x (ref x))
	  (bi op-set reg-x reg-x-ind))
(test-one (set y (ref y))
	  (bi op-set reg-y reg-y-ind))
(test-one (set z (ref z))
	  (bi op-set reg-z reg-z-ind))
(test-one (set i (ref i))
	  (bi op-set reg-i reg-i-ind))
(test-one (set j (ref j))
	  (bi op-set reg-j reg-j-ind))

;; [next word + register] / literals
(test-one (set (ref (+ 1 a)) 0)
	  (bi op-set reg-a-ind-offset (small-literal 0))
	  1)
(test-one (set (ref (+ 5 b)) 1)
	  (bi op-set reg-b-ind-offset (small-literal 1))
	  5)
(test-one (set (ref (+ #x10 c)) #x1f)
	  (bi op-set reg-c-ind-offset (small-literal #x1f))
	  #x10)
(test-one (set (ref (+ #x1234 x)) #x1f)
	  (bi op-set reg-x-ind-offset (small-literal #x1f))
	  #x1234)
(test-one (set (ref (+ #x1234 y)) #x4321)
	  (bi op-set reg-y-ind-offset lit-next-word)
	  #x1234
	  #x4321)

;; [a + 1] == [1 + a]
(test-one (set (ref (+ 1 a)) 0) (bi op-set reg-a-ind-offset (small-literal 0)) 1)
(test-one (set (ref (+ a 1)) 0) (bi op-set reg-a-ind-offset (small-literal 0)) 1)

;; stack operations
(test-one (set a pop) (bi op-set reg-a reg-pop))
(test-one (set peek 9) (bi op-set reg-peek (small-literal 9)))
(test-one (set push #xa) (bi op-set reg-push (small-literal #xa)))

;; sp pc o
(test-one (set sp #xb) (bi op-set reg-sp (small-literal #xb)))
(test-one (set pc #x1234) (bi op-set reg-pc lit-next-word) #x1234)
(test-one (set o #x20) (bi op-set reg-o lit-next-word) #x20)

;; jsr
(test-one (jsr a) (nbi op-jsr reg-a))
(test-one (jsr pop) (nbi op-jsr reg-pop))
(test-one (jsr #x0) (nbi op-jsr (small-literal 0)))
(test-one (jsr #x1234) (nbi op-jsr lit-next-word) #x1234)

;; indirect literal
(test-one (set (ref #xdd) #xff) (bi op-set ind-next-word lit-next-word) #xdd #xff)
(test-one (set (ref #x1f) #x20) (bi op-set ind-next-word lit-next-word) #x1f #x20)
(test-one (set (ref #x1f) #x1f) (bi op-set ind-next-word (small-literal #x1f)) #x1f)

;; all opcodes
(test-one (add a b) (bi op-add reg-a reg-b))
(test-one (sub a b) (bi op-sub reg-a reg-b))
(test-one (mul a b) (bi op-mul reg-a reg-b))
(test-one (div a b) (bi op-div reg-a reg-b))
(test-one (mod a b) (bi op-mod reg-a reg-b))
(test-one (shl a b) (bi op-shl reg-a reg-b))
(test-one (shr a b) (bi op-shr reg-a reg-b))
(test-one (and a b) (bi op-and reg-a reg-b))
(test-one (bor a b) (bi op-bor reg-a reg-b))
(test-one (xor a b) (bi op-xor reg-a reg-b))
(test-one (ife a b) (bi op-ife reg-a reg-b))
(test-one (ifn a b) (bi op-ifn reg-a reg-b))
(test-one (ifg a b) (bi op-ifg reg-a reg-b))
(test-one (ifb a b) (bi op-ifb reg-a reg-b))

(format #t "~%done~%")
