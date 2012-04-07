(load "assembler.scm")

(define (glue-3hex operator a b)
  (cond
    ; woe is me, copy pasta ahoy
    [(> a #b111111) (raise-error "> 6 bits: ~a" a)]
    [(> b #b111111) (raise-error "> 6 bits: ~a" b)]
    [(> operator #b1111) (raise-error "> 4 bits: ~a" operator)]
    [else
     (+ (bitwise-arithmetic-shift b 10) (bitwise-arithmetic-shift a 4) operator)]))

; as a sanity check, one could check that...
(assert (equal? (glue-3hex #xf #x3f #x3f) #xffff))

(define (glue-hex hex)
  (if (list? hex)
      (case (length hex)
        [(3) (apply glue-3hex hex)]
        [(2) (apply glue-3hex 0 hex)]
        [else (raise-error "Invalid hexop: ~a" hex)])
      hex))

; apparently chez lacks define-syntax-rule -- so lame!
(define-syntax hexops
  (syntax-rules ()
    [(_ <hex> ...)
     (map glue-hex '(<hex> ...))]))

(define-syntax test-assembly
  (syntax-rules ()
    [(_) (begin (format #t "Done!") (newline))]
    [(_ (<test-name> [<left> <right>] ...) <other-tests> ...)
       (let ([passed (andmap (lambda (a) (apply test-two a)) `((,<left> ,<right> <left> <right>) ...))])
        (and passed
            (begin
             (format #t "~a: ... PASS" <test-name>)
             (newline)
             #t))
           
        (test-assembly <other-tests> ...))]))
           

(define (test-two left right left-display right-display)
    (let ([left (incorporate-labels (fixup-labels (assemble left)))])
        (if (equal? left right)
            #t
            (begin
             (format #t "~a: ... FAIL" <test-name>)
             (newline)
             (format #t "    ~a == ~a" '<left> left)
             (newline)
             (format #t "    ~a == ~a" '<right> right)
             (newline)
             #f))))

(test-assembly
 ("Notch's example code :)"
  ['(
     (set a #x30)
     (set (ref #x1000) #x20)
     (sub a (ref #x1000))
     (ifn a #x10)
     (set pc crash)
     (set i 10)
     (set a #x2000)
     loop
     (set (ref (+ i #x2000)) (ref a))
     (sub i 1)
     (ifn i 0)
     ;; this would be better than the actual jump
     ; (sub pc (relative loop))
     (set pc loop)
     (set x #x4)
     (jsr testsub)
     (set pc crash)
     testsub
     (shl x 4)
     (set pc pop)
     crash
     (set pc crash))
   `(
     #x7c01 #x0030
            #x7de1 #x1000 #x0020
            #x7803 #x1000
            #xc00d
            ;; This assembler is better than Notch's. :)
            ;#x7dc1 #x001a
            ,(glue-3hex #x1 #x1c #x36) ; looks about right.
            #xa861
            #x7c01 #x2000
            #x2161 #x2000
            #x8463
            #x806d
            ,(glue-3hex #x1 #x1c #x2c)
            #x9031
            ,(glue-3hex #x0 #x1 #x34)
            ,(glue-3hex #x1 #x1c #x36)
            #x9037
            #x61c1
            ,(glue-3hex #x1 #x1c #x36))])
 
 ("registers / [registers]"
  ['((set a (ref a))) (hexops (#x1 #x0 #x8))]
  ['((set b (ref b))) (hexops (#x1 #x1 #x9))]
  ['((set c (ref c))) (hexops (#x1 #x2 #xa))]
  ['((set x (ref x))) (hexops (#x1 #x3 #xb))]
  ['((set y (ref y))) (hexops (#x1 #x4 #xc))]
  ['((set z (ref z))) (hexops (#x1 #x5 #xd))]
  ['((set i (ref i))) (hexops (#x1 #x6 #xe))]
  ['((set j (ref j))) (hexops (#x1 #x7 #xf))])
 
 ("[next word + register] / literals"
  ['((set (ref (+ 1 a)) 0)) (hexops (#x1 #x10 #x20) 1)]
  ['((set (ref (+ 1 b)) 1)) (hexops (#x1 #x11 #x21) 1)]
  ['((set (ref (+ 1 c)) 2)) (hexops (#x1 #x12 #x22) 1)]
  ['((set (ref (+ 1 x)) 3)) (hexops (#x1 #x13 #x23) 1)]
  ['((set (ref (+ 1 y)) 4)) (hexops (#x1 #x14 #x24) 1)]
  ['((set (ref (+ 1 z)) 5)) (hexops (#x1 #x15 #x25) 1)]
  ['((set (ref (+ 1 i)) 6)) (hexops (#x1 #x16 #x26) 1)]
  ['((set (ref (+ 1 j)) 7)) (hexops (#x1 #x17 #x27) 1)])
 
 ("[A + 1] == [1 + A]"
  ['((set (ref (+ 1 a)) 0)) (hexops (#x1 #x10 #x20) 1)]
  ['((set (ref (+ a 1)) 0)) (hexops (#x1 #x10 #x20) 1)])
 
 
 ("Stack operations"
  ['((set pop 8)) (hexops (#x1 #x18 #x28))]
  ['((set peek 9)) (hexops (#x1 #x19 #x29))]
  ['((set push #xa)) (hexops (#x1 #x1a #x2a))])
 
 ("Rest of the values (SP/PC/O, [next], next)"
  ['((set sp #xb)) (hexops (#x1 #x1b #x2b))]
  ['((set pc #xc)) (hexops (#x1 #x1c #x2c))]
  ['((set o #xd)) (hexops (#x1 #x1d #x2d))]
  ['((set (ref #xff) #xe)) (hexops (#x1 #x1e #x2e) #xff)]
  ['((set #xff #xf)) (hexops (#x1 #x1f #x2f) #xff)])
 
 ("Basic opcodes"
  ['((set a #x10)) (hexops (#x1 0 #x30))]
  ['((add a #x11)) (hexops (#x2 0 #x31))]
  ['((sub a #x12)) (hexops (#x3 0 #x32))]
  ['((mul a #x13)) (hexops (#x4 0 #x33))]
  ['((div a #x14)) (hexops (#x5 0 #x34))]
  ['((mod a #x15)) (hexops (#x6 0 #x35))]
  ['((shl a #x16)) (hexops (#x7 0 #x36))]
  ['((shr a #x17)) (hexops (#x8 0 #x37))]
  ['((and a #x18)) (hexops (#x9 0 #x38))]
  ['((bor a #x19)) (hexops (#xa 0 #x39))]
  ['((xor a #x1a)) (hexops (#xb 0 #x3a))]
  ['((ife a #x1b)) (hexops (#xc 0 #x3b))]
  ['((ifn a #x1c)) (hexops (#xd 0 #x3c))]
  ['((ifg a #x1d)) (hexops (#xe 0 #x3d))]
  ['((ifb a #x1e)) (hexops (#xf 0 #x3e))])
 
 ("Non-basic opcodes"
  ['((jsr a)) (hexops (#x01 #x00))]))

