;; add include, asm syntax (with escape via !, like sassy)
(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((next (read))
		 (result '()))
	(if (eof-object? next)
	    (reverse result)
	    (loop (read) (cons next result)))))))

(define (raise-error . params)
  (error #f (apply format #f params)))

(define +registers+ '((a #x00 (ref #x08) (plus-next #x10))
		      (b #x01 (ref #x09) (plus-next #x11))
		      (c #x02 (ref #x0a) (plus-next #x12))
		      (x #x03 (ref #x0b) (plus-next #x13))
		      (y #x04 (ref #x0c) (plus-next #x14))
		      (z #x05 (ref #x0d) (plus-next #x15))
		      (i #x06 (ref #x0e) (plus-next #x16))
		      (j #x07 (ref #x0f) (plus-next #x17))
		      (pop #x18)
		      (peek #x19)
		      (push #x1a)
		      (sp #x1b)
		      (pc #x1c)
		      (o #x1d)
		      (reference-next #x1e)
		      (literal #x1f)))

(define (find-register name)
  (assq name +registers+))

(define (register-direct reg)
  (cadr reg))

(define (register-indirect reg)
  (cadr (caddr reg)))

(define (register-indirect-offset reg offset high-or-low)
  (list (cadr (cadddr reg))
	(if (number? offset)
	    offset
	    (list 'absolute offset #f #f #f))))

(define-record-type opcode (fields name type opcode cycle-cost))

;; implicit: non-basic opcodes take 1 argument, basic opcodes take 2
(define +opcodes+ 
  `((set ,(make-opcode 'set 'basic #x1 1))
    (add ,(make-opcode 'add 'basic #x2 2))
    (sub ,(make-opcode 'sub 'basic #x3 2))
    (mul ,(make-opcode 'mul 'basic #x4 2))
    (div ,(make-opcode 'div 'basic #x5 3))
    (mod ,(make-opcode 'mod 'basic #x6 3))
    (shl ,(make-opcode 'shl 'basic #x7 2))
    (shr ,(make-opcode 'shr 'basic #x8 2))
    (and ,(make-opcode 'and 'basic #x9 1))
    (bor ,(make-opcode 'bor 'basic #xa 1))
    (xor ,(make-opcode 'xor 'basic #xb 1))
    (ife ,(make-opcode 'ife 'basic #xc 2))		   ;; plus 1 cost if the test fails
    (ifn ,(make-opcode 'ifn 'basic #xd 2))		   ;; plus 1 cost if the test fails
    (ifg ,(make-opcode 'ifg 'basic #xe 2))		   ;; plus 1 cost if the test fails
    (ifb ,(make-opcode 'ifb 'basic #xf 2))		   ;; plus 1 cost if the test fails
    (jsr ,(make-opcode 'jsr 'non-basic #x01 2))))

(define +data-stack-register+ 'j)
(define +macros+
  `(((nop) (set a a))
    ((push-gprs) (push b) (push c) (push i) (push x) (push y) (push z))
    ((pop-gprs) (pop z) (pop y) (pop x) (pop i) (pop c) (pop b))
    ((jmp target) (set pc target))
    ((jb target) (sub pc (relative target)))
    ((jf target) (add pc (relative target)))
    ((push value) (set push value))
    ((pop target) (set target pop))
    ((data-push value) (sub ,+data-stack-register+ 1) (set (ref ,+data-stack-register+) value))
    ((data-pop target) (set target (ref ,+data-stack-register+)) (add ,+data-stack-register+ 1))
    ((call target) (jsr target))
    ((ret) (set pc pop))
    ((brk) 0)))

(define (find-opcode name)
  (let ((data (assq name +opcodes+)))
    (if data 
	(cadr data)
	#f)))

(define (find-macro-parameter-mapping macro instruction)
  (if (null? macro)
      '()
      (cons (cons (car macro) (car instruction)) (find-macro-parameter-mapping (cdr macro) (cdr instruction)))))

(define (expand-parameters-in-macro expansion params)
  (map (lambda (x) (if (list? x) (expand-parameters-in-macro x params) 
		  (let ((exp (assq x params))) (if exp (cdr exp) x)))) expansion))

(define (expand-macro-with-pattern instruction macro expansion)
  (let ((params (find-macro-parameter-mapping (cdr macro) (cdr instruction))))
    (expand-parameters-in-macro expansion params)))

(define (macro-expand instruction)
  (if (list? instruction)
      (let* ((op (car instruction))
	     (macro (find (lambda (x) (eq? (caar x) op)) +macros+)))
	(if macro
	    (let ((macro-expansion (cdr macro)))
	      (if (null? (cdar macro)) ;; no parameters
		  macro-expansion
		  (expand-macro-with-pattern instruction (car macro) macro-expansion)))
	    instruction))
      instruction))

(define (assemble-one orig-instruction)
  (let ((instruction (macro-expand orig-instruction)))
    (if (equal? instruction orig-instruction)
	(cond ((symbol? instruction) ;; label
	       `(,instruction))
	      ((and (number? instruction)
		    (not (negative? instruction))
		    (< instruction #x10000)) ;; int
	       `(,instruction))
	      ((string? instruction)
	       (map char->integer (string->list instruction)))
	      ((pair? instruction)
	       (let* ((op (car instruction))
		      (op-data (find-opcode op)))
		 (if (not op-data)
		     (if (eq? op 'ref)
			 `(,instruction)
			 (raise-error "unknown op code: ~a" op))
		     (if (eq? 'basic (opcode-type op-data))
			 (assemble-basic instruction op-data)
			 (assemble-non-basic instruction op-data)))))
	      (else
	       (raise-error "unknown instruction: ~a" instruction)))
	(flatten-once (map assemble-one instruction)))))

(define (operand instruction n)
  (if (<= (+ 1 (length instruction)) n)
      (raise-error "not enough operands: ~a" instruction)
      (list-ref instruction (+ 1 n))))

(define (assemble-basic instruction op-data)
  (let ((opcode (opcode-opcode op-data))
	(op-a (assemble-param (operand instruction 0) 'low))
	(op-b (assemble-param (operand instruction 1) 'high)))
    (let ((op-a (if (list? op-a) (car op-a) op-a))
	  (op-b (if (list? op-b) (car op-b) op-b))
	  (rest (if (list? op-a)
		    (cons (cadr op-a)
			  (if (list? op-b)
			      (list (cadr op-b))
			      '()))
		    (if (list? op-b)
			(list (cadr op-b))
			'()))))
      (cons (+ (bitwise-arithmetic-shift-left op-b 10)
	       (bitwise-arithmetic-shift-left op-a 4)
	       opcode)
	    rest))))

(define (assemble-non-basic instruction op-data)
  (let ((opcode (opcode-opcode op-data))
	(op-a (assemble-param (operand instruction 0) 'high)))
    (let ((op-a (if (list? op-a) (car op-a) op-a))
	  (rest (if (list? op-a)
		    (list (cadr op-a))
		    '())))
      (cons (+ (bitwise-arithmetic-shift-left op-a 10)
	       (bitwise-arithmetic-shift-left opcode 4))
	    rest))))

(define (assemble-param param high-or-low)
  (cond ((symbol? param)
	 (let ((reg (find-register param)))
	   (if reg
	       (register-direct reg)
	       (list #x1f (list 'absolute param high-or-low #f #f))))) ;; internal? offset
	((list? param)
	 (cond ((eq? (car param) 'ref)
		(let ((reg (find-register (cadr param))))
		  (cond (reg
			 (register-indirect reg))
			((number? (cadr param))
			 (list #x1e (cadr param)))
			((symbol? (cadr param))
			 (list #x1e (list 'absolute (cadr param) #f #f #f)))
			((and (list? (cadr param))
			      (eq? '+ (caadr param)))
			 ;; (ref (+ x lit))  or  (ref (+ x label))
			 (let* ((operands (cdadr param))
				(op-a (car operands))
				(op-b (cadr operands)))
			   (let ((op-a-reg (find-register op-a))
				 (op-b-reg (find-register op-b)))
			     (cond (op-a-reg
				    (register-indirect-offset op-a-reg op-b high-or-low))
				   (op-b-reg
				    (register-indirect-offset op-b-reg op-a high-or-low))
				   (else
				    (raise-error "illegal indirect offset: ~a" operands))))))
			(else
			 (raise-error "cannot reference: ~a" param)))))
	       ((eq? (car param) 'relative)
		(if (symbol? (cadr param))
		    (list #x1f (list 'relative (cadr param) high-or-low #f #f))
		    (raise-error "not a literal: ~a" (cadr param)))) ;; internal? offset
	       (else
		(raise-error  "unknown param type: ~a" param))))
	((number? param)
	 (if (<= param #x1f)
	     (+ #x20 param)
	     (list #x1f param)))
	(else
	 (raise-error "unknown parameter: ~a" param))))

(define (flatten-once lst)
  (apply append lst))

(define (flatten list)
  (cond ((null? list) '())
	((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
	(else
	 (cons (car list) (flatten (cdr list))))))

(define (assemble lst)
  (incorporate-labels (fixup-labels (flatten-once (map assemble-one lst)))))

(define (pa lst)
  (map (lambda (x) (if (number? x) (format #f "~x" x) x)) (assemble lst)))

(define (label-index lst lbl)
  (let loop ((lst lst)
	     (count 0))
    (if (null? lst)
	(raise-error "label not found: ~a" lbl))
    (if (equal? (car lst) lbl)
	count
	(loop (cdr lst)
	      (+ count (cond ((number? (car lst))
			      1)
			     ((symbol? (car lst))
			      0)
			     ((and (list? (car lst))
				   (member (caar lst) '(absolute relative)))
			      (if (list-ref (car lst) 3) ;; internal?
				  0
				  1))
			     ((and (list? (car lst))
				   (eq? (caar lst) 'ref))
			      1)
			     (else
			      (raise-error "unknown item in list: ~a" (car lst)))))))))

(define (fixup-labels lst)
  (let loop ((changes #f)
	     (count 0))
    (for-each (lambda (instr)
;		(format #t "fixup: ~a~%" instr)
		(cond  ((number? instr)
			#t)
		       ((symbol? instr)
			#t)
		       ((and (list? instr)
			     (eq? (car instr) 'absolute))
			(let* ((label (cadr instr))
			       (fixup? (list-ref instr 2))
			       (internal? (list-ref instr 3))
			       (index (label-index lst label))
			       (new-internal? (and fixup? (<= index #x1f))))
			  (if (not (eq? new-internal? internal?))
			      (begin
				(set-car! (cdddr instr) new-internal?)
				(set! changes #t)))
			  (set-car! (cddddr instr) index)))
		       ((and (list? instr)
			     (eq? (car instr) 'relative))
			(let* ((label (cadr instr))
			       (internal? (list-ref instr 3))
			       (my-index (label-index lst instr))
			       (index (label-index lst label))
			       (diff (- index my-index))
			       (new-type (<= (abs diff) #x1f)))
			  (if (not (eq? new-type internal?))
			      (begin
				(set-car! (cdddr instr) new-type)
				(set! changes #t)))
;			  (format #t "relative jump by ~a (actually: ~a)~%" (abs diff) diff)
			  (set-car! (cddddr instr) (abs diff))))
		       ((and (list? instr)
			     (eq? (car instr) 'ref))
			'ok)
		       (else
			(raise-error "unknown fixup: ~a" instr))))
	      lst)
    (if changes
	(if (< count 10)
	    (loop #f (+ count 1))
	    lst)
	lst)))

(define (patch-instruction-immediate instruction word immediate)
  (let* ((shift (if (eq? word 'high) 10 4))
	 (mask (bitwise-arithmetic-shift-left #x3f shift)))
    (+ (bitwise-and (bitwise-not mask) instruction) 
       (bitwise-arithmetic-shift-left (+ #x20 immediate) shift))))

(define (hexdump lst . file)
  (if (null? file)
      (for-each (lambda (x) (display x) (newline)) (pa lst))
      (with-output-to-file (car file)
	(lambda () 
	  (for-each (lambda (x) (display x) (newline)) (pa lst))))))

(define (incorporate-labels forms)
  (let loop ((lst forms)
	     (result '()))
    (if (null? lst)
	(reverse result)
	(cond ((number? (car lst))
	       (loop (cdr lst) (cons (car lst) result)))
	      ((symbol? (car lst))
	       (loop (cdr lst) result))
	      ((and (list? (car lst))
		    (memq (caar lst) '(absolute relative)))
	       (let* ((item (car lst))
		      (word (list-ref item 2))
		      (internal (list-ref item 3))
		      (offset (list-ref item 4)))
		 (if internal
		     (begin 
		       (set-car! result (patch-instruction-immediate (car result) word offset))
		       (loop (cdr lst) result))
		     (loop (cdr lst) (cons offset result)))))
	      ((and (list? (car lst))
		    (eq? (caar lst) 'ref))
	       (let ((label (cadar lst)))
		 (loop (cdr lst) (cons (label-index forms label) result))))
	      (else
	       (raise-error "unknown item: ~a" (car lst)))))))



;; foo: add PC, 1 (7dc2 0001)   ->  PC is foo+3
