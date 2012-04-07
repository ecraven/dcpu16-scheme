(define (raise-error . params)
  (error #f (apply format #f params)))

(define fixnum-tag #b00)
(define fixnum-tag-mask #b11)
(define char-tag #b01)

(define special-tag #b10)

(define (make-tagger tag shift)
  (lambda (n)
    (bitwise-ior (bitwise-arithmetic-shift-left n shift) tag)))

(define tag-fixnum (make-tagger fixnum-tag 2))
(define tag-special (make-tagger special-tag 4))
(define tag-char (make-tagger char-tag 2))

(define special/false (tag-special 0))
(define special/true (tag-special 1))
(define special/empty-list (tag-special 2))

(define (primcall? expr)
  (and (list? expr)
       (not (null? expr))
       (primitive? (car expr))))

(define *primitives* (make-hash-table))

(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name si arg* ...) b b* ...)
     (begin
       (hashtable-set! *primitives* 'prim-name (make-primitive 'prim-name (length '(arg* ...)) (lambda (si arg* ...) b b* ...)))))))

(define (make-primitive name args emitter)
  (list '*primitive* name args emitter))

(define (primitive-emitter prim)
  (list-ref (hashtable-ref *primitives* prim #f) 3))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit `(add a ,(immediate-rep 1))))

(define-primitive (fx+ si env arg1 arg2)
  (emit-expr si env arg1)
  (emit '(push a))
  (emit-expr si env arg2)
  (emit '(add a pop)))

(define-primitive (fx- si env arg1 arg2)
  (emit-expr si env arg2)
  (emit '(push a))
  (emit-expr si env arg1)
  (emit '(sub a pop)))

(define-primitive (fx< si env arg1 arg2)
  (emit-expr si env arg2)
  (emit `(push a))
  (emit-expr si env arg1)
  (emit `(pop b))
  (emit `(set c ,special/true))
  (emit `(ife a b))
  (emit `(set c ,special/false))
  (emit `(ifg a b))
  (emit `(set c ,special/false))
  (emit `(set a c)))

(define-primitive (fx= si env arg1 arg2)
  (emit-expr si env arg2)
  (emit `(push a))
  (emit-expr si env arg1)
  (emit `(pop b))
  (emit `(set c ,special/false))
  (emit `(ife a b))
  (emit `(set c ,special/true))
  (emit `(set a c)))

(define (primitive? x)
  (and (symbol? x)
       (hashtable-ref *primitives* x #f)))

(define (emit-primcall si env expr)
  (let ((prim (car expr))
	(args (cdr expr)))
    (apply (primitive-emitter prim) si env args)))

(define (emit-expr si env expr)
  (cond ((immediate? expr) (emit-immediate expr))
	((primcall? expr) (emit-primcall si env expr))
	((if? expr) (emit-if si env expr))
	((let? expr) (emit-let si env expr))
	(else (raise-error "unknown expression: ~a" expr))))


(define (emit-let si env expr)
  (let loop ((bindings (let-bindings expr))
	     (si si)
	     (new-env env))
    (cond ((null? bindings)
	   (emit-expr si new-env (let-body expr)))
	  (else
	   (let ((b (car bindings)))
	     (emit-expr si env (rhs b))
	     (emit-stack-save si)
	     (format #t "new binding: ~a ~a~%" (lhs b) si)
	     (loop (cdr bindings)
		   (next-stack-index si)
		   (extend-env new-env (lhs b) si)))))))

(define (extend-env env key value)
  (cons (cons key value) env))

(define (compile-program form)
  (set! *result* '())
  (emit-expr (- word-size) '() form)
  (reverse *result*))

(define (let? expr)
  (and (list? expr)
       (eq? (car expr) 'let)))
(define lhs car)
(define rhs cadr)
(define let-bindings cadr)
(define let-body caddr) ;; TODO: beginify
(define (next-stack-index si)
  (- si word-size))

(define word-size 1)

(define (if? expr)
  (and (list? expr)
       (eq? (car expr) 'if)))

(define unique-label 
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (string->symbol (string-append "label-" (number->string count))))))

;; these assume z is set correctly
(define (emit-stack-save si)
  (emit `(set (ref (+ z ,si)) a)))

(define (emit-stack-load si)
  (emit `(set a (ref (+ z ,si)))))

(define (emit-if si env expr)
  (let ((alt-label (unique-label))
	(end-label (unique-label)))
    (emit-expr si env (if-test expr))
    (emit `(ife a ,special/false))
    (emit `(jf ,alt-label))
    (emit-expr si env (if-conseq expr))
    (emit `(jf ,end-label))
    (emit alt-label)
    (emit-expr si env (if-altern expr))
    (emit end-label)
    ))

(define (if-test expr)
  (list-ref expr 1))
(define (if-conseq expr)
  (list-ref expr 2))
(define (if-altern expr)
  (list-ref expr 3))

(define (immediate? x)
  (or (number? x) (boolean? x) (null? x) (char? x)))

(define (emit-immediate expr)
  (emit `(set a ,(immediate-rep expr))))

(define *result* '())

(define (emit form)
  (set! *result* (cons form *result*)))

(define (immediate-rep x)
  (cond ((integer? x)
	 (tag-fixnum x))
	((boolean? x)
	 (if x special/true special/false))
	((null? x)
	 special/empty-list)
	((char? x)
	 (tag-char (char->integer x)))
	(else
	 (raise-error "unknown immediate: ~a" x))))

