#| 
loophole: 
- remove (ret) after (jmp _)
- remove (set a a)
|#

;;; the following register *must* match assembler.scm!
(define +data-stack-register+ 'j)

(define (raise-error . params)
  (error #f (apply format #f params)))

(define (reset-virtual-registers!)
  (set! *first-virtual-register* 0))
(define *first-virtual-register* 0)

(define-record-type virtual-register (fields number))

(define (new-virtual-register)
  (set! *first-virtual-register* (+ 1 *first-virtual-register*))
  (make-virtual-register *first-virtual-register*))

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

(define (call? expr)
  (and (list? expr)
       (not (null? expr))
       (not (primitive? (car expr)))))

(define *primitives* (make-hash-table))

(define-syntax define-primitive
  (syntax-rules ()
    ((_ (prim-name target si tail-position? arg* ...) b b* ...)
     (begin
       (hashtable-set! *primitives* 'prim-name (make-primitive 'prim-name (length '(arg* ...)) (lambda (target si tail-position? arg* ...) b b* ...)))))))

(define (make-primitive name args emitter)
  (list '*primitive* name args emitter))

(define (primitive-emitter prim)
  (list-ref (hashtable-ref *primitives* prim #f) 3))

(define (primitive? x)
  (and (symbol? x)
       (hashtable-ref *primitives* x #f)))

(define (emit-primcall target si env expr tail-position?)
  (let ((prim (car expr))
	(args (cdr expr)))
    (apply (primitive-emitter prim) target si tail-position? env args)))

(define (variable-arity? function-name)
  #f)

(define (take n l)
  (if (zero? n)
      '()
      (cons (car l) (take (- n 1) (cdr l)))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define +non-tail-position+ #f)
(define +tail-position+ #t)
(define +return-value-target+ 'a) ;; register a

(define (emit-call target si env expr tail-position?)
  (if (variable-arity? (car expr))
      'todo
      (let* ((params (cdr expr))
	     (function (car expr))
	     (param-count (length params))
	     (param-places ;; only stack for now, later find out how to use a b c but prevent clobbering while calculating the other parameters 
	      ;; (take param-count `(a b c ,@(make-list (max 0 (- param-count 3)) 'push)))
	      (make-list param-count 'push)
))
	(for-each
	 (lambda (expr target)
	   (emit-expr target si env expr +non-tail-position+)) 
	 params
	 param-places)
	(let ((jump-target (new-virtual-register)))
	  (emit-expr jump-target si env function tail-position?)
	(if tail-position?
	    (emit `(jmp ,jump-target))
	    (begin
	      (emit `(call ,jump-target))
	      (emit-set target +return-value-target+))))
)))

(define (variable? x)
  (symbol? x))

(define (global-binding? expr env)
  (let ((bound (hashtable-ref *globals* expr #f)))
    (if bound
	#t
	(begin
	  (hashtable-set! *globals* expr 'used) ;; enter as used
	  #t))))

(define (local-binding? expr env)
  (assq expr env))

(define (local-binding-offset expr env)
  (cdr (assq expr env)))

(define (emit-set target value)
  (if (eq? target 'push)
      (emit `(data-push ,value))
      (emit `(set ,target ,value))))

(define (emit-local-binding-ref target si env expr tail-position?)
  (let ((binding-offset (local-binding-offset expr env)))
    (emit-set target `(ref (+ ,+data-stack-register+ ,binding-offset)))))

(define (emit-global-binding-ref target si env expr tail-position?)
  (emit-set target `(ref ,expr)))

(define (emit-variable-reference target si env expr tail-position?)
  (cond ((local-binding? expr env)
	 (emit-local-binding-ref target si env expr tail-position?))
	((global-binding? expr env)
	 (emit-global-binding-ref target si env expr tail-position?))
	(else
	 (raise-error "unknown binding type: ~a" expr))))

(define (lambda? expr)
  (and (list? expr)
       (not (null? expr))
       (eq? (car expr) 'lambda)
       (> (length expr) 2)))

(define (zip . lst)
  (apply map cons lst))

(define (extend-env env params)
  (zip params
       (reverse (iota (length params)))))  ;; push left to right, in order to support reduction for varargs

(define (emit-lambda target si env expr tail-position?)
  (let* ((params (cadr expr))
	 (body (cddr expr))
	 (env (extend-env env params))) ;; TODO: extend env
    ;; emit body
    (emit-expr +return-value-target+ si env (cons 'begin body) +tail-position+)
    (emit `(ret))))

(define (begin? expr)
  (and (list? expr)
       (not (null? expr))
       (eq? (car expr) 'begin)))

(define (emit-sequence target si env expr tail-position?)
  (let ((forms (cdr expr)))
    (for-each 
     (lambda (expr) (emit-expr target si env expr +non-tail-position+))
     (take (- (length forms) 1) forms))
    (emit-expr target si env (last forms) tail-position?)))

(define (emit-expr target si env expr tail-position?)
  (cond ((immediate? expr) (emit-immediate target expr tail-position?))
	((primcall? expr) (emit-primcall target si env expr tail-position?))
	((if? expr) (emit-if target si env expr tail-position?))
	((lambda? expr) (emit-lambda target si env expr tail-position?))
	((begin? expr) (emit-sequence target si env expr tail-position?))
	((call? expr) (emit-call target si env expr tail-position?))
;;	((let? expr) (emit-let target si env expr))
	((variable? expr) (emit-variable-reference target si env expr tail-position?))
	(else (raise-error "unknown expression: ~a" expr))))


;; (define (emit-let si env expr)
;;   (let loop ((bindings (let-bindings expr))
;; 	     (si si)
;; 	     (new-env env))
;;     (cond ((null? bindings)
;; 	   (emit-expr si new-env (let-body expr)))
;; 	  (else
;; 	   (let ((b (car bindings)))
;; 	     (emit-expr si env (rhs b))
;; 	     (emit-stack-save si)
;; 	     (format #t "new binding: ~a ~a~%" (lhs b) si)
;; 	     (loop (cdr bindings)
;; 		   (next-stack-index si)
;; 		   (extend-env new-env (lhs b) si)))))))

;; (define (extend-env env key value)
;;   (cons (cons key value) env))
(define (emit-prologue)
  (emit `(set j #xf800)) ; set data stack pointer
  )

(define (compile-program forms)
  (set! *result* '())
  (set! *globals* (make-hash-table))
  (reset-virtual-registers!)
  (emit-prologue)
  (for-each (lambda (form) (emit-top-level 'a (- word-size) '() form +tail-position+)) forms)
  (reverse *result*))

(define (emit-top-level target si env expr tail-position?)
  (cond ((define? expr)
	 (emit-definition target si env expr tail-position?))
	(else
	 (emit-expr target si env expr tail-position?))))

(define (define? expr)
  (and (list? expr)
       (> (length expr) 2)
       (eq? (car expr) 'define)
       (symbol? (cadr expr))))

(define (add-global-binding! name)
  (hashtable-set! *globals* name #t))

(define *globals* (make-hash-table))

(define (emit-definition target si env expr tail-position?)
  (let ((name (cadr expr))
	(value (caddr expr)))
    (add-global-binding! name)
    (if (lambda? value)
	(let ((end (unique-label))
	      (start (unique-label)))
	  (emit `(jmp ,end))
	  (emit name) ;; label
	  (emit `(ref ,start))
	  (emit start)
	  (emit-expr target si env value tail-position?)
	  (emit end))
	(emit-constant target si env name value tail-position?))))

(define general-purpose-registers '(b c i x y z))
;; a is used for return values, j is used as data stack pointer

(define (make-register-hash-table lst value)
  (let ((table (make-hash-table)))
    (for-each (lambda (x) (hashtable-set! table x value))
	      lst)
    table))

(define (substitute-registers-in-form form mapping free-registers future-virtual-registers)
  (let ((result (substitute-registers-in-form-aux form mapping))
	(vregs-in-form (find-virtual-registers-in-form form)))
    (for-each (lambda (virtual-register)
		      (if (not (memq virtual-register future-virtual-registers))
			  (remove-register-mapping! mapping free-registers virtual-register)))
		    vregs-in-form)
    result))

(define (substitute-registers-in-form-aux form mapping)
  (cond ((virtual-register? form)
	 (let ((subst (hashtable-ref mapping form #f)))
	   (if subst
	       subst
	       form)))
	((list? form)
	 (map (lambda (x) (substitute-registers-in-form-aux x mapping)) form))
	(else
	 form)))

(define (allocate-free-register free-registers-table)
  (let loop ((registers (vector->list (hashtable-keys free-registers-table))))
    (if (null? registers)
	(raise-error "no more free registers!")
	(if (hashtable-ref free-registers-table (car registers) #t)
	    (begin
	      (hashtable-set! free-registers-table (car registers) #f)
	      (car registers))
	    (loop (cdr registers))))))

(define (add-register-mapping! mapping reg virtual-reg)
  (hashtable-set! mapping virtual-reg reg))

(define (remove-register-mapping! mapping free-registers-table virtual-register)
  (let ((register (hashtable-ref mapping virtual-register #f)))
    (hashtable-delete! mapping virtual-register)
    (hashtable-set! free-registers-table register #t)))

(define (find-virtual-registers-in-form form)
  (if (list? form)
      (let loop ((registers '())
		 (form form))
	(remove-duplicates (filter virtual-register? (flatten form))))
      '()))

(define (remove-duplicates l)
  (cond ((null? l)
         '())
        ((member (car l) (cdr l))
         (remove-duplicates (cdr l)))
        (else
         (cons (car l) (remove-duplicates (cdr l))))))

(define (allocate-registers-in-form form rest free-registers register-mapping)
  (if (list? form)
      (let* ((virtual-registers (find-virtual-registers-in-form form))
	     (regs (map (lambda (x) (add-register-mapping! register-mapping (allocate-free-register free-registers) x)) virtual-registers)))
	(let* ((future-vregs (remove-duplicates (apply append (map find-virtual-registers-in-form rest))))
	       (result (substitute-registers-in-form form register-mapping free-registers future-vregs)))
	  (for-each (lambda (virtual-register)
		      (if (not (memq virtual-register future-vregs))
			  (remove-register-mapping! register-mapping free-registers virtual-register)))
		    virtual-registers)
	  result))
      form))

;; primitive first try
(define (allocate-registers forms)
  (let loop ((free-registers (make-register-hash-table general-purpose-registers #t))
	     (forms forms)
	     (register-mapping (make-hash-table))
	     (result '()))
    (if (null? forms)
	(reverse result)
	(let* ((future-vregs (remove-duplicates (apply append (map find-virtual-registers-in-form (cdr forms)))))
	       (form (substitute-registers-in-form (car forms) register-mapping free-registers future-vregs))) ;; substitute known substitutions
	  (loop free-registers 
		(cdr forms)
		register-mapping 
		(cons (allocate-registers-in-form form forms free-registers register-mapping) result))))))

(define (emit-constant target si env name expr tail-position?)
  (let ((end (unique-label)))
    (emit `(jmp ,end))
    (emit name)
    (emit (immediate-rep expr))
    (emit end)))

;; (define (let? expr)
;;   (and (list? expr)
;;        (eq? (car expr) 'let)))
;; (define lhs car)
;; (define rhs cadr)
;; (define let-bindings cadr)
;; (define let-body caddr) ;; TODO: beginify

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
;; (define (emit-stack-save si)
;;   (emit `(set (ref (+ z ,si)) a)))

;; (define (emit-stack-load si)
;;   (emit `(set a (ref (+ z ,si)))))

(define (emit-if target si env expr tail-position?)
  (let ((alt-label (unique-label))
	(end-label (unique-label))
	(tmp (new-virtual-register)))
    (emit-expr tmp si env (if-test expr) +non-tail-position+)
    (emit `(ife ,tmp ,special/false))
    (emit `(jf ,alt-label))
    (emit-expr target si env (if-conseq expr) tail-position?)
    (emit `(jf ,end-label))
    (emit alt-label)
    (emit-expr target si env (if-altern expr) tail-position?)
    (emit end-label)))

(define (if-test expr)
  (list-ref expr 1))
(define (if-conseq expr)
  (list-ref expr 2))
(define (if-altern expr)
  (list-ref expr 3))

(define (immediate? x)
  (or (number? x) (boolean? x) (null? x) (char? x)))

(define (emit-immediate target expr tail-position?)
  (emit-set target (immediate-rep expr)))

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



;; primitives

(define-primitive (fxadd1 target si tail-position? env arg)
  (emit-expr target si env arg +non-tail-position+)
  (emit `(add ,target ,(immediate-rep 1))))

(define-primitive (fx+ target si tail-position? env arg1 arg2)
  (let ((vr-arg1 (new-virtual-register))
	(vr-arg2 (new-virtual-register)))
    (emit-expr vr-arg1 si env arg1 +non-tail-position+)
    (emit-expr vr-arg2 si env arg2 +non-tail-position+)
    (emit `(add ,vr-arg1 ,vr-arg2))
    (emit-set target vr-arg1)))

(define-primitive (fx* target si tail-position? env arg1 arg2)
  (let ((vr-arg1 (new-virtual-register))
	(vr-arg2 (new-virtual-register)))
    (emit-expr vr-arg1 si env arg1 +non-tail-position+)
    (emit-expr vr-arg2 si env arg2 +non-tail-position+)
    (emit `(shr ,vr-arg1 2))
    (emit `(shr ,vr-arg2 2))
    (emit `(mul ,vr-arg1 ,vr-arg2))
    (emit `(shl ,vr-arg1 2))
    (emit-set target vr-arg1)))

(define-primitive (fx- target si tail-position? env arg1 arg2)
  (let ((vr-arg1 (new-virtual-register))
	(vr-arg2 (new-virtual-register)))
    (emit-expr vr-arg1 si env arg1 +non-tail-position+)
    (emit-expr vr-arg2 si env arg2 +non-tail-position+)
    (emit `(sub ,vr-arg1 ,vr-arg2))
    (emit-set target vr-arg1)))

(define-primitive (fx< target si tail-position? env arg1 arg2)
  (let ((vr-arg1 (new-virtual-register))
	(vr-arg2 (new-virtual-register))
	(vr-res (new-virtual-register)))
    (emit-expr vr-arg1 si env arg2 +non-tail-position+)
    (emit-expr vr-arg2 target si env arg1 +non-tail-position+)
    (emit-set vr-res special/true)
    (emit `(ife ,vr-arg1 ,vr-arg2))
    (emit-set vr-res special/false)
    (emit `(ifg ,vr-arg1 ,vr-arg2))
    (emit-set vr-res special/false)
    (emit-set target vr-res)))

(define-primitive (fx= target si tail-position? env arg1 arg2)
  (let ((vr-arg1 (new-virtual-register))
	(vr-arg2 (new-virtual-register))
	(vr-res (new-virtual-register)))
    (emit-expr vr-arg1 si env arg2 +non-tail-position+)
    (emit-expr vr-arg2 si env arg1 +non-tail-position+)
    (emit-set vr-res special/false)
    (emit `(ife ,vr-arg1 ,vr-arg2))
    (emit-set vr-res special/true)
    (emit-set target vr-res)))
