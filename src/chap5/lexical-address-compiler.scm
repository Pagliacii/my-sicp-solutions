(load "compiler")

;; Exercise 5.39
(define (make-lexical-address frame-num displacement)
  (list frame-num displacement))
(define (frame-num address) (car address))
(define (displacement address) (cadr address))
(define (lexical-address-lookup address env)
  (let ((frame (list-ref env (frame-num address))))
    (let ((value (list-ref (frame-values frame) (displacement address))))
      (if (eq? value '*unassigned*)
          (error "Unassigned variable: COMPILE" address)
          value))))
(define (lexical-address-set! address val env)
  (let ((frame (list-ref env (frame-num address))))
    (list-set! (frame-values frame) (displacement address) val)))

;; Exercise 5.40
;;; add a compile-time-environment argument
(define (compile exp target linkage ctenv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage ctenv))
        ((assignment? exp)
         (compile-assignment exp target linkage ctenv))
        ((definition? exp)
         (compile-definition exp target linkage ctenv))
        ((if? exp) (compile-if exp target linkage ctenv))
        ((lambda? exp) (compile-lambda exp target linkage ctenv))
        ((begin? exp)
         (compile-sequence
          (begin-actions exp) target linkage ctenv))
        ((cond? exp)
         (compile (cond->if exp) target linkage ctenv))
        ((application? exp)
         (compile-application exp target linkage ctenv))
        (else
         (error "Unknown expression type: COMPILE" exp))))
;;; extend it in compile-lambda-body
(define (extend-ctenv frame ctenv)
  (cons frame ctenv))
;(define (compile-lambda-body exp proc-entry ctenv)
;  (let ((formals (lambda-parameters exp)))
;    (append-instruction-sequences
;     (make-instruction-sequence '(env proc argl) '(env)
;                                `(,proc-entry
;                                  (assign env
;                                          (op compiled-procedure-env)
;                                          (reg proc))
;                                  (assign env
;                                          (op extend-environment)
;                                          (const ,formals)
;                                          (reg argl)
;                                          (reg env))))
;     (compile-sequence (lambda-body exp) 'val 'return (extend-ctenv formals ctenv)))))

;; Exercise 5.41
(define (find-variable var env)
  (define (lookup-frame num frames)
    (if (null? frames)
        'not-found
        (let ((displacement (lookup-variable 0 (car frames))))
          (if displacement
              (list num displacement)
              (lookup-frame (+ num 1) (cdr frames))))))
  (define (lookup-variable displacement frame)
    (cond ((null? frame) false)
          ((eq? var (car frame)) displacement)
          (else (lookup-variable (+ displacement 1) (cdr frame)))))
  (lookup-frame 0 env))

;; Exercise 5.42
(define (compile-variable exp target linkage ctenv)
  (let ((addr (find-variable exp ctenv))
        (proc '*unassigned*)
        (ref '*unassigned*))
    (if (eq? addr 'not-found)
        (begin (set! proc 'lookup-variable-value)
               (set! ref  exp))
        (begin (set! proc 'lexical-address-lookup)
               (set! ref  addr)))
    (end-with-linkage linkage
                      (make-instruction-sequence '(env) (list target)
                                                 `((assign ,target
                                                           (op ,proc)
                                                           (const ,ref)
                                                           (reg env)))))))
(define (compile-assignment exp target linkage ctenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next ctenv)))
    (let ((addr (find-variable var ctenv))
          (proc '*unassigned*)
          (ref '*unassigned*))
      (if (eq? addr 'not-found)
          (begin (set! proc 'set-variable-value!)
                 (set! ref  var))
          (begin (set! proc 'lexical-address-set!)
                 (set! ref  addr)))
      (end-with-linkage linkage
                        (preserving '(env)
                                    get-value-code
                                    (make-instruction-sequence '(env val) (list target)
                                                               `((preform (op ,proc)
                                                                          (const ,ref)
                                                                          (reg val)
                                                                          (reg env))
                                                                 (assign ,target (const ok)))))))))

;; Exercise 5.43
;;; Takes from Exercise 4.16
(define (make-assignment var exp)
  (list 'set! var exp))
(define (scan-out-defines body)
  (define (collect seq defs exps)
    (if (null? seq)
        (cons defs exps)
        (if (definition? (car seq))
            (collect (cdr seq) (cons (car seq) defs) exps)
            (collect (cdr seq) defs (cons (car seq) exps)))))
  (let ((pair (collect body '() '())))
    (let ((defs (car pair))
          (exps (cdr pair)))
      (append (map (lambda (def)
                     (make-assignment (definition-variable def)
                                      (definition-value def)))
                   defs)
              exps))))
(define (compile-lambda-body exp proc-entry ctenv)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
                                `(,proc-entry
                                  (assign env
                                          (op compiled-procedure-env)
                                          (reg proc))
                                  (assign env
                                          (op extend-environment)
                                          (const ,formals)
                                          (reg argl)
                                          (reg env))))
     (compile-sequence
      (scan-out-defines (lambda-body exp))
      'val
      'return
      (extend-ctenv formals ctenv)))))

;; Other code generators
(define (compile-definition exp target linkage ctenv)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next ctenv)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))
(define (compile-if exp target linkage ctenv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next ctenv))
            (c-code (compile (if-consequent exp) target consequent-linkage ctenv))
            (a-code (compile (if-alternative exp) target linkage ctenv)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))
(define (compile-lambda exp target linkage ctenv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence '(env) (list target)
                                                     `((assign ,target
                                                               (op make-compiled-procedure)
                                                               (label ,proc-entry)
                                                               (reg env)))))
        (compile-lambda-body exp proc-entry ctenv))
       after-lambda))))
(define (compile-sequence seq target linkage ctenv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage ctenv)
      (preserving
       '(env continue)
       (compile (first-exp seq) target 'next ctenv)
       (compile-sequence (rest-exps seq) target linkage ctenv))))
(define (compile-application exp target linkage ctenv)
  (let ((proc-code (compile (operator exp) 'proc 'next ctenv))
        (operand-codes
         (map (lambda
                  (operand) (compile operand 'val 'next ctenv))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))
