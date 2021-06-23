(load "simulator.scm")

(define (make-save inst machine pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (stack (get-stack machine reg-name)))
    (lambda ()
      (push stack (cons reg-name (get-contents reg)))
      (advance-pc pc))))
(define (make-restore inst machine pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (stack (get-stack machine reg-name)))
    (lambda ()
      (let ((top (pop stack)))
        (if (eq? (car top) reg-name)
            (begin (set-contents! reg (cdr top))
                   (advance-pc pc))
            (error "No matched name: STACK" reg-name))))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stacks (list (list 'global (make-stack))))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda ()
                         (for-each
                          (lambda (stack) (stack 'initialize))
                          stacks)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin (set! register-table
                         (cons (list name (make-register name))
                               register-table))
                   (set! stacks (cons (list name (make-stack)) stacks))))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (lookup-stack name)
        (let ((stack (assoc name stacks)))
          (if stack
              (cadr stack)
              (assoc 'global stacks))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stacks) stacks)
              ((eq? message 'get-stack) lookup-stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

(define (get-stack machine name)
  ((machine 'get-stack) name))

(define (make-execution-procedure
         inst labels machine pc flag ops)
  (let ((inst-type (car inst)))
    (cond ((eq? inst-type 'assign)
           (make-assign inst machine labels ops pc))
          ((eq? inst-type 'test)
           (make-test inst machine labels ops flag pc))
          ((eq? inst-type 'branch)
           (make-branch inst machine labels flag pc))
          ((eq? inst-type 'goto)
           (make-goto inst machine labels pc))
          ((eq? inst-type 'save)
           (make-save inst machine pc))
          ((eq? inst-type 'restore)
           (make-restore inst machine pc))
          ((eq? inst-type 'perform)
           (make-perform inst machine labels ops pc))
          ((eq? inst-type 'add)
           (make-add inst machine pc))
          (else
           (error "Unknown instruction type: ASSEMBLE"
                  inst)))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag ops)))
     insts)))