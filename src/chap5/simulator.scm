;; Creates a register that holds a value that can be accessed or changed
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))
;; The following procedures are used to access registers
(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;; Creates a stack whose local state consists of a list of the items on the stack.
;; A stack accepts requests to ~push~ an item onto the stack,
;; to ~pop~ the top item off the stack and return it,
;; and to ~initialize~ the stack to empty.
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else (error "Unknown request: STACK" message))))
    dispatch))
;; The following procedures are used to access stacks:
(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

;; Constructs an object whose local state consists of a stack,
;; an initially empty instruction sequence, a list of operations
;; that initially contians an operation to initialize the stack,
;; and a register table that initially contains two registers,
;; named ~flag~ and ~pc~ (for "program counter").
(define (make-new-machine)
  (let ((pc (make-register 'pc))                           ;; determines the sequencing of instructions
        (flag (make-register 'flag))                       ;; controls branching
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))    ;; instruction execution procedure
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      ;; adds new entries to the register table
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      ;; looks up registers in the table
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      ;; gets that instruction, executes it be calling
      ;; the instruction execution procedure, and
      ;; repeats this cycle until there are no more
      ;; instructions to execute
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
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))

;; Looks up the register with a given name in a given machine
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst  proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE"
               label-name))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;; The syntax of ~reg~, ~label~, and ~const~ expressions is determined by
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

;; Generates execution procedures to produce values for these expressions during the simulation
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label
                       labels
                       (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

;; The syntax of operation expressions is determined by
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

;; Looking up the operation name in the operation table for the machine
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

;; Produces an execution procedure for an "operation expression"--a list containing
;; the operation and operand expressions from the instruction
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; handles ~assign~ instructions
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()    ;; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

;; handle ~test~ instructions
(define (test-condition test-instruction)
  (cdr test-instruction))
;; Extracts the expression that specifies the condition to be tested
;; and generates an execution procedure for it.
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

;; handle ~branch~ instructions
(define (branch-dest branch-instruction)
  (cadr branch-instruction))
;; Checks the contents of the ~flag~ register and either sets the contents of the ~pc~
;; to the branch destination (if the branch is taken) or else just advances the ~pc~
;; (if the branch is not taken).
;; Notice that the indicated destination in a ~branch~ instruction must be a label,
;; and the ~make-branch~ procedure enforces this.
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label
                labels
                (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

;; handle ~goto~ instructions
(define (goto-dest goto-instruction)
  (cadr goto-instruction))
;; A ~goto~ instruction is similar to a branch, except that the destination
;; may be specified either as a label or as a register, and there is no condition
;; to check
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label
                         labels
                         (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register
                       machine
                       (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

;; The stack instructions ~save~ and ~restore~ simply use the stack with
;; the designated register and advance the ~pc~
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

;; Generates an execution procedure for the action to be performed.
(define (perform-action inst) (cdr inst))
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda () (action-proc) (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

;; Dispatches on the type of instruction to generate the appropriate execution procedure.
(define (make-execution-procedure
         inst labels machine pc flag stack ops)
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
           (make-save inst machine stack pc))
          ((eq? inst-type 'restore)
           (make-restore inst machine stack pc))
          ((eq? inst-type 'perform)
           (make-perform inst machine labels ops pc))
          (else
           (error "Unknown instruction type: ASSEMBLE"
                  inst)))))

;; Takes as arguments a list ~text~ (the sequence of controller instruction expressions)
;; and a ~receive~ procedure.
;; ~receive~ will be called with two values:
;; (1) a list ~insts~ of instruction data structures, each containing an instruction from ~text~
;; (2) a table called ~labels~, which associates each label from ~text~ with the position
;;     in the list ~insts~ that the label designates.
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

;; Modifies the instruction list, which initially contains only the text of the instructions,
;; to include the corresponding execution procedures
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst)
         labels machine pc flag stack ops)))
     insts)))

;; The assemble procedure is the main entry to the assembler.
;; It takes the controller text and the machine model as arguments
;; and returns the instruction sequence to be stored in the model.
(define (assemble controller-text machine)
  (extract-labels
   controller-text
   (lambda (insts labels)
     (update-insts! insts labels machine)
     insts)))

;; Construsts and returns a model of the machine
;; with the given registers, operations, and controller.
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; Stores a value in a simulated register in the given machine.
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)

;; Returns the contents of a simulated register in the given machine.
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

;; Simulates the execution of the given machine,
;; starting from the beginning of the controller sequence
;; and stopping when it reaches the end of the sequence.
(define (start machine) (machine 'start))
