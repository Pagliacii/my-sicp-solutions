(load "compiler")

(define (compile-and-assemble exp)
  (assemble
   (statements
    (compile exp 'val 'return))
   rcep-eceval))

(define rcep-eceval-regs '(exp env proc val argl continue))

(define rcep-eceval-operations
  (append
   eceval-operations
   (list (list 'compile-and-assemble compile-and-assemble))))

(define rcep-eceval-instructions
  '(read-compile-execute-print-loop
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";; RECP-EC-EVAL input: "))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-results))
      (assign val (op compile-and-assemble) (reg exp))
      (goto (reg val))
    print-results
      (perform (op print-stack-statistics))
      (perform (op announce-output) (const ";; RECP-EC-EVAL value: "))
      (perform (op user-print) (reg val))
      (goto (label read-compile-execute-print-loop))))

;; A register machine that performs a read-compile-execute-print loop.
;; That is, the machine should run a loop that reads an expression,
;; compiles it, assembles and executes the resulting code, and prints the result.
(define rcep-eceval
  (make-machine
   rcep-eceval-regs
   rcep-eceval-operations
   rcep-eceval-instructions))

(define (start-rcep)
  (set! the-global-environment (setup-environment))
  (start rcep-eceval))
