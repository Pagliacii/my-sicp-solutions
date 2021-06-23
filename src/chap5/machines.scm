;; A model of the GCD machine of Section 5.1.1
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)))

;; A model of the recursive expt machine of Exercise 5.4
(define recr-expt-machine
  (make-machine
   '(b n continue val)
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'read read))
   '(controller
     (assign b (op read))
     (assign n (op read))
     (assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore n)
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))

;; A model of the iterative expt machine of Exercise 5.4
(define iter-expt-machine
  (make-machine
   '(b n c p)
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'read read))
   '(controller
     (assign b (op read))
     (assign n (op read))
     (assign c (reg n))
     (assign p (const 1))
     expt-iter
     (test (op =) (reg c) (const 0))
     (branch (label expt-done))
     (assign c (op -) (reg c) (const 1))
     (assign p (op *) (reg b) (reg p))
     (goto (label expt-iter))
     expt-done)))

;; A model of the test machine of Exercise 5.11
(define test-restore-machine
  (make-machine
   '(x y)
   '()
   '(controller
     (assign x (const 1))
     (assign y (const 2))
     (save y)
     (save x)
     (restore y)
     done)))

;; A model of the factorial machine of Exercise 5.14
(define factorial-machine
  (make-machine
   '(continue n val)
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'read read)
         (list 'print (lambda (val) (newline) (display val) (newline))))
   '(controller
     entry-point
     (perform (op print) (const "Please enter a number:"))
     (assign n (op read))
     (assign continue (label fact-done))    ;set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ;val now contains n(n - 1)!
     (goto (reg continue))                   ;return to caller
     base-case
     (assign val (const 1))                  ;base case: 1! = 1
     (goto (reg continue))                   ;return to caller
     fact-done
     (perform (op print) (reg val))
     (perform (op print-stack-statistics))
     (perform (op initialize-stack))
     (goto (label entry-point)))))

;; Test machine tracing
(define (test-tracing)
  (gcd-machine 'trace-on)
  (set-register-contents! gcd-machine 'a 1024)
  (set-register-contents! gcd-machine 'b 127)
  (start gcd-machine)
  (gcd-machine 'trace-off)
  (get-register-contents gcd-machine 'a))

;; Test register tracing
(define (test-reg-tracing)
  (trace-register gcd-machine 'a)
  (set-register-contents! gcd-machine 'a 1024)
  (set-register-contents! gcd-machine 'b 127)
  (start gcd-machine)
  (stop-tracing gcd-machine 'a)
  (get-register-contents gcd-machine 'a))

;; Test breakpoint
(define (test-breakpoint-at-gcd)
  (set-breakpoint gcd-machine 'test-b 4)
  (set-register-contents! gcd-machine 'a 1024)
  (set-register-contents! gcd-machine 'b 127)
  (start gcd-machine))

;; A model of the count-leaves machine of Exercise 5.21 Part a
(define count-leaves-machine
  (make-machine
   '(continue t tree val)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?)
         (list 'car car) (list 'cdr cdr))
   '(controller
       (assign continue (label count-done))
     count-loop
       (test (op null?) (reg tree))
       (branch (label base-case))
       (test (op pair?) (reg tree))
       (branch (label else-clause))
       (assign val (const 1))
       (goto (reg continue))
    else-clause
       (save continue)
       (assign continue (label aftercount-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label count-loop))
     aftercount-car
       (restore tree)
       (assign tree (op cdr) (reg tree))
       (assign continue (label aftercount-cdr))
       (save val)
       (goto (label count-loop))
     aftercount-cdr
       (assign t (reg val))
       (restore val)
       (assign val (op +) (reg t) (reg val))
       (restore continue)
       (goto (reg continue))
     base-case
       (assign val (const 0))
       (goto (reg continue))
     count-done)))

;; A model of the count-leaves machine of Exercise 5.21 Part b
(define count-leaves-with-counter-machine
  (make-machine
   '(continue n tree val)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?)
         (list 'car car) (list 'cdr cdr))
   '(controller
       (assign continue (label count-done))
       (assign n (const 0))
     count-iter
       (test (op null?) (reg tree))
       (branch (label base-case))
       (test (op pair?) (reg tree))
       (branch (label else-clause))
       (assign val (op +) (reg n) (const 1))
       (goto (reg continue))
     else-clause
       (save continue)
       (assign continue (label aftercount-inner))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label count-iter))
     aftercount-inner
       (restore tree)
       (assign tree (op cdr) (reg tree))
       (assign n (reg val))
       (assign continue (label aftercount-outter))
       (goto (label count-iter))
     aftercount-outter
       (restore continue)
       (goto (reg continue))
     base-case
       (assign val (reg n))
       (goto (reg continue))
     count-done)))

;; A model of the append machine of Exercise 5.22
(define append-machine
  (make-machine
   '(continue val x y)
   (list (list 'null? null?)
         (list 'cons cons)
         (list 'car car)
         (list 'cdr cdr))
   '(controller
       (assign continue (label append-done))
     append-loop
       (test (op null?) (reg x))
       (branch (label base-case))
       (save x)
       (assign x (op cdr) (reg x))
       (save continue)
       (assign continue (label afterappend-cdr))
       (goto (label append-loop))
     afterappend-cdr
       (restore continue)
       (restore x)
       (assign x (op car) (reg x))
       (assign val (op cons) (reg x) (reg val))
       (goto (reg continue))
     base-case
       (assign val (reg y))
       (goto (reg continue))
     append-done)))

;; A model of the append! machine of Exercise 5.22
(define append!-machine
  (make-machine
   '(x y iter-x cdr-x)
   (list (list 'null? null?)
         (list 'cdr cdr)
         (list 'set-cdr! set-cdr!))
   '(controller
       (assign iter-x (reg x))
     last-pair
       (assign cdr-x (op cdr) (reg iter-x))
       (test (op null?) (reg cdr-x))
       (branch (label append))
       (assign iter-x (reg cdr-x))
       (goto (label last-pair))
     append
       (perform (op set-cdr!) (reg iter-x) (reg y))
     done)))

;; A model of the Fibonacci machine of Figure 5.12
(define fibonacci-machine
  (make-machine
   '(continue n val)
   (list (list 'print (lambda (s) (newline) (display s)))
         (list 'read read)
         (list 'eq? eq?)
         (list 'noninteger? (lambda (n) (not (integer? n))))
         (list '< <)
         (list '- -)
         (list '+ +))
   '(controller
       (perform (op print) (const "Please enter a number or 'q' for quit: "))
       (assign n (op read))
       (test (op eq?) (reg n) (const q))
       (branch (label done))
       (test (op noninteger?) (reg n))
       (branch (label controller))
       (assign continue (label fib-done))
     fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))
       ;; set up to compute Fib(n-1)
       (save continue)
       (assign continue (label afterfib-n-1))
       (save n)                 ; save old value of n
       (assign n (op -) (reg n) (const 1))    ; clobber n to n-1
       (goto (label fib-loop))  ; perform recursive call
     afterfib-n-1     ; upon return, val contains Fib(n-1)
       (restore n)
       (restore continue)
       ;; set up to compute Fib(n-2)
       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val)               ; save Fib(n-1)
       (goto (label fib-loop))
     afterfib-n-2     ; upon return, val contains Fib(n-2)
       (assign n (reg val))     ; n now contains Fib(n-2)
       (restore val)            ; val now contains Fib(n-1)
       (restore continue)
       (assign val              ; Fib(n-1) + Fib(n-2)
               (op +) (reg val) (reg n))
       (goto (reg continue))    ; return to caller, answer is in val
     immediate-answer
       (assign val (reg n))     ; base case: Fib(n) = n
       (goto (reg continue))
     fib-done
       (perform (op print-stack-statistics))
       (perform (op print) (reg val))
       (perform (op initialize-stack))
       (goto (label controller))
     done)))
