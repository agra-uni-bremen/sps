(define-record-type State-Machine-State
  (make-state name proc)
  state?
  (name state-name)
  (proc state-proc))

;; Convenience type alias for states of a state machine.
(define-type state (struct State-Machine-State))

;; Type for state transitions.
(define-type transition (bytevector -> state))

;; Type annotations for accessors of state machine state.
(: make-state (symbol transition -> state))
;;(: state? (state -> boolean))
(: state-name (state -> symbol))
(: state-proc (state -> transition))

(: apply-state (state bytevector -> state))
(define (apply-state state input)
  ((state-proc state) input))

(define-record-type State-Machine
  (make-sm state)
  state-machine?
  (state sm-state sm-state-set!))

;; Type annotations for accessors of state machine.
(: make-sm (state -> (struct State-Machine)))
;;(: state-machine? ((struct State-Machine) -> boolean))
(: sm-state ((struct State-Machine) -> state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-state-machine
  (syntax-rules (start)
    ((define-state-machine NAME (start START) BODY ...)
     (define-state-machine
       NAME
       (let ((s (get 'state-machines (quote NAME))))
         (if s
           s
           (put! 'state-machines
                 (quote NAME)
                 (make-sm
                   (make-state (quote START) START)))))
       BODY ...))
    ((define-state-machine NAME SM BODY ...)
     (define (NAME input)
       BODY ...

       (let-values (((ret new-state) (apply-state (sm-state SM) input)))
         (sm-state-set! SM new-state)
         ret)))))

(define-syntax define-state
  (syntax-rules ()
    ((define-state NAME)
     (define (NAME input)
       (-> '() NAME)))
    ((define-state (NAME ARGV ...) BODY ...)
     (define (NAME ARGV ...)
       BODY ...))
    ((define-state NAME BODY ...)
     (define (NAME input)
       BODY ...))))

(define-syntax ->
  (syntax-rules ()
    ((-> EXPR NEW-STATE)
     (values
       EXPR
       (make-state (quote NEW-STATE) NEW-STATE)))))

(define (state-machine-reset! name)
  (remprop! 'state-machines name))

(define (state-machine-run sm input)
  (sm input))
