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
  (make-sm state end)
  state-machine?
  (state sm-state sm-state-set!)
  (end sm-end-states))

;; Type annotations for accessors of state machine.
(: make-sm (state (list-of symbol) -> (struct State-Machine)))
;;(: state-machine? ((struct State-Machine) -> boolean))
(: sm-state ((struct State-Machine) -> state))
(: sm-end-states ((struct State-Machine) -> (list-of symbol)))

(: end-state? ((struct State-Machine) state -> boolean))
(define (end-state? sm state)
  (member (state-name state) (sm-end-states sm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-state-machine
  (syntax-rules (start end)
    ((define-state-machine NAME (start START) (end ENDS) BODY ...)
     (define-state-machine
       NAME
       (let ((s (get 'state-machines (quote NAME))))
         (if s
           s
           (put! 'state-machines
                 (quote NAME)
                 (make-sm
                   (make-state (quote START) START)
                   (list (quote ENDS))))))
       BODY ...))
    ((define-state-machine NAME SM BODY ...)
     (define (NAME input)
       BODY ...

       (if (end-state? SM (sm-state SM))
         (error "current state is an end state")
         (let-values (((ret new-state) (apply-state (sm-state SM) input)))
           (sm-state-set! SM new-state)
           (values
             ret
             (not (end-state? SM new-state)))))))))

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
  ;; This can obviously be shortend to (sm input).
  (let-values (((ret has-next?) (sm input)))
    (if has-next?
      (values ret #t)
      (values ret #f))))
