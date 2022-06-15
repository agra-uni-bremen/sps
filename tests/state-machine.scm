(define-state-machine sm-simple
  (start initial-state)

  (define-state initial-state
    (-> "initial-state" second-state))

  (define-state second-state
    (-> "second-state" accept-state))

  (define-state accept-state
    (-> '() accept-state)))

(define-state-machine sm-choice
  (start initial-state)

  (define-state (initial-state input)
    (match input
      ("foo" (-> "->foo" foo-state))
      ("bar" (-> "->bar" bar-state))))

  (define-state foo-state
    (-> "foo" end-state))
  (define-state bar-state
    (-> "bar" end-state))

  (define-state end-state))

(define (run-sm sm input)
  (let ((r (sm (car input))))
    (if (> (length input) 1)
      (cons r (run-sm sm (cdr input)))
      (list r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-group "state machine"
  (test "run without choice"
    '("initial-state" "second-state")
    (run-sm sm-simple '(1 2)))

  (test "run with transition choice"
    '("->foo" "foo")
    (run-sm sm-choice '("foo" "foo/bar state"))))
