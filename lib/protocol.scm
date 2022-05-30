(define DATA  #x00)
(define RESET #x01)

(define (handle-conn sm input-port output-port)
  (let ((msg (read-bencode input-port)))
    (when msg ;; not eof
      (when (or (not (vector? msg)) (not (eq? (vector-length msg) 2)))
        (error "invalid bencode message"))

      (let ((msg-type (vector-ref msg 0))
            (msg-content (vector-ref msg 1)))
        (switch msg-type
          ((RESET)
           (state-machine-reset! 'mqtt-machine) ;; TODO
           (handle-conn sm input-port output-port))

          ((DATA)
           (unless (string? msg-content)
             (error "unexpected bencode data type"))

           (let*-values (((bv) (string->utf8 msg-content))
                         ((resp has-next?) (state-machine-run sm bv)))
             (write-format resp output-port)
             (when has-next?
               (handle-conn sm input-port output-port)))))))))

(define (sm-server sm host port)
  ;; Default backlog according to tcp-listen documentation.
  (define backlog 100)

  (let ((listener (tcp-listen port backlog host)))
    (loop
      (let*-values (((in out) (tcp-accept listener)))
        (parameterize ((tcp-read-timeout #f)
                       (tcp-write-timeout #f))
          (handle-conn sm in out))

        (close-input-port in)
        (close-output-port out)))))
