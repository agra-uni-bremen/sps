(define (handle-conn sm input-port output-port)
  (let ((bencode (read-bencode input-port)))
    (when bencode ;; not EOF
      (if (not (string? bencode))
        (error "unexpected bencode type")
        (let*-values (((bv) (string->utf8 bencode))
                      ((resp has-next?) (state-machine-run sm bv)))
          (write-format resp output-port)
          (if has-next?
            (handle-conn sm input-port output-port)))))))

(define (sm-server sm host port)
  ;; Default backlog according to tcp-listen documentation.
  (define backlog 100)

  (let ((listener (tcp-listen port backlog host)))
    (loop
      (let*-values (((in out) (tcp-accept listener)))
        (call-with-current-continuation
          (lambda (k)
            (with-exception-handler
              (lambda (eobj)
                (fprintln (current-error-port)
                          (error-object-message eobj))
                (k #f))
              (lambda ()
                (handle-conn sm in out)))))

        (close-input-port in)
        (close-output-port out)))))
