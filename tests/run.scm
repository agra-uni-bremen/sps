(import r7rs matchable test sisl sps)

(include-relative "state-machine.scm")
(include-relative "parser.scm")

;; Exit with non-zero exit status on test failure.
(test-exit)
