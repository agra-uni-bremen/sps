(define-library (sps)
  (import (scheme base)

          (srfi 1)
          (srfi 151)

          (chicken type)
          (chicken plist)

          (sisl))

  (export define-state-machine define-state -> state-machine-run)

  (include "lib/util.scm"
           "lib/state-machine.scm"))
