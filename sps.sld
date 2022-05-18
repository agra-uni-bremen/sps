(define-library (sps)
  (import (scheme base)
          (scheme write)

          (srfi 1)
          (srfi 151)

          (chicken tcp)
          (chicken type)
          (chicken plist)

          (bencode)
          (sisl))

  (export define-state-machine define-state -> state-machine-run
          sm-server parse-message switch bytevector->number)

  (include "lib/util.scm"
           "lib/state-machine.scm"
           "lib/protocol.scm"
           "lib/parser.scm"))
