;;; Native callback history Suite; CI/development owns execution.
(import :std/test)
(import (only-in :gerbil-parser/src/ffi/language-handles
                register-language! release-language! language-handle-parse
                language-handle-parse/publish language-handle-scan-counts language-handle-count
                language-handle-descriptor)
        (only-in :gerbil-parser/languages/bash/parser bash-source-language))
(def (require! value message) (unless value (error message)))
(def (fails? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def (counts handle)
  (let-values (((scanned reused) (language-handle-scan-counts handle))) (list scanned reused)))
(def initial "cat <<A\nα\nA\necho end\n")
(def nested "echo猫 <<A\nα\nA\necho end\n")
(def outer "unrelated\n")
(export history-commit-test)
(def history-commit-test
  (test-suite "callback history commit admission"
    (test-case "nested, failed, rejected and released histories retain ownership"
(def baseline (language-handle-count))

(for-each
 (lambda (raises?)
  (let ((handle (register-language! bash-source-language))
        (fresh (register-language! bash-source-language)) (committed #f) (inner-payload #f))
   (try
    (language-handle-parse handle (string->utf8 initial))
    (let (publish (lambda (_)
                   (set! inner-payload (language-handle-parse handle (string->utf8 nested)))
                   (set! committed (counts handle))
                   (if raises? (error "outer callback failed after inner commit") #t)))
     (if raises?
       (require! (fails? (lambda () (language-handle-parse/publish handle (string->utf8 outer) publish)))
                 "outer failure swallowed")
       (require! (equal? (language-handle-parse/publish handle (string->utf8 outer) publish)
                        (language-handle-parse fresh (string->utf8 outer))) "outer payload changed")))
    (require! (and (= (car committed) 1) (> (cadr committed) 0)) "inner history not measured")
    (require! (equal? committed (counts handle)) "outer completion overwrote newer history")
    (require! (equal? inner-payload (language-handle-parse fresh (string->utf8 nested))) "inner payload differs from fresh")
    (displayln "HISTORY-NESTED-COMMIT-OK raises=" raises?) (force-output)
    (finally (release-language! handle) (release-language! fresh))))) '(#f #t))

(let (handle (register-language! bash-source-language))
 (try
  (language-handle-parse handle (string->utf8 initial))
  (let (before (counts handle))
   (language-handle-parse/publish handle (string->utf8 outer)
     (lambda (_)
       (require! (fails? (lambda () (language-handle-parse handle #u8(255)))) "invalid nested source accepted")
       #t))
   (require! (equal? before (counts handle)) "superseded outer committed after failed newer attempt"))
  (displayln "HISTORY-FAILED-SUPERSESSION-OK") (force-output)
  (finally (release-language! handle))))

(let (handle (register-language! bash-source-language))
 (try
  (language-handle-parse handle (string->utf8 initial))
  (let (before (counts handle))
   (language-handle-parse/publish handle (string->utf8 outer)
     (lambda (_)
       (language-handle-parse/publish handle (string->utf8 nested) (lambda (_) #f))
       #t))
   (require! (equal? before (counts handle)) "rejected newer publication allowed stale outer commit"))
  (displayln "HISTORY-REJECTED-SUPERSESSION-OK") (force-output)
  (finally (release-language! handle))))

(let ((handle (register-language! bash-source-language))
      (other (register-language! bash-source-language)))
 (try
  (language-handle-parse handle (string->utf8 initial))
  (language-handle-parse/publish handle (string->utf8 outer)
    (lambda (_)
      (language-handle-descriptor handle)
      (language-handle-parse other (string->utf8 nested))
      #t))
  (language-handle-parse other (string->utf8 outer))
  (require! (equal? (counts handle) (counts other)) "unrelated handle or metadata query superseded request")
  (displayln "HISTORY-HANDLE-ISOLATION-OK") (force-output)
  (finally (release-language! handle) (release-language! other))))

(let (handle (register-language! bash-source-language))
  (language-handle-parse/publish handle (string->utf8 initial)
    (lambda (_) (release-language! handle) #t))
  (require! (fails? (lambda () (counts handle))) "released handle retained readable history")
  (displayln "HISTORY-RELEASE-OK") (force-output))
(require! (= baseline (language-handle-count)) "history controls leaked handles")
(displayln "HISTORY-COMMIT-CONTROLS-OK") (force-output)

)))
