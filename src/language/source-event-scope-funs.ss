;;; Pure deterministic allocation; no runtime name lookup or tree rewrite.
(import (only-in :clan/poo/object .ref)
        (only-in ./source-event-scope-types source-event-scope?))
(export source-event-name)
(def (identifier-part value)
  (apply string-append
    (map (lambda (byte)
           (let (digits (number->string byte 16))
             (if (< byte 16) (string-append "0" digits) digits)))
         (u8vector->list (string->utf8 (symbol->string value))))))
(def (source-event-name scope role)
  (unless (and (source-event-scope? scope) (symbol? role))
    (error "invalid native event scope binding" scope role))
  (string->symbol
    (string-append "source_" (identifier-part (.ref scope 'owner))
                   "_" (identifier-part role))))
