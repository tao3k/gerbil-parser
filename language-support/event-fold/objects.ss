(import (only-in :clan/poo/object .def .get)
        (only-in :clan/poo/io module-action)
        (only-in "./types.ss" EventFoldScopeContract))
(export EventFold.)

;; Scope values cross into native callers, so use the compiled owner here.
(def call-prepare
  (module-action "gerbil-parser/src/compiler/event-fold-program"
                 'gerbil-parser/src/compiler/event-fold-program#prepare-event-fold-program))

(.def (EventFold. @ (.get EventFoldScopeContract proto))
  prepare: (lambda args (call-prepare args)))
