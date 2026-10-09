;;; Inert native POO scope with a stable, explicit author-owned identity.
(import (only-in :clan/poo/object .o)
        (only-in ./source-event-scope-types source-event-scope?))
(export make-source-event-scope)
(def (make-source-event-scope owner-value)
  (let (scope (.o kind: 'source-event-scope owner: owner-value))
    (unless (source-event-scope? scope)
      (error "invalid native event scope owner" owner-value))
    scope))
