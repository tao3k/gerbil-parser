;;; -*- Gerbil -*-
(import (rename-in ./fragments (preserve-host-reference renamed-reference)))
(export run-binding-research)

(def private-value 'use-site)

(def (research-check label actual expected)
  (unless (equal? actual expected)
    (error "binding research mismatch" label actual expected))
  (display "BINDING-CASE-OK: ") (display label) (newline) (force-output))

(def (run-binding-research)
  (research-check 'renamed-definition-reference
    (renamed-reference private-value) 'definition-site)
  (research-check 'reconstructed-use-site-reference
    (reconstruct-host-reference private-value) 'use-site)
  (research-check 'caller-parameter
    (parameter-reference private-value) 'use-site)
  (research-check 'local-shadow-preserves-definition
    (let ((private-value 'local-shadow))
      (renamed-reference private-value)) 'definition-site)
  (research-check 'local-shadow-reconstruction
    (let ((private-value 'local-shadow))
      (reconstruct-host-reference private-value)) 'local-shadow)
  (research-check 'local-shadow-parameter
    (let ((private-value 'local-shadow))
      (parameter-reference private-value)) 'local-shadow)
  (display "BINDING-RESEARCH-OK: 6 checks passed") (newline) (force-output))
