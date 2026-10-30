;;; -*- Gerbil -*-
;;; Research controls: preserve syntax versus reconstruct from a caller anchor.
(export preserve-host-reference reconstruct-host-reference parameter-reference)

(def private-value 'definition-site)

(defrules preserve-host-reference ()
  ((_ anchor) private-value))

(defsyntax (reconstruct-host-reference stx)
  (syntax-case stx ()
    ((_ anchor)
     (datum->syntax #'anchor (syntax->datum #'private-value)))))

(defrules parameter-reference ()
  ((_ expression) expression))
