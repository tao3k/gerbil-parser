;;; Shared LR leaf vocabulary. Phase-specific action structure and index bounds
;;; remain with the compiler producer and runtime selector respectively.
(import (only-in :clan/poo/mop element?)
        (only-in :clan/poo/number Integer))
(export lr-terminal? lr-precedence?)

(def (lr-terminal? value)
  (and (list? value) (pair? value) (eq? (car value) 'terminal)
       (pair? (cdr value))
       (case (cadr value)
         ((eof) (= (length value) 2))
         ((token) (and (= (length value) 3) (symbol? (caddr value))))
         ((literal layout-start layout-next)
          (and (= (length value) 3) (string? (caddr value))))
         (else #f))))
;; Runtime metadata uses native exact Integer. The compiler explicitly supplies
;; its historical integer? predicate; sharing syntax must not widen/narrow a
;; phase's scalar domain or silently change authored grammar compatibility.
(def (exact-rank? value) (element? Integer value))
(def (lr-precedence? value (rank? exact-rank?))
  (or (not value)
      (and (list? value) (= (length value) 2)
           (memq (car value) '(none left right dynamic))
           (rank? (cadr value)))))
