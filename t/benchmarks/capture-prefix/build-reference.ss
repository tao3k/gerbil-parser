;;; Normal SDK compiler for an exact Git snapshot and its selected owner set.
(import (only-in :gerbil/compiler compile-module))
(def (main source output (scope "selector-input"))
  (unless (member scope '("selector-input"))
    (error "unknown native reference scope" scope))
  (let ((output (path-expand output))
        (runtime (path-normalize (path-expand "../" (path-directory (path-expand source))))))
    ;; Earlier modules' frozen interfaces must precede shared current inputs.
    (add-load-path! output)
    (for-each
     (lambda (path)
       (displayln "REFERENCE-COMPILE " path) (force-output)
       (compile-module path
         (list output-dir: output optimize: #t generate-ssxi: #t invoke-gsc: #t)))
     (map (lambda (name) (path-expand (string-append name ".ss") runtime))
          '("lr-parser/state" "layout" "lr-parser/prepare" "lr-parser/recognition" "lr-parser/batch"
            "lr-parser/checkpoint" "lr-parser/transfer" "parser")))))
