;;; Product language keeps common declaration syntax independent of semantic engines.
(.def Value port: 5440)
(unless (= (.get Value port) 5440) (error "Parser lost common POO bindings"))
;; Each numeric phase is compiled from the same inert public declaration stub.
;; No implementation module or arbitrary suffix receives that exemption.
(def (public-declaration-code? name)
  (def base "yuanxu/module-system/selection/init-syntax")
  (or (equal? name base)
      (and (string-prefix? (string-append base "~") name)
           (let* ((section (substring name (+ 1 (string-length base)) (string-length name)))
                  (phase (string->number section)))
             (and (fixnum? phase) (>= phase 0)
                  (equal? section (number->string phase)))))))
(def (forbidden-prelude-module? name)
  (or (string-prefix? "gerbil-parser/src/" name)
      (and (string-prefix? "yuanxu/module-system/" name)
           (not (public-declaration-code? name)))))
(def unexpected-prelude-modules
  (map car (filter (lambda (row) (forbidden-prelude-module? (car row))) (list-modules))))
(unless (null? unexpected-prelude-modules)
  (error "Parser prelude eagerly loaded an engine" unexpected-prelude-modules))
(begin-syntax
  (begin-syntax (def fixture-port 5440))
  (defsyntax (literal stx) (datum->syntax (stx-car stx) fixture-port))
  (def expected (literal)))
(defsyntax (expected-literal stx) (datum->syntax (stx-car stx) expected))
(unless (= (.get Value port) (expected-literal)) (error "Parser lost macro phases"))
(displayln "PARSER-PRELUDE-INERT-OK")
