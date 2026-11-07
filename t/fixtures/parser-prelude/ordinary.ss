;;; Product language imports neither grammar compilation nor the Module System.
(.def Value port: 5440)
(unless (= (.get Value port) 5440) (error "Parser lost common POO bindings"))
(when (ormap (lambda (row)
               (or (string-prefix? "gerbil-parser/src/" (car row))
                   (and (string-prefix? "yuanxu/module-system/" (car row))
                        ;; The common prelude imports only this declaration transformer.
                        (not (equal? (car row) "yuanxu/module-system/selection/init-syntax~1")))))
             (list-modules))
  (error "Parser prelude eagerly loaded an engine"))
(begin-syntax
  (begin-syntax (def fixture-port 5440))
  (defsyntax (literal stx) (datum->syntax (stx-car stx) fixture-port))
  (def expected (literal)))
(defsyntax (expected-literal stx) (datum->syntax (stx-car stx) expected))
(unless (= (.get Value port) (expected-literal)) (error "Parser lost macro phases"))
(displayln "PARSER-PRELUDE-INERT-OK")
