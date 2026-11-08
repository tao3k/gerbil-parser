;;; Product language keeps common declaration syntax independent of semantic engines.
(.def Value port: 5440)
(unless (= (.get Value port) 5440) (error "Parser lost common POO bindings"))
(when (ormap (lambda (row)
               (let (name (car row))
                 (or (string-prefix? "gerbil-parser/src/" name)
                     ;; Core's public declaration stub is ordinary syntax.
                     ;; Its implementation and all semantic owners stay inert.
                     (and (string-prefix? "yuanxu/module-system/" name)
                          (not (member name
                            '("yuanxu/module-system/selection/init-syntax"
                              "yuanxu/module-system/selection/init-syntax~0")))))))
             (list-modules))
  (error "Parser prelude eagerly loaded an engine"))
(begin-syntax
  (begin-syntax (def fixture-port 5440))
  (defsyntax (literal stx) (datum->syntax (stx-car stx) fixture-port))
  (def expected (literal)))
(defsyntax (expected-literal stx) (datum->syntax (stx-car stx) expected))
(unless (= (.get Value port) (expected-literal)) (error "Parser lost macro phases"))
(displayln "PARSER-PRELUDE-INERT-OK")
