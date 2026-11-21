;;; Product language keeps common declaration syntax independent of semantic engines.
(import (only-in :clan/poo/object .def .get))

(.def Value port: 5440)
(unless (= (.get Value port) 5440) (error "Parser lost common POO bindings"))
(def (forbidden-prelude-module? name)
  (or (string-prefix? "gerbil-parser/src/" name)
      (string-prefix? "tao3k/yuanxu/" name)
      (string-prefix? "yuanxu/" name)))
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
