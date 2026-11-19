#!/usr/bin/env gxi
(import (only-in :clan/poo/object .get)
        (only-in :std/build-script defbuild-script)
        (only-in :tao3k/yuanxu/module-system/building/products current-build-module-roots)
        (only-in "./languages-init.ss" gerbil-parser.languages-init.exports))

(defbuild-script
  (parameterize ((current-build-module-roots '(hcl)))
    (.get gerbil-parser.languages-init.exports std-make-spec)))
