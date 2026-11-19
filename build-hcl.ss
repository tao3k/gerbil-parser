#!/usr/bin/env gxi
(import (only-in :clan/poo/object .call)
        (only-in :std/build-script defbuild-script)
        (only-in "./languages-init.ss" gerbil-parser.languages-init.exports))

(defbuild-script (.call gerbil-parser.languages-init.exports .make-spec-for '(Languages/hcl)))
