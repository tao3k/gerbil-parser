#!/usr/bin/env gxi
(import (only-in :clan/poo/object .get)
        (only-in :std/build-script defbuild-script)
        (only-in "./languages-init.ss" gerbil-parser.languages-init.exports))

(defbuild-script (.get gerbil-parser.languages-init.exports std-make-spec))
