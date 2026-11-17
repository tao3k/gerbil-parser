#!/usr/bin/env gxi
(import (only-in :clan/poo/object .get)
        (only-in :std/build-script defbuild-script)
        (only-in "./package-spec.ss" gerbil-parser.package.exports))

(defbuild-script (.get gerbil-parser.package.exports std-make-spec))
