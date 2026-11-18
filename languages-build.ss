#!/usr/bin/env gxi
(import (only-in :clan/poo/object .get)
        (only-in :std/build-script defbuild-script)
        (only-in "./init.ss" gerbil-parser.init.exports))

(defbuild-script (.get gerbil-parser.init.exports Languages __std-make-spec))
