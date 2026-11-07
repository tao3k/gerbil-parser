#!/usr/bin/env gxi
;;; Assemble the qualification module using the standard Gerbil builder.
(import
        (only-in "./t/support/qualification/build.ss" qualification-build-spec)
        (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(defbuild-script (qualification-build-spec) optimize: #t)
