#!/usr/bin/env gxi
;;; Assemble the qualification module's products through the compiler policy.
(import (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec)
        (only-in "./t/support/qualification/build.ss" qualification-build-spec)
        (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))
(defbuild-script (compiler-build-spec (qualification-build-spec)))
