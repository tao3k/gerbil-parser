#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional native command product; the library package is built by build.ss.

(import (only-in "./src/build-support/library-publication.ss" publication-build-main)
        (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file))

(defbuild-script (compiler-build-spec '("src/cli"
                   (exe: "src/main" bin: "gparse"))))

(set! main (publication-build-main main))
