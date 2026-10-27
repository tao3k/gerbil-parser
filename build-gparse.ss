#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional native command product; the library package is built by build.ss.

(import (only-in "./src/build-support/compiler-profile.ss" compiler-build-spec))
(import (only-in :std/build-script defbuild-script)
        (only-in :std/make pkg-config-libs)
        (only-in :std/source this-source-file))

(defbuild-script
  (compiler-build-spec
    `("src/cli"
      (exe: "src/main" bin: "gparse"
            "-ld-options" ,(pkg-config-libs "openssl")))))
