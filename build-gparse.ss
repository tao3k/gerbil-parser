#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional native command product; the library package is built by build.ss.

(import (only-in :std/build-script defbuild-script))

(defbuild-script '((exe: "src/main" bin: "gparse")))
