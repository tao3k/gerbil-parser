#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Optional native command product; the library package is built by build.ss.

(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./src/cli/build.ss" gparse-build-spec))

(defbuild-script (gparse-build-spec) optimize: #t)
