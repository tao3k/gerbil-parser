#!/usr/bin/env gxi
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./src/build-support/compiler-profile.ss" initialize-compiler-build!)
        (only-in "./t/research/build.ss" conformance-build-spec))

(parameterize ((current-output-port (current-error-port)))
  (initialize-compiler-build!))
(defbuild-script (conformance-build-spec) optimize: #t)
