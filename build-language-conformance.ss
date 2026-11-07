#!/usr/bin/env gxi
(import (only-in :std/build-script defbuild-script)
        (only-in :std/source this-source-file)
        (only-in "./t/research/build.ss" conformance-build-spec))

(defbuild-script (conformance-build-spec) optimize: #t)
