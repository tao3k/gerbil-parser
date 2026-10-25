#!/usr/bin/env -S timeout --signal=TERM --kill-after=5s 180s gxi
;;; Explicit qualification issuance over the ordinary Core/SDK build owner.
(import (rename-in (only-in "./build.ss" main) (main compile-library))
        (only-in "./src/build-support/library-publication.ss" publication-build-main))
(export main)
(def main (publication-build-main compile-library #f))
