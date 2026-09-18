#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Downstream native language-pack acceptance entrypoint.

(import (only-in :std/test run-tests!)
        (only-in :gerbil-parser/t/fixtures/rust-rowan-downstream/languages/records/v1/parser-test
                 records-v1-parser-tests))

(run-tests! records-v1-parser-tests)
