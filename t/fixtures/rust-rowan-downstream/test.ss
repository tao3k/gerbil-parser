#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Downstream native language-pack acceptance entrypoint.

(import (only-in :std/test run-tests!)
        (only-in "languages/records/v1/parser-test.ss"
                 records-v1-parser-tests))

(run-tests! records-v1-parser-tests)
