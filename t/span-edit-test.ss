;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; Tests for language-neutral UTF-8 byte-span structural edits.

(import :gerbil-parser/language-support/testing
        (only-in :clan/poo/object .ref)
        (only-in "../graph-support/edit.ss"
                 make-span-edit span-edit-insert span-edit-delete span-edit-replace
                 span-edit? span-edit-result? utf8-byte-slice
                 apply-span-edits apply-span-edits/verified))

(deflanguage-parser-tests span-edit-test "Language-neutral SpanEdit algebra"
  (engine)
  (scenario "disjoint replace, insert, and delete apply deterministically in UTF-8 bytes"
    (scope ((src "* TODO draft\u2014v1\n:PROPERTIES:\n:ID: t1\n:END:\n"))
      (scope ((bytes (string->utf8 src))
              ;; "* TODO " = 7 bytes; "draft" = 5 bytes; U+2014 = 3 bytes; "v1" = 2 bytes -> end = 17
              (e1 (span-edit-replace 2 6 "DONE" label: "todo"))
              (e2 (span-edit-insert 17 " :ARCHIVE:" label: "tag")))
        (scope ((out (apply-span-edits src (list e2 e1))))
          (equal (span-edit? e1) #t)
          (equal (utf8-byte-slice bytes 7 17) "draft\u2014v1")
          (equal out "* DONE draft\u2014v1 :ARCHIVE:\n:PROPERTIES:\n:ID: t1\n:END:\n")))))
  (scenario "apply-span-edits/verified invokes verifier and returns validated receipt"
    (scope ((res (apply-span-edits/verified
                  "hello world"
                  (list (span-edit-replace 6 11 "parser" label: "word"))
                  (lambda (s) (list 'parsed (string-length s))))))
      (equal (span-edit-result? res) #t)
      (equal (.ref res 'source) "hello parser")
      (equal (.ref res 'applied-count) 1)
      (equal (.ref res 'byte-delta) 1)
      (equal (.ref res 'verified?) #t)
      (equal (.ref res 'parsed) '(parsed 12))))
  (scenario "fail-closed negative controls reject overlaps, bounds, and verifier failure"
    (equal (parser-exception-matches?
            (lambda () (make-span-edit 5 3 "bad")) true)
           #t)
    (equal (parser-exception-matches?
            (lambda ()
              (apply-span-edits "abcdef"
                                (list (span-edit-replace 1 4 "X" label: "a")
                                      (span-edit-replace 3 5 "Y" label: "b"))))
            true)
           #t)
    (equal (parser-exception-matches?
            (lambda () (apply-span-edits "abc" (list (span-edit-replace 0 10 "X"))))
            true)
           #t)
    (equal (parser-exception-matches?
            (lambda ()
              (apply-span-edits/verified
               "abc"
               (list (span-edit-replace 0 3 "bad"))
               (lambda (_s) (error "re-parse rejected"))))
            true)
           #t)))
