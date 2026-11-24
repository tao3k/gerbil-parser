;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; Tests for language-neutral UTF-8 byte-span structural edits.

(import :std/test
        (only-in :clan/poo/object .ref)
        (only-in "../graph-support/edit.ss"
                 make-span-edit span-edit-insert span-edit-delete span-edit-replace
                 span-edit? span-edit-result? utf8-byte-slice
                 apply-span-edits apply-span-edits/verified))
(export span-edit-test)

(def span-edit-test
  (test-suite "Language-neutral SpanEdit algebra"
    (test-case "disjoint replace, insert, and delete apply deterministically in UTF-8 bytes"
      (let* ((src "* TODO 中文标题\n:PROPERTIES:\n:ID: t1\n:END:\n")
             (bytes (string->utf8 src))
             ;; "* TODO " = 7 bytes; "中文" = 6 bytes; "标题" = 6 bytes
             (e1 (span-edit-replace 2 6 "DONE" label: "todo"))
             (e2 (span-edit-insert 19 " :ARCHIVE:" label: "tag"))
             (out (apply-span-edits src (list e2 e1))))
        (check (span-edit? e1) => #t)
        (check (utf8-byte-slice bytes 7 19) => "中文标题")
        (check out => "* DONE 中文标题 :ARCHIVE:\n:PROPERTIES:\n:ID: t1\n:END:\n")))

    (test-case "apply-span-edits/verified invokes verifier and returns validated receipt"
      (let* ((src "hello world")
             (res (apply-span-edits/verified
                   src
                   (list (span-edit-replace 6 11 "parser" label: "word"))
                   (lambda (s) (list 'parsed (string-length s))))))
        (check (span-edit-result? res) => #t)
        (check (.ref res 'source) => "hello parser")
        (check (.ref res 'applied-count) => 1)
        (check (.ref res 'byte-delta) => 1)
        (check (.ref res 'verified?) => #t)
        (check (.ref res 'parsed) => '(parsed 12))))

    (test-case "fail-closed negative controls reject overlaps, bounds, and verifier failure"
      (check-exception (make-span-edit 5 3 "bad") true)
      (check-exception
       (apply-span-edits "abcdef"
                         (list (span-edit-replace 1 4 "X" label: "a")
                               (span-edit-replace 3 5 "Y" label: "b")))
       true)
      (check-exception
       (apply-span-edits "abc" (list (span-edit-replace 0 10 "X")))
       true)
      (check-exception
       (apply-span-edits/verified
        "abc"
        (list (span-edit-replace 0 3 "bad"))
        (lambda (_s) (error "re-parse rejected")))
       true))))
