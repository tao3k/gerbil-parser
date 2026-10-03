;;; -*- Gerbil -*-
;;; POO source fragments execute and AOT-lower one event algorithm.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/encoding/json JSONReadOptions string->json)
        (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in :gerbil-parser/src/modules/parser/source-fragment-objects
                 make-source-delimited-fragment make-source-first-split
                 make-source-reference-scan)
        (only-in :gerbil-parser/src/modules/parser/source-fragment-types
                 source-delimited-fragment? source-first-split?
                 source-reference-scan?)
        (only-in :gerbil-parser/src/modules/parser/source-fragment-funs
                 source-delimited-fragment-initial source-delimited-fragment-forms
                 source-first-split-initial source-first-split-forms
                 source-reference-scan-initial source-reference-scan-forms)
        (only-in :gerbil-parser/rust-rowan-event-support
                 run-event-fold event-fold-ir-json))
(export source-fragment-test)

(def fragment (make-source-delimited-fragment "::" 'Line 'source-item 'Line))
(def helpers
  (list (list 'source-segments
              (source-delimited-fragment-initial fragment)
              (source-delimited-fragment-forms fragment))
        '(source-item ()
          ((start-node Text) (token Line start end) (finish-node)))))
(def line-forms '((call-source-helper source-segments start end)))
(def first-split (make-source-first-split "=" 'Line 'source-before 'source-after))
(def split-helpers
  (list (list 'source-split
              (source-first-split-initial first-split)
              (source-first-split-forms first-split))
        '(source-before ()
          ((start-node Text) (token Line start end) (finish-node)))
        '(source-after ()
          ((start-node Heading) (token Line start end) (finish-node)))))
(def split-forms '((call-source-helper source-split start end)))
(def reference-rule
  (make-source-reference-scan '((#\$ . Line) (#\@ . Line))
                              "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789$@#.<>_+-"
                              "remote(" 'Line 'Text 'Line))
(def reference-helpers
  (list (list 'source-references
              (source-reference-scan-initial reference-rule)
              (source-reference-scan-forms reference-rule))))
(def reference-forms '((call-source-helper source-references start end)))

(def source-fragment-test
  (test-suite "POO source fragment event AOT"
    (test-case "admitted delimiter partitions source without copying text"
      (check (source-delimited-fragment? fragment) => #t)
      (check (run-event-fold "a::b::c" 'Document '() line-forms '() helpers)
             => '((start Document)
                  (start Text) (token Line 0 1) (finish)
                  (token Line 1 3)
                  (start Text) (token Line 3 4) (finish)
                  (token Line 4 6)
                  (start Text) (token Line 6 7) (finish)
                  (finish))))
    (test-case "the same policy lowers to typed AOT helper IR"
      (let* ((wire (event-fold-ir-json
                    'parse_source_segments event-lines-language-grammar
                    'Document '() line-forms '() helpers))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (hash-ref ir "schema")
               => "gerbil-scheme-rust.event-function-ir.v1")
        (check (vector-length (hash-ref ir "helpers")) => 2)))
    (test-case "first split composes two source-bounded helpers"
      (check (source-first-split? first-split) => #t)
      (check (run-event-fold "a=b=c" 'Document '() split-forms '()
                             split-helpers)
             => '((start Document)
                  (start Text) (token Line 0 1) (finish)
                  (token Line 1 2)
                  (start Heading) (token Line 2 5) (finish)
                  (finish)))
      (let* ((wire (event-fold-ir-json
                    'parse_first_split event-lines-language-grammar
                    'Document '() split-forms '() split-helpers))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (vector-length (hash-ref ir "helpers")) => 3)))
    (test-case "marker and balanced-call references share one bounded policy"
      (check (source-reference-scan? reference-rule) => #t)
      (check (run-event-fold "$2 remote(x,$1) @3" 'Document '()
                             reference-forms '() reference-helpers)
             => '((start Document)
                  (start Text) (token Line 0 2) (finish)
                  (token Line 2 3)
                  (start Text) (token Line 3 15) (finish)
                  (token Line 15 16)
                  (start Text) (token Line 16 18) (finish)
                  (finish)))
      (let* ((wire (event-fold-ir-json
                    'parse_source_references event-lines-language-grammar
                    'Document '() reference-forms '() reference-helpers))
             (ir (string->json wire
                               (JSONReadOptions object-as-hash: #t
                                                array-as-vector: #t))))
        (check (vector-length (hash-ref ir "helpers")) => 1)))
    (test-case "POO admission rejects unsupported delimiters"
      (check-exception (make-source-delimited-fragment
                        ":::" 'Line 'source-item 'Line)
                       true)
      (check-exception (make-source-reference-scan
                        '((#\$ . Line)) "abc" "remote"
                        'Line 'Text 'Line)
                       true))))
