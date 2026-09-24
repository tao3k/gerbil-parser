;;; -*- Gerbil -*-
;;; Parser-event algorithms execute in Scheme before AOT and stay AST-backed.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in "event-strategy-fixture.ss"
                 event-lines-language-grammar parse-event-lines parse_event_lines)
        (only-in :gerbil-parser/src/compiler/event-strategy-aot
                 compile-line-event-parser)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-line-event-function-form-digest)
        (only-in "rust-aot-test-syntax.ss" check-rust-event-strategy))
(export event-strategy-test)

(def event-strategy-test
  (test-suite "Scheme parser event strategy AOT"
    (test-case "one Scheme body owns headings and UTF-8 source byte ranges"
      (check (parse-event-lines "* α\r\nbody\n")
             => '((start Document)
                  (start Heading) (token Line 0 6) (finish)
                  (start Text) (token Line 6 11) (finish)
                  (finish)))
      (check (parse-event-lines "")
             => '((start Document) (finish)))
      (check (parse-event-lines "plain\r* β")
             => '((start Document)
                  (start Text) (token Line 0 6) (finish)
                  (start Heading) (token Line 6 10) (finish)
                  (finish))))
    (test-case "AOT is a structured parser function, not source assembly"
      (check-rust-event-strategy parse_event_lines
                                 'parse_event_lines 0)
      (check (string-prefix?
              "sha256:" (rust-line-event-function-form-digest parse_event_lines))
             => #t)
      (check (equal?
              (rust-line-event-function-form-digest parse_event_lines)
              (rust-line-event-function-form-digest
               (compile-line-event-parser
                'parse_event_lines event-lines-language-grammar
                'Document '(line start end)
                '(event-node Text (event-token Line start end)))))
             => #f))
    (test-case "AOT admits only declared event kinds and pure expressions"
      (check-exception
       (compile-line-event-parser
        'invalid event-lines-language-grammar 'Document '(line start end)
        '(event-node Missing (event-token Line start end)))
       true)
      (check-exception
       (compile-line-event-parser
        'invalid event-lines-language-grammar 'Document '(line start end)
        '(begin (display "side effect") (event-token Line start end)))
       true))))
