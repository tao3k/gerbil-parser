;;; -*- Gerbil -*-
;;; One Scheme algorithm is executed in tests and compiled for Rowan.

(import (only-in :gerbil-parser/language-support deflanguage-grammar)
        (only-in :gerbil-parser/src/compiler/event-strategy-aot
                 define-line-event-parser event-node event-token
                 line-starts-with?))
(export event-lines-language-grammar parse-event-lines parse_event_lines)

(deflanguage-grammar event-lines
  (identity "event-lines" "v1" "event-lines.v1")
  (syntax-kinds
   (Document node (line))
   (Heading node (line))
   (Text node (line))
   (Line token (text)))
  (terminals (line Line))
  (lexical-rules (line (line)))
  (rules
   (document
    (alias Document (repeat (field line (reference text-line)))))
   (text-line (alias Text (field line (token line)))))
  (extras)
  (keywords)
  (parser-entrypoints (document parse pure))
  (recoveries)
  (conflicts reject)
  (case-insensitive #f)
  (flow (source lexical) (lexical cst)))

(define-line-event-parser
  parse-event-lines parse_event_lines event-lines-language-grammar Document
  source (line start end)
  (if (line-starts-with? line "* ")
    (event-node Heading (event-token Line start end))
    (event-node Text (event-token Line start end))))
