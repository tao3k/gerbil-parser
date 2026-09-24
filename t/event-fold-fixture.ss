;;; -*- Gerbil -*-
;;; One source-owned stateful parser algorithm for Scheme and Rust verification.

(import (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in :gerbil-parser/rust-rowan-event-support
                 define-event-fold-parser))
(export parse-fold-lines parse_fold_lines)

(define-event-fold-parser
  parse-fold-lines parse_fold_lines event-lines-language-grammar Document
  source ((paragraph-open #f))
  ((if (line-starts-with "* ")
       ((if (state paragraph-open)
            ((finish-node) (set-bool paragraph-open (bool #f)))
            ())
        (start-node Heading) (token Line start end) (finish-node))
       ((if (not (state paragraph-open))
            ((start-node Text) (set-bool paragraph-open (bool #t)))
            ())
        (token Line start end))))
  ((if (state paragraph-open) ((finish-node)) ())))
