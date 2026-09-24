;;; -*- Gerbil -*-
;;; One source-owned stateful parser algorithm for Scheme and Rust verification.

(import (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in :gerbil-parser/rust-rowan-event-support
                 define-event-fold-parser))
(export parse-fold-lines parse_fold_lines parse-outline-lines parse_outline_lines)

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

;; The very same bounded Scheme transitions execute here and lower to Rust.
(define-event-fold-parser
  parse-outline-lines parse_outline_lines event-lines-language-grammar Document
  source ((open-levels (uint-stack)))
  ((if (uint-positive? (line-marker-level "*" " "))
       ((close-through open-levels (line-marker-level "*" " "))
        (open-level open-levels (line-marker-level "*" " ") Section)
        (start-node Heading) (token Line start end) (finish-node))
       ((start-node Text) (token Line start end) (finish-node))))
  ((close-all open-levels)))
