;;; Complete products for shared publication, independent of a language grammar.
(import (only-in :std/list/list-builder with-list-builder)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact sha256-text)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition
                 make-recognition-node make-recognition-child))
(export window-tokens window-artifact)

(def (window-tokens source)
  (with-list-builder (emit)
    (let loop ((index 0) (offset 0))
      (unless (= index (string-length source))
        (let* ((lexeme (string (string-ref source index)))
               (end (+ offset (u8vector-length (string->utf8 lexeme)))))
          (emit (make-token 'word lexeme offset end))
          (loop (+ index 1) end))))))

(def (window-artifact source tokens)
  (make-success-parse-artifact (sha256-text "shared-window-publication") source tokens
    (make-recognition-node 'Root 0 (u8vector-length (string->utf8 source))
      (map (lambda (token) (make-recognition-child #f token)) tokens))
    (lambda (token) #f)))
