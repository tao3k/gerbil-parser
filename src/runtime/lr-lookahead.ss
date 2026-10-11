;;; -*- Gerbil -*-
;;; Grammar-owned literal identities, invocation-owned lookahead classification.
(import (only-in ./lr-action-index lookup-action-entry lr-action-row-eof lr-action-row-tokens
                 lr-action-row-casefold-literals)
        (only-in ./scan literal-trie-terminal literal-trie-child)
        (only-in ./token token-lexeme token-kind))
(export prepare-lr-lookahead-selector)

(defstruct lr-lookahead-row (literals tokens eof) final: #t)

;;; Keep the first original entry, including duplicate literals and forks.
;;; Interned literal strings are private keys, never published as token text.
(def (identity-index entries)
  (if (<= (length entries) 8) entries
    (let (index (make-table test: eq?))
      (for-each (lambda (entry)
                  (unless (table-ref index (car entry) #f)
                    (table-set! index (car entry) (cdr entry)))) entries)
      index)))
(def (identity-ref index key)
  (and key
       (if (or (pair? index) (null? index))
         (let (entry (assq key index)) (and entry (cdr entry)))
         (table-ref index key #f))))

;;; Reuse the prepared grammar trie. ASCII misses allocate no normalized text;
;;; Unicode retains the original whole-string conversion semantics.
(def (folded-literal trie source)
  (and trie
       (let follow ((node trie) (index 0))
         (if (= index (string-length source)) (literal-trie-terminal node)
           (let (code (char->integer (string-ref source index)))
             (if (>= code 128) (string-upcase source)
               (let (child (literal-trie-child node
                            (integer->char (if (<= 97 code 122) (- code 32) code))))
                 (and child (follow child (+ index 1))))))))))

(def (prepare-lr-lookahead-selector rows ordinary case-insensitive?)
  (let ((vocabulary (make-table test: eqv?))
        (trie (and case-insensitive? (positive? (vector-length ordinary))
                   (lr-action-row-casefold-literals (vector-ref ordinary 0))))
        (prepared (make-vector (vector-length rows))))
    (def (intern literal)
      (let* ((width (string-length literal))
             (bucket (or (table-ref vocabulary width #f)
                         (let (created (make-table test: equal?))
                           (table-set! vocabulary width created) created))))
        (or (table-ref bucket literal #f)
            (let (owned (string-copy literal))
              (table-set! bucket owned owned) owned))))
    ;; A long identifier whose width is absent from the grammar needs no
    ;; whole-string hash. Do not allocate a max-width padded vocabulary.
    (def (classify literal)
      (let (bucket (table-ref vocabulary (string-length literal) #f))
        (and bucket (table-ref bucket literal #f))))
    (let prepare ((state 0))
      (when (< state (vector-length rows))
        (let* ((row (vector-ref ordinary state))
               (literals
                (filter-map
                 (lambda (entry)
                   (let (terminal (car entry))
                     (and (eq? (cadr terminal) 'literal)
                          (cons (intern (caddr terminal)) entry))))
                 (vector-ref rows state))))
          (vector-set! prepared state
            (make-lr-lookahead-row (identity-index literals)
                                  (lr-action-row-tokens row) (lr-action-row-eof row))))
        (prepare (+ state 1))))
    ;; Only the closed deterministic executor admits this cache. A fresh
    ;; invocation owns it; checkpoints, GLR and installed arbitrary steps do
    ;; not share mutable classification state through the prepared runtime.
    (lambda ()
      (let ((remaining #f) (exact #f) (folded #f) (folded? #f))
        (lambda (state tokens)
          (let (row (vector-ref prepared state))
            (if (null? tokens) (lr-lookahead-row-eof row)
              (let* ((token (car tokens)) (lexeme (token-lexeme token)))
                (unless (eq? remaining tokens)
                  (set! remaining tokens)
                  (set! exact (classify lexeme))
                  (set! folded #f)
                  (set! folded? #f))
                (or (identity-ref (lr-lookahead-row-literals row) exact)
                    (and case-insensitive?
                         (begin
                           (unless folded?
                             (set! folded
                               (let (literal (folded-literal trie lexeme))
                                 (and literal (classify literal))))
                             (set! folded? #t))
                           (identity-ref (lr-lookahead-row-literals row) folded)))
                    (lookup-action-entry (lr-lookahead-row-tokens row) (token-kind token)))))))))))
