;;; -*- Gerbil -*-
;;; Closed tokenizer and balanced-token traversal for ANTLR4 grammar sources.
;;; Optimization boundary: scanners carry character and delimiter state in
;;; tail calls so they preserve exact source offsets without allocating suffix
;;; substrings or reparsing already admitted input.

(export token-kind
        token-value
        scan-antlr4-tokens
        identifier-token?
        punctuation-token?
        antlr4-tokens->datum
        antlr4-tokens-from-datum
        skip-balanced
        collect-rule-expression)

;;; ANTLR4 source tokens are a named internal protocol, not anonymous pairs;
;;; the compatibility selectors below keep parser call sites stable.
;; : (forall (k v) (-> k v (Antlr4Token k v)))
(defstruct antlr4-token (kind value) transparent: #t)

;; : (forall (k v) (-> k v (Antlr4Token k v)))
(def (token kind value)
  (make-antlr4-token kind value))

;; : (forall (k v) (-> (Antlr4Token k v) k))
(def (token-kind value)
  (antlr4-token-kind value))

;; : (forall (k v) (-> (Antlr4Token k v) v))
(def (token-value value)
  (antlr4-token-value value))

;; : (-> (List Antlr4Token) (List Datum))
(def (antlr4-tokens->datum tokens)
  (map (lambda (input-token)
         (list (token-kind input-token) (token-value input-token)))
       tokens))

;; : (-> (List Datum) (List Antlr4Token))
(def (antlr4-tokens-from-datum rows)
  (map (lambda (row)
         (unless (and (list? row) (= (length row) 2))
           (error "invalid materialized ANTLR4 token" row))
         (token (car row) (cadr row)))
       rows))

;; : (-> Char Boolean)
(def (antlr4-space? ch)
  (char-whitespace? ch))

;; : (-> Char Boolean)
(def (antlr4-identifier-start? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

;; : (-> Char Boolean)
(def (antlr4-identifier-rest? ch)
  (or (antlr4-identifier-start? ch) (char-numeric? ch)))

;; : (-> String Integer String Boolean)
(def (literal-at? source offset literal)
  (let ((source-length (string-length source))
        (literal-length (string-length literal)))
    (and (<= (+ offset literal-length) source-length)
         (string=? (substring source offset (+ offset literal-length))
                   literal))))

;; : (-> String Integer Char Boolean)
(def (source-character=? source offset character)
  (char=? (string-ref source offset) character))

;; : (-> String Integer Integer Boolean)
(def (line-end? source cursor length)
  (or (= cursor length) (source-character=? source cursor #\newline)))

;; : (-> String Integer Boolean)
(def (source-space? source offset)
  (antlr4-space? (string-ref source offset)))

;; : (-> String Integer Boolean)
(def (source-identifier-start? source offset)
  (antlr4-identifier-start? (string-ref source offset)))

;; : (-> String Integer Integer Boolean)
(def (source-identifier-rest? source offset length)
  (and (< offset length)
       (antlr4-identifier-rest? (string-ref source offset))))

;; : (-> Symbol String Integer Integer Antlr4Token)
(def (sliced-token kind source start end)
  (token kind (substring source start end)))

;; : (-> String Integer Antlr4Token)
(def (punctuation-token-at source offset)
  (token 'punctuation (string (string-ref source offset))))

;; : (-> String Integer Integer Integer)
(def (scan-until-line-end-from source cursor length)
  (if (line-end? source cursor length)
    cursor
    (scan-until-line-end-from source (+ cursor 1) length)))

;; : (-> String Integer Integer)
(def (scan-until-line-end source offset)
  (scan-until-line-end-from source offset (string-length source)))

;; : (-> String Integer Integer Integer)
(def (scan-block-comment-end-from source offset cursor length)
  (cond
   ((>= cursor length)
    (error "unterminated ANTLR4 block comment" offset))
   ((literal-at? source cursor "*/") (+ cursor 2))
   (else
    (scan-block-comment-end-from source offset (+ cursor 1) length))))

;; : (-> String Integer Integer)
(def (scan-block-comment-end source offset)
  (scan-block-comment-end-from
   source offset (+ offset 2) (string-length source)))

;; : (-> String Integer Integer Integer Char String Boolean Integer)
(def (scan-delimited-end-from
      source offset cursor length delimiter owner escaped?)
  (cond
   ((>= cursor length)
    (error "unterminated ANTLR4 delimited token" owner offset))
   (escaped?
    (scan-delimited-end-from
     source offset (+ cursor 1) length delimiter owner #f))
   ((source-character=? source cursor #\\)
    (scan-delimited-end-from
     source offset (+ cursor 1) length delimiter owner #t))
   ((source-character=? source cursor delimiter) (+ cursor 1))
   (else
    (scan-delimited-end-from
     source offset (+ cursor 1) length delimiter owner #f))))

;; : (-> String Integer Char String Integer)
(def (scan-delimited-end source offset delimiter owner)
  (scan-delimited-end-from
   source offset (+ offset 1) (string-length source) delimiter owner #f))

;; : (-> String Integer Integer Integer)
(def (scan-identifier-end source end length)
  (if (source-identifier-rest? source end length)
    (scan-identifier-end source (+ end 1) length)
    end))

;; : (-> String Integer Integer (List Antlr4Token) (List Antlr4Token))
(def (scan-antlr4-tokens-from source length offset tokens)
  (cond
   ((= offset length) (reverse tokens))
   ((source-space? source offset)
    (scan-antlr4-tokens-from source length (+ offset 1) tokens))
   ((literal-at? source offset "//")
    (scan-antlr4-tokens-from
     source length (scan-until-line-end source (+ offset 2)) tokens))
   ((literal-at? source offset "/*")
    (scan-antlr4-tokens-from
     source length (scan-block-comment-end source offset) tokens))
   ((source-character=? source offset #\')
    (let (end (scan-delimited-end source offset #\' 'literal))
      (scan-antlr4-tokens-from
       source length end
       (cons (sliced-token 'literal source (+ offset 1) (- end 1))
             tokens))))
   ((source-character=? source offset #\[)
    (let (end (scan-delimited-end source offset #\] 'character-class))
      (scan-antlr4-tokens-from
       source length end
       (cons (sliced-token 'character-class source offset end)
             tokens))))
   ((source-identifier-start? source offset)
    (let (end (scan-identifier-end source (+ offset 1) length))
      (scan-antlr4-tokens-from
       source length end
       (cons (sliced-token 'identifier source offset end) tokens))))
   ((literal-at? source offset "->")
    (scan-antlr4-tokens-from
     source length (+ offset 2) (cons (token 'punctuation "->") tokens)))
   ((literal-at? source offset "+=")
    (scan-antlr4-tokens-from
     source length (+ offset 2) (cons (token 'punctuation "+=") tokens)))
   (else
    (scan-antlr4-tokens-from
     source length (+ offset 1)
     (cons (punctuation-token-at source offset)
           tokens)))))

;; scan-antlr4-tokens
;;   : (-> String (List Antlr4Token))
;;   | doc m%
;;       Tokenizes the closed ANTLR4 grammar-source subset used by admission.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-antlr4-tokens "grammar Tiny; rule : 'x';")
;;       ;; => ordered identifier, punctuation, and literal token pairs
;;       ```
;;       Result: comments and whitespace are omitted; source order is retained.
;;     %
(def (scan-antlr4-tokens source)
  (scan-antlr4-tokens-from source (string-length source) 0 '()))

;; : (-> Antlr4Token String Boolean)
(def (identifier-token? value wanted)
  (and (antlr4-token? value)
       (eq? (token-kind value) 'identifier)
       (string=? (token-value value) wanted)))

;; : (-> Antlr4Token String Boolean)
(def (punctuation-token? value wanted)
  (and (antlr4-token? value)
       (eq? (token-kind value) 'punctuation)
       (string=? (token-value value) wanted)))

;;; Balanced traversal is parser state, not a collection transform: depth
;;; controls the exact suffix returned to the declaration parser.
;; : (-> (List Antlr4Token) Integer String String (List Antlr4Token))
(def (skip-balanced-from rest depth opening closing)
  (unless (pair? rest)
    (error "unterminated ANTLR4 balanced block" opening closing))
  (cond
   ((punctuation-token? (car rest) opening)
    (skip-balanced-from (cdr rest) (+ depth 1) opening closing))
   ((and (= depth 1) (punctuation-token? (car rest) closing))
    (cdr rest))
   ((punctuation-token? (car rest) closing)
    (skip-balanced-from (cdr rest) (- depth 1) opening closing))
   (else
    (skip-balanced-from (cdr rest) depth opening closing))))

;; skip-balanced
;;   : (-> (List Antlr4Token) String String (List Antlr4Token))
;;   | doc m%
;;       Consumes one balanced token block and returns its exact suffix.
;;
;;       # Examples
;;
;;       ```scheme
;;       (skip-balanced (list (token 'punctuation "{")
;;                            (token 'punctuation "}")) "{" "}")
;;       ;; => ()
;;       ```
;;       Result: malformed or unterminated nesting fails closed.
;;     %
(def (skip-balanced tokens opening closing)
  (unless (and (pair? tokens) (punctuation-token? (car tokens) opening))
    (error "ANTLR4 balanced block expected" opening))
  (skip-balanced-from (cdr tokens) 1 opening closing))

;;; Rule collection is likewise stateful: semicolons terminate only at depth
;;; zero, while the returned suffix remains owned by the top-level parser.
;; : (-> (List Antlr4Token) Integer (List Antlr4Token) (Values (List Antlr4Token) (List Antlr4Token)))
(def (collect-rule-expression-from rest depth found)
  (unless (pair? rest)
    (error "unterminated ANTLR4 rule"))
  (let (current (car rest))
    (cond
     ((and (zero? depth) (punctuation-token? current ";"))
      (values (reverse found) (cdr rest)))
     ((or (punctuation-token? current "(")
          (punctuation-token? current "[")
          (punctuation-token? current "{"))
      (collect-rule-expression-from
       (cdr rest) (+ depth 1) (cons current found)))
     ((or (punctuation-token? current ")")
          (punctuation-token? current "]")
          (punctuation-token? current "}"))
      (when (zero? depth)
        (error "unbalanced ANTLR4 rule expression" (token-value current)))
      (collect-rule-expression-from
       (cdr rest) (- depth 1) (cons current found)))
     (else
      (collect-rule-expression-from (cdr rest) depth (cons current found))))))

;; collect-rule-expression
;;   : (-> (List Antlr4Token) (Values (List Antlr4Token) (List Antlr4Token)))
;;   | doc m%
;;       Splits one rule expression at its first depth-zero semicolon.
;;
;;       # Examples
;;
;;       ```scheme
;;       (collect-rule-expression (list (token 'identifier "x")
;;                                      (token 'punctuation ";")))
;;       ;; => values: expression tokens and empty suffix
;;       ```
;;       Result: nested delimiters remain part of the returned expression.
;;     %
(def (collect-rule-expression tokens)
  (collect-rule-expression-from tokens 0 '()))
