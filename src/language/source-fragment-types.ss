;;; -*- Gerbil -*-
;;; POO admission for source-bounded event fragments.

(import (only-in :clan/poo/object .ref .slot? object?)
        (only-in :clan/poo/mop define-type Type. element?))
(export SourceDelimitedFragment source-delimited-fragment?
        SourceFirstSplit source-first-split?
        SourceReferenceScan source-reference-scan?
        +source-delimited-fragment-kind+ +source-first-split-kind+
        +source-reference-scan-kind+)

(def +source-delimited-fragment-kind+ 'gerbil-parser-source-delimited-fragment)
(def +source-first-split-kind+ 'gerbil-parser-source-first-split)
(def +source-reference-scan-kind+ 'gerbil-parser-source-reference-scan)

(def (source-ascii? value)
  (and (string? value)
       (> (string-length value) 0)
       (every (lambda (char)
                (and (<= 1 (char->integer char))
                     (< (char->integer char) 128)))
              (string->list value))))

(def (source-delimiter? value)
  (and (source-ascii? value)
       (<= (string-length value) 2)))

(def (source-delimited-fragment-shape? value)
  (and (object? value)
       (.slot? value 'kind)
       (.slot? value 'delimiter)
       (.slot? value 'separator-token)
       (.slot? value 'item-helper)
       (.slot? value 'trivia-token)
       (eq? (.ref value 'kind) +source-delimited-fragment-kind+)
       (source-delimiter? (.ref value 'delimiter))
       (symbol? (.ref value 'separator-token))
       (symbol? (.ref value 'item-helper))
       (symbol? (.ref value 'trivia-token))))

(define-type (SourceDelimitedFragment @ Type.)
  .element?: source-delimited-fragment-shape?)

(def (source-delimited-fragment? value)
  (element? SourceDelimitedFragment value))

(def (source-first-split-shape? value)
  (and (object? value)
       (.slot? value 'kind)
       (.slot? value 'delimiter)
       (.slot? value 'separator-token)
       (.slot? value 'before-helper)
       (.slot? value 'after-helper)
       (eq? (.ref value 'kind) +source-first-split-kind+)
       (source-delimiter? (.ref value 'delimiter))
       (= (string-length (.ref value 'delimiter)) 1)
       (symbol? (.ref value 'separator-token))
       (symbol? (.ref value 'before-helper))
       (symbol? (.ref value 'after-helper))))

(define-type (SourceFirstSplit @ Type.)
  .element?: source-first-split-shape?)

(def (source-first-split? value)
  (element? SourceFirstSplit value))

(def (source-reference-scan-shape? value)
  (and (object? value)
       (.slot? value 'kind)
       (.slot? value 'markers)
       (.slot? value 'continuation)
       (.slot? value 'call-prefix)
       (.slot? value 'call-token)
       (.slot? value 'reference-node)
       (.slot? value 'text-token)
       (eq? (.ref value 'kind) +source-reference-scan-kind+)
       (let (markers (.ref value 'markers))
         (and (pair? markers)
              (every (lambda (entry)
                       (and (pair? entry)
                            (char? (car entry))
                            (< (char->integer (car entry)) 128)
                            (symbol? (cdr entry))))
                     markers)))
       (source-ascii? (.ref value 'continuation))
       (source-ascii? (.ref value 'call-prefix))
       (char=? (string-ref (.ref value 'call-prefix)
                           (- (string-length (.ref value 'call-prefix)) 1))
               #\()
       (symbol? (.ref value 'call-token))
       (symbol? (.ref value 'reference-node))
       (symbol? (.ref value 'text-token))))

(define-type (SourceReferenceScan @ Type.)
  .element?: source-reference-scan-shape?)

(def (source-reference-scan? value)
  (element? SourceReferenceScan value))
