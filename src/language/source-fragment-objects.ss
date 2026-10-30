;;; -*- Gerbil -*-
;;; Source fragment declarations are admitted POO values, not raw form tuples.

(import (only-in :clan/poo/object .o .ref)
        (only-in ./source-fragment-types
                 source-delimited-fragment? source-first-split?
                 source-reference-scan?
                 +source-delimited-fragment-kind+ +source-first-split-kind+
                 +source-reference-scan-kind+))
(export make-source-delimited-fragment
        source-delimited-fragment-delimiter
        source-delimited-fragment-separator-token
        source-delimited-fragment-item-helper
        source-delimited-fragment-trivia-token
        make-source-first-split source-first-split-delimiter
        source-first-split-separator-token source-first-split-before-helper
        source-first-split-after-helper
        make-source-reference-scan source-reference-scan-markers
        source-reference-scan-continuation
        source-reference-scan-call-prefix source-reference-scan-call-token
        source-reference-scan-reference-node source-reference-scan-text-token)

(def (make-source-delimited-fragment delimiter-value separator-token-value
                                     item-helper-value trivia-token-value)
  (let (candidate
        (.o kind: +source-delimited-fragment-kind+
            delimiter: delimiter-value
            separator-token: separator-token-value
            item-helper: item-helper-value
            trivia-token: trivia-token-value))
    (unless (source-delimited-fragment? candidate)
      (error "invalid source-delimited fragment" delimiter-value
             separator-token-value item-helper-value trivia-token-value))
    candidate))

(def (source-delimited-fragment-delimiter value) (.ref value 'delimiter))
(def (source-delimited-fragment-separator-token value)
  (.ref value 'separator-token))
(def (source-delimited-fragment-item-helper value) (.ref value 'item-helper))
(def (source-delimited-fragment-trivia-token value) (.ref value 'trivia-token))

(def (make-source-first-split delimiter-value separator-token-value
                              before-helper-value after-helper-value)
  (let (candidate
        (.o kind: +source-first-split-kind+
            delimiter: delimiter-value
            separator-token: separator-token-value
            before-helper: before-helper-value
            after-helper: after-helper-value))
    (unless (source-first-split? candidate)
      (error "invalid source first-split" delimiter-value
             separator-token-value before-helper-value after-helper-value))
    candidate))

(def (source-first-split-delimiter value) (.ref value 'delimiter))
(def (source-first-split-separator-token value) (.ref value 'separator-token))
(def (source-first-split-before-helper value) (.ref value 'before-helper))
(def (source-first-split-after-helper value) (.ref value 'after-helper))

(def (make-source-reference-scan markers-value continuation-value
                                 call-prefix-value call-token-value
                                 reference-node-value text-token-value)
  (let (candidate
        (.o kind: +source-reference-scan-kind+
            markers: markers-value
            continuation: continuation-value
            call-prefix: call-prefix-value
            call-token: call-token-value
            reference-node: reference-node-value
            text-token: text-token-value))
    (unless (source-reference-scan? candidate)
      (error "invalid source reference scan" candidate))
    candidate))

(def (source-reference-scan-markers value) (.ref value 'markers))
(def (source-reference-scan-continuation value) (.ref value 'continuation))
(def (source-reference-scan-call-prefix value) (.ref value 'call-prefix))
(def (source-reference-scan-call-token value) (.ref value 'call-token))
(def (source-reference-scan-reference-node value) (.ref value 'reference-node))
(def (source-reference-scan-text-token value) (.ref value 'text-token))
