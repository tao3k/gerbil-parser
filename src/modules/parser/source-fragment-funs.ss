;;; -*- Gerbil -*-
;;; Lower an admitted POO fragment to the existing Scheme/event-AOT algebra.
;;; One pass over the bounded slice emits source-backed separator and item events.

(import (only-in ./source-fragment-types
                 source-delimited-fragment? source-first-split?
                 source-reference-scan?)
        (only-in ./source-fragment-objects
                 source-delimited-fragment-delimiter
                 source-delimited-fragment-separator-token
                 source-delimited-fragment-item-helper
                 source-delimited-fragment-trivia-token
                 source-first-split-delimiter
                 source-first-split-separator-token
                 source-first-split-before-helper
                 source-first-split-after-helper
                 source-reference-scan-markers
                 source-reference-scan-continuation
                 source-reference-scan-call-prefix
                 source-reference-scan-call-token
                 source-reference-scan-reference-node
                 source-reference-scan-text-token))
(export source-delimited-fragment-initial
        source-delimited-fragment-forms
        source-first-split-initial source-first-split-forms
        source-reference-scan-initial source-reference-scan-forms)

(def segment-index '(line-index segment-byte-index))
(def segment-next `(line-step ,segment-index))
(def segment-start '(state-offset segment-start))

(def (segment-events rule from until)
  `(if (line-bytes-all-in? ,from ,until (9 32))
       ((token ,(source-delimited-fragment-trivia-token rule) ,from ,until))
       ((call-source-helper ,(source-delimited-fragment-item-helper rule)
                            ,from ,until))))

(def (source-delimited-fragment-initial rule)
  (unless (source-delimited-fragment? rule)
    (error "unadmitted source fragment" rule))
  '((segment-start 0) (segment-skip-next #f)))

(def (source-delimited-fragment-forms rule)
  (unless (source-delimited-fragment? rule)
    (error "unadmitted source fragment" rule))
  (let* ((delimiter (source-delimited-fragment-delimiter rule))
         (first (char->integer (string-ref delimiter 0)))
         (double? (= (string-length delimiter) 2))
         (match? (if double?
                   `(and (line-byte-equal? ,segment-index ,first)
                         (line-byte-equal? ,segment-next
                                           ,(char->integer
                                             (string-ref delimiter 1))))
                   `(line-byte-equal? ,segment-index ,first)))
         (separator-end (if double? `(line-step ,segment-next) segment-next))
         (separator-token (source-delimited-fragment-separator-token rule)))
    `((set-uint segment-start (offset start))
      (for-line-bytes segment-byte-index start end
        ((if (state segment-skip-next)
             ((set-bool segment-skip-next (bool #f)))
             ((if ,match?
                  (,(segment-events rule segment-start segment-index)
                   (token ,separator-token ,segment-index ,separator-end)
                   (set-uint segment-start (offset ,separator-end))
                   ,@(if double?
                       '((set-bool segment-skip-next (bool #t)))
                       '())) ())))))
      ,(segment-events rule segment-start 'end))))

(def split-index '(line-index split-byte-index))
(def split-at '(state-offset split-at))
(def split-after `(line-step ,split-at))

(def (source-first-split-initial rule)
  (unless (source-first-split? rule)
    (error "unadmitted source first-split" rule))
  '((split-at 0) (split-seen #f)))

(def (source-first-split-forms rule)
  (unless (source-first-split? rule)
    (error "unadmitted source first-split" rule))
  (let ((delimiter (char->integer
                    (string-ref (source-first-split-delimiter rule) 0)))
        (separator-token (source-first-split-separator-token rule))
        (before-helper (source-first-split-before-helper rule))
        (after-helper (source-first-split-after-helper rule)))
    `((for-line-bytes split-byte-index start end
        ((if (and (not (state split-seen))
                  (line-byte-equal? ,split-index ,delimiter))
             ((set-uint split-at (offset ,split-index))
              (set-bool split-seen (bool #t))) ())))
      (if (state split-seen)
          ((call-source-helper ,before-helper start ,split-at)
           (token ,separator-token ,split-at ,split-after)
           (call-source-helper ,after-helper ,split-after end))
          ((call-source-helper ,before-helper start end))))))

(def ref-index '(line-index ref-byte-index))
(def ref-next `(line-step ,ref-index))
(def ref-start '(state-offset ref-start))
(def ref-text-start '(state-offset ref-text-start))

(def (offset-after offset count)
  (let loop ((remaining count) (cursor offset))
    (if (= remaining 0) cursor
      (loop (- remaining 1) `(line-step ,cursor)))))

(def (prefix-at offset prefix)
  (cons 'and
        (let loop ((chars (string->list prefix)) (cursor offset))
          (if (null? chars) '()
            (cons `(line-byte-equal? ,cursor ,(char->integer (car chars)))
                  (loop (cdr chars) `(line-step ,cursor)))))))

(def (ref-node rule token until (continue? #t))
  `((start-node ,(source-reference-scan-reference-node rule))
    (token ,token ,ref-start ,until)
    (finish-node)
    (set-uint ref-text-start (offset ,until))
    ,@(if continue? '((set-uint ref-kind (uint 0))) '())))

(def (ref-begin rule kind)
  `((if (offset-less? ,ref-text-start ,ref-index)
        ((token ,(source-reference-scan-text-token rule)
                ,ref-text-start ,ref-index)) ())
    (set-uint ref-start (offset ,ref-index))
    (set-uint ref-text-start (offset ,ref-index))
    (set-uint ref-kind (uint ,kind))))

(def (marker-start-chain rule markers call-kind)
  `(if ,(prefix-at ref-index (source-reference-scan-call-prefix rule))
       ,(ref-begin rule call-kind)
       ,(let loop ((rest markers) (kind 1))
          (if (null? rest) '()
            `((if (line-byte-equal? ,ref-index
                                   ,(char->integer (caar rest)))
                  ,(ref-begin rule kind)
                  ,(loop (cdr rest) (+ kind 1))))))))

(def (marker-end-chain rule markers until (continue? #t))
  (let loop ((rest markers) (kind 1))
    (if (null? rest) '()
      `((if (uint-equal? (state ref-kind) (uint ,kind))
            ,(ref-node rule (cdar rest) until continue?)
            ,(loop (cdr rest) (+ kind 1)))))))

(def (source-reference-scan-initial rule)
  (unless (source-reference-scan? rule)
    (error "unadmitted source reference scan" rule))
  '((ref-start 0) (ref-text-start 0) (ref-kind 0)
    (ref-opens 0) (ref-closes 0)))

(def (source-reference-scan-forms rule)
  (unless (source-reference-scan? rule)
    (error "unadmitted source reference scan" rule))
  (let* ((markers (source-reference-scan-markers rule))
         (call-kind (+ (length markers) 1))
         (continuation (map char->integer
                            (string->list
                             (source-reference-scan-continuation rule))))
         (call-token (source-reference-scan-call-token rule)))
    `((set-uint ref-text-start (offset start))
      (for-line-bytes ref-byte-index start end
        ((if (uint-equal? (state ref-kind) (uint ,call-kind))
             ((if (line-byte-equal? ,ref-index 40)
                  ((set-uint ref-opens
                             (uint-add (state ref-opens) (uint 1))))
                  ((if (and (line-byte-equal? ,ref-index 41)
                            (uint-greater? (state ref-opens)
                                           (state ref-closes)))
                       ((set-uint ref-closes
                                  (uint-add (state ref-closes) (uint 1)))
                        (if (uint-equal? (state ref-opens)
                                         (state ref-closes))
                            ,(ref-node rule call-token ref-next) ())) ()))))
             ((if (and (uint-positive? (state ref-kind))
                       (not (line-bytes-any-in?
                             ,ref-index ,ref-next ,continuation)))
                  ,(marker-end-chain rule markers ref-index) ())
              (if (uint-equal? (state ref-kind) (uint 0))
                  (,(marker-start-chain rule markers call-kind)) ())))))
      ,@(marker-end-chain rule markers 'end #f)
      (if (offset-less? ,ref-text-start end)
          ((token ,(source-reference-scan-text-token rule)
                  ,ref-text-start end)) ()))))
