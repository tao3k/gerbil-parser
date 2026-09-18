;;; -*- Gerbil -*-
;;; Canonical backend-neutral ParseArtifact v1 and CST event authority.

(import (only-in :std/misc/func compose every-of)
        (only-in :std/sugar alet cut if-let)
        (only-in ../modules/parser/types
                 +diagnostic-schema+ +parse-artifact-schema+)
        (only-in ./identity sha256-text)
        (only-in ./recognition
                 recognition-child-field recognition-child-value
                 recognition-fragment-children recognition-fragment-end
                 recognition-fragment? recognition-node-children
                 recognition-node-end recognition-node-kind recognition-node?
                 recognition-node-start recognition-value-end
                 recognition-value-start)
        (only-in ./token
                 token? token-end token-kind token-lexeme token-start))
(export +parse-artifact-schema+
        +diagnostic-schema+
        sha256-text
        make-success-parse-artifact
        make-failure-parse-artifact
        parse-artifact-ref
        parse-artifact-events
        parse-artifact-status
        parse-artifact-success?
        parse-artifact-valid?
        parse-artifact-roundtrip
        event-kind
        token-event?
        token-event-id
        token-event-token-kind
        token-event-lexeme
        event-start
        event-end)

;; parse-artifact-ref
;; : (-> Alist Symbol Datum)
(def (parse-artifact-ref artifact key)
  (alet (entry (assq key artifact))
    (cdr entry)))

;; parse-artifact-events
;; : (forall (a) (-> [(Pair Symbol a)] [Vector]))
;; : (-> Alist List)
(def parse-artifact-events (cut parse-artifact-ref <> 'events))

;; parse-artifact-status
;; : (forall (a) (-> [(Pair Symbol a)] Symbol))
;; : (-> Alist Symbol)
(def parse-artifact-status (cut parse-artifact-ref <> 'status))

;; parse-artifact-success?
;; : (-> Alist Boolean)
(def parse-artifact-success?
  (compose (cut eq? <> 'accepted) parse-artifact-status))

;; event-kind
;; : (-> Vector Symbol)
(def (event-kind event)
  (if-let (event (and (vector? event)
                      (positive? (vector-length event))
                      event))
    (vector-ref event 0)
    #f))

;; token-event?
;; : (-> Vector Boolean)
(def +token-event-predicates+
  (list vector?
        (lambda (event) (= (vector-length event) 6))
        (lambda (event) (eq? (event-kind event) 'token))))
(def token-event? (cut every-of +token-event-predicates+ <>))

;; token-event-id
;; : (-> Vector Fixnum)
(def token-event-id (cut vector-ref <> 1))
;; token-event-token-kind
;; : (-> Vector Symbol)
(def token-event-token-kind (cut vector-ref <> 2))
;; token-event-lexeme
;; : (-> Vector String)
(def token-event-lexeme (cut vector-ref <> 3))

;; event-offset
;; : (-> Alist Vector Fixnum)
(def (event-offset offsets event)
  (alet (entry (assq (event-kind event) offsets))
    (vector-ref event (cdr entry))))

(def +event-start-offsets+
  '((start-node . 3) (start-field . 2) (token . 4)))

;; event-start
;; : (-> Vector Fixnum)
(def event-start (cut event-offset +event-start-offsets+ <>))

(def +event-end-offsets+
  '((finish-node . 3) (finish-field . 2) (token . 5)))

;; event-end
;; : (-> Vector Fixnum)
(def event-end (cut event-offset +event-end-offsets+ <>))

;; make-token-event
;; : (-> Fixnum Token Vector)
(def (make-token-event id source-token)
  (vector 'token id
          (token-kind source-token)
          (token-lexeme source-token)
          (token-start source-token)
          (token-end source-token)))

;;; Linearizes the recognition tree and source tokens into one lossless ordered event stream.
;;; Node/token identifiers are allocated once; source gaps become trivia only here.
;; recognition-events
;; : (-> List Recognition Boolean Fixnum List)
(def (recognition-events tokens root trivia? source-byte-length)
  (let ((remaining tokens)
        (events '())
        (next-node-id 0)
        (next-token-id 0))
    (letrec
        ((emit!
          (lambda (event)
            (set! events (cons event events))))
         (emit-source-token!
          (lambda (source-token)
            (emit! (make-token-event next-token-id source-token))
            (set! next-token-id (+ next-token-id 1))
            (set! remaining (cdr remaining))))
         (emit-trivia-until!
          (lambda (boundary)
            (let loop ()
              (when (and (pair? remaining)
                         (<= (token-end (car remaining)) boundary))
                (unless (trivia? (car remaining))
                  (error "unclaimed significant token"
                         (token-kind (car remaining))
                         (token-start (car remaining))))
                (emit-source-token! (car remaining))
                (loop)))))
         (emit-child!
          (lambda (child)
            (let* ((value (recognition-child-value child))
                   (field (recognition-child-field child))
                   (start (recognition-value-start value))
                   (end (recognition-value-end value)))
              (emit-trivia-until! start)
              (when field (emit! (vector 'start-field field start)))
              (emit-value! value)
              (when field (emit! (vector 'finish-field field end))))))
         (emit-children!
          (lambda (children end)
            (for-each emit-child! children)
            (emit-trivia-until! end)))
         (emit-node!
          (lambda (node root?)
            (let* ((id next-node-id)
                   (kind (recognition-node-kind node))
                   (start (if root? 0 (recognition-node-start node)))
                   (end (if root?
                          source-byte-length
                          (recognition-node-end node))))
              (set! next-node-id (+ next-node-id 1))
              (emit! (vector 'start-node id kind start))
              (emit-children! (recognition-node-children node) end)
              (emit! (vector 'finish-node id kind end)))))
         (emit-fragment!
          (lambda (fragment)
            (emit-children! (recognition-fragment-children fragment)
                            (recognition-fragment-end fragment))))
         (emit-token!
          (lambda (source-token)
            (emit-trivia-until! (token-start source-token))
            (unless (and (pair? remaining)
                         (eq? source-token (car remaining)))
              (error "recognition token does not match source order"
                     (token-kind source-token)
                     (token-start source-token)))
            (emit-source-token! source-token)))
         (emit-value!
          (lambda (value)
            (cond
             ((token? value) (emit-token! value))
             ((recognition-node? value) (emit-node! value #f))
             ((recognition-fragment? value) (emit-fragment! value))
             (else (error "invalid recognition value" value))))))
      (unless (recognition-node? root)
        (error "parse root must be a recognition node" root))
      (emit-node! root #t)
      (unless (null? remaining)
        (error "source tokens remain outside parse root" remaining))
      (let reverse! ((rest events) (found '()))
        (if (null? rest)
          found
          (let (next (cdr rest))
            (set-cdr! rest found)
            (reverse! next rest)))))))

;; flat-token-events
;; : (-> List List)
(def (flat-token-events tokens)
  (map make-token-event (iota (length tokens)) tokens))

;; artifact
;; : (-> String String Symbol List List Alist)
(def (artifact grammar-digest source status events diagnostics)
  (map cons
       '(schema grammarDigest sourceDigest sourceByteLength
                status events diagnostics)
       (list +parse-artifact-schema+
             grammar-digest
             (sha256-text source)
             (u8vector-length (string->utf8 source))
             status
             events
             diagnostics)))

;; make-success-parse-artifact
;; : (-> String String List Recognition Boolean Alist)
(def (make-success-parse-artifact grammar-digest source tokens root trivia?)
  (let* ((source-byte-length (u8vector-length (string->utf8 source)))
         (value
          (artifact grammar-digest source 'accepted
                    (recognition-events tokens root trivia? source-byte-length)
                    '())))
    value))

;; make-failure-parse-artifact
;; : (-> String String List Datum Alist)
(def (make-failure-parse-artifact grammar-digest source tokens diagnostic)
  (let (value
        (artifact grammar-digest source 'rejected
                  (flat-token-events tokens)
                  (list diagnostic)))
    value))

;; digest?
;; : (-> Datum Boolean)
(def (digest? value)
  (and (string? value)
       (= (string-length value) 71)
       (string=? (substring value 0 7) "sha256:")))

;; require-event-shape
;; : (-> Vector Fixnum Void)
(def (require-event-shape event length)
  (unless (and (vector? event) (= (vector-length event) length))
    (error "invalid CST event shape" event)))

;;; Validates identity, byte ranges, nesting, and terminal balance before publication.
;;; The admitted source string is returned so roundtrip does not traverse and
;;; concatenate the complete event stream a second time. Malformed streams
;;; still fail closed and never become observable parse artifacts.
;; validate-parse-artifact!
;; : (-> Alist String)
(def (validate-parse-artifact! artifact)
  (unless (and (list? artifact)
               (equal? (parse-artifact-ref artifact 'schema)
                       +parse-artifact-schema+)
               (digest? (parse-artifact-ref artifact 'grammarDigest))
               (digest? (parse-artifact-ref artifact 'sourceDigest)))
    (error "invalid ParseArtifact identity" artifact))
  (let ((source-byte-length
         (parse-artifact-ref artifact 'sourceByteLength))
        (status (parse-artifact-status artifact))
        (events (parse-artifact-events artifact))
        (diagnostics (parse-artifact-ref artifact 'diagnostics))
        (stack '())
        (coverage 0)
        (expected-token-id 0)
        (expected-node-id 0)
        (root-count 0)
        (source-port (open-output-string)))
    (unless (and (integer? source-byte-length)
                 (>= source-byte-length 0)
                 (memq status '(accepted rejected))
                 (list? events)
                 (list? diagnostics))
      (error "invalid ParseArtifact terminal fields" artifact))
    (for-each
     (lambda (event)
       (case (event-kind event)
         ((start-node)
          (require-event-shape event 4)
          (let ((id (vector-ref event 1))
                (kind (vector-ref event 2))
                (start (vector-ref event 3)))
            (unless (and (= id expected-node-id)
                         (symbol? kind)
                         (= start coverage))
              (error "invalid start-node event" event coverage))
            (when (null? stack) (set! root-count (+ root-count 1)))
            (set! expected-node-id (+ expected-node-id 1))
            (set! stack (cons (list 'node id kind start) stack))))
         ((finish-node)
          (require-event-shape event 4)
          (let ((id (vector-ref event 1))
                (kind (vector-ref event 2))
                (end (vector-ref event 3)))
            (unless (and (pair? stack)
                         (eq? (caar stack) 'node)
                         (= id (cadar stack))
                         (eq? kind (caddar stack))
                         (= end coverage)
                         (<= (cadddr (car stack)) end))
              (error "unbalanced finish-node event" event stack coverage))
            (set! stack (cdr stack))))
         ((start-field)
          (require-event-shape event 3)
          (let ((field (vector-ref event 1))
                (start (vector-ref event 2)))
            (unless (and (symbol? field) (= start coverage) (pair? stack))
              (error "invalid start-field event" event coverage))
            (set! stack (cons (list 'field field start) stack))))
         ((finish-field)
          (require-event-shape event 3)
          (let ((field (vector-ref event 1))
                (end (vector-ref event 2)))
            (unless (and (pair? stack)
                         (eq? (caar stack) 'field)
                         (eq? field (cadar stack))
                         (= end coverage)
                         (<= (caddar stack) end))
              (error "unbalanced finish-field event" event stack coverage))
            (set! stack (cdr stack))))
         ((token)
          (require-event-shape event 6)
          (let ((id (token-event-id event))
                (kind (token-event-token-kind event))
                (lexeme (token-event-lexeme event))
                (start (event-start event))
                (end (event-end event)))
            (unless (and (= id expected-token-id)
                         (symbol? kind)
                         (string? lexeme)
                         (= start coverage)
                         (> end start)
                         (= (- end start)
                            (u8vector-length (string->utf8 lexeme))))
              (error "invalid token event coverage" event coverage))
            (display lexeme source-port)
            (set! coverage end)
            (set! expected-token-id (+ expected-token-id 1))))
         (else (error "unknown CST event" event))))
     events)
    (let (source (get-output-string source-port))
      (unless (and (null? stack)
                   (= coverage source-byte-length)
                   (equal? (sha256-text source)
                           (parse-artifact-ref artifact 'sourceDigest)))
        (error "ParseArtifact source coverage mismatch" artifact))
      (case status
        ((accepted)
         (unless (and (= root-count 1) (null? diagnostics))
           (error "accepted ParseArtifact requires one root" artifact)))
        ((rejected)
         (unless (and (= root-count 0) (= (length diagnostics) 1))
           (error "rejected ParseArtifact exposes partial structure" artifact))))
      source)))

;; parse-artifact-valid?
;; : (forall (a) (-> [(Pair Symbol a)] Boolean))
;; : (-> Alist Boolean)
(def (parse-artifact-valid? artifact)
  (with-catch
   (lambda (_condition) #f)
   (lambda ()
     (validate-parse-artifact! artifact)
     #t)))

;; parse-artifact-roundtrip
;; : (forall (a) (-> [(Pair Symbol a)] String))
;; : (-> Alist String)
(def (parse-artifact-roundtrip artifact)
  (let (source
        (with-catch
         (lambda (_condition) #f)
         (lambda () (validate-parse-artifact! artifact))))
    (unless source
      (error "cannot roundtrip invalid ParseArtifact"))
    source))
