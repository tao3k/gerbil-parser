;;; -*- Gerbil -*-
;;; Canonical backend-neutral ParseArtifact v1 and CST event authority.

(import (only-in :std/string/utf8 string-utf8-length)
        (only-in ./parse-cost with-parser-cost-stage)
        (only-in ./event-program event-program-value? event-program-value-kind
                 event-program-value-code event-program-walk/inline event-program-relocate)
        (only-in ./funcs recognition-sequence-for-each)
        (only-in :std/func compose every-of)
        (only-in :std/string/misc string-concatenate-reverse)
        (only-in ../modules/parser/types
                 +diagnostic-schema+ +parse-artifact-schema+)
        (only-in ./identity sha256-bytes sha256-text)
        (only-in ./recognition
                 recognition-relocation? recognition-relocation-value recognition-relocation-delta
                 recognition-child-field recognition-child-value
                 recognition-fragment-children recognition-fragment-end
                 recognition-fragment-start
                 recognition-fragment? recognition-node-children
                 recognition-node-end recognition-node-kind recognition-node?
                 recognition-node-start)
        (only-in ./token
                 token? token-end token-kind token-lexeme token-start))
(export +parse-artifact-schema+
        +diagnostic-schema+
        sha256-text
        make-success-parse-artifact
        make-success-parse-artifact/canonical-events
        make-raw-parse-event
        make-success-parse-artifact/raw-event-tape
        make-same-width-token-artifact
        make-shifted-token-artifact
        make-certified-window-artifact
        make-failure-parse-artifact
        parse-artifact-ref
        parse-artifact-events
        with-parse-event-walk
        parse-artifact-status
        parse-artifact-success?
        parse-artifact-valid? parse-artifact-valid-for-source?
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
  (let (valid-event
        (and (vector? event)
             (positive? (vector-length event))
             event))
    (and valid-event (vector-ref valid-event 0))))

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
(defrule (with-recognition-event-walk tokens root trivia? source-byte-length
                                            node-emitter field-emitter token-emitter)
  (let* ((remaining tokens)
         (next-node-id 0) (next-token-id 0))
    (def (emit-source-token! source-token)
      (token-emitter next-token-id source-token)
      (set! next-token-id (+ next-token-id 1))
      (set! remaining (cdr remaining)))
    (def (emit-trivia-until! boundary)
      (let loop ()
        (when (and (pair? remaining) (<= (token-end (car remaining)) boundary))
          (unless (trivia? (car remaining))
            (error "unclaimed significant token" (token-kind (car remaining))
                   (token-start (car remaining))))
          (emit-source-token! (car remaining)) (loop))))
    (def (emit-token! source-token delta translated?)
      (unless (and (pair? remaining)
                   (if translated?
                     (let (current (car remaining))
                       (and (= (token-start current) (+ (token-start source-token) delta))
                            (= (token-end current) (+ (token-end source-token) delta))
                            (eq? (token-kind current) (token-kind source-token))
                            (equal? (token-lexeme current) (token-lexeme source-token))))
                     (eq? source-token (car remaining))))
        (error "recognition token does not match source order"
               (token-kind source-token) (+ delta (token-start source-token))))
      ;; The canonical event always uses this request's actual source token.
      (emit-source-token! (car remaining)))
    (def (emit-children! children end delta translated?)
      ;; Most production children already are canonical lists. Retain their
      ;; direct consumer rather than adding sequence callbacks and zero deltas.
      (if (list? children)
        (for-each (lambda (child)
                    (emit-value! (recognition-child-value child)
                                 (recognition-child-field child) delta translated? #f)) children)
        (recognition-sequence-for-each
         (lambda (child child-delta child-translated?)
           (emit-value! (recognition-child-value child) (recognition-child-field child)
                        (+ delta child-delta) (or translated? child-translated?) #f)) children))
      (emit-trivia-until! end))
    (def (emit-value! value field delta translated? root?)
      (cond
       ((recognition-relocation? value)
        (emit-value! (recognition-relocation-value value) field
                     (+ delta (recognition-relocation-delta value)) #t root?))
       ((token? value)
        (when root? (error "parse root must be a recognition node" value))
        (let ((start (+ delta (token-start value))) (end (+ delta (token-end value))))
          (emit-trivia-until! start)
          (when field (field-emitter 'start-field field start))
          (emit-token! value delta translated?)
          (when field (field-emitter 'finish-field field end))))
       ((recognition-node? value)
        (let* ((start (if root? 0 (+ delta (recognition-node-start value))))
               (end (if root? source-byte-length (+ delta (recognition-node-end value))))
               (id next-node-id) (kind (recognition-node-kind value)))
          (emit-trivia-until! start)
          (when field (field-emitter 'start-field field start))
          (set! next-node-id (+ next-node-id 1))
          (node-emitter 'start-node id kind start)
          (emit-children! (recognition-node-children value) end delta translated?)
          (node-emitter 'finish-node id kind end)
          (when field (field-emitter 'finish-field field end))))
       ((recognition-fragment? value)
        (when root? (error "parse root must be a recognition node" value))
        (let ((start (+ delta (recognition-fragment-start value)))
              (end (+ delta (recognition-fragment-end value))))
          (emit-trivia-until! start)
          (when field (field-emitter 'start-field field start))
          (emit-children! (recognition-fragment-children value) end delta translated?)
          (when field (field-emitter 'finish-field field end))))
       (else (error "invalid recognition value" value))))
    (emit-value! root #f 0 #f #t)
    (unless (null? remaining) (error "source tokens remain outside parse root" remaining))
    (void)))

(defrule (collect-canonical-events node-emitter field-emitter token-emitter walk)
  (let* ((events (cons #f '())) (tail events))
    (def (emit! event)
      (let (cell (cons event '())) (set-cdr! tail cell) (set! tail cell)))
    (def (node-emitter tag id kind position) (emit! (vector tag id kind position)))
    (def (field-emitter tag field position) (emit! (vector tag field position)))
    (def (token-emitter id token) (emit! (make-token-event id token)))
    walk
    (cdr events)))
(def (recognition-events tokens root trivia? source-byte-length)
  (collect-canonical-events node-emitter field-emitter token-emitter
    (with-recognition-event-walk tokens root trivia? source-byte-length
                                node-emitter field-emitter token-emitter)))

;; flat-token-events
;; : (-> List List)
(def (flat-token-events tokens)
  (map make-token-event (iota (length tokens)) tokens))

;; artifact
;; : (-> String String Symbol List List (? U8Vector) Alist)
(def (artifact grammar-digest source status events diagnostics
               (source-bytes #f))
  (let (bytes (or source-bytes
                 (with-parser-cost-stage 'artifact-source-encoding (string->utf8 source))))
    (with-parser-cost-stage 'artifact-source-identity
      (map cons
           '(schema grammarDigest sourceDigest sourceByteLength
                    status events diagnostics)
           (list +parse-artifact-schema+
                 grammar-digest
                 (sha256-bytes bytes)
                 (u8vector-length bytes)
                 status
                 events
                 diagnostics)))))

;; make-success-parse-artifact
;; : (-> String String List Recognition Boolean Alist)
(def (make-success-parse-artifact grammar-digest source tokens root trivia?)
  (let* ((source-bytes
          (with-parser-cost-stage 'artifact-source-encoding (string->utf8 source)))
         (source-byte-length (u8vector-length source-bytes))
         (value
          (artifact grammar-digest source 'accepted
                    (with-parser-cost-stage 'artifact-event-publication
                     (if (or (event-program-value? root)
                            (and (recognition-relocation? root)
                                 (event-program-root? root)))
                      (event-program-events tokens root trivia? source-byte-length)
                      (recognition-events tokens root trivia? source-byte-length)))
                    '() source-bytes)))
    value))

;;; Interpret committed event bytecode directly into the canonical artifact.
;;; The program retains old source tokens only as proof operands: moved leaves
;;; bind and verify the actual current source token, never a copied substitute.
(def (event-program-root? value)
  (if (recognition-relocation? value)
    (event-program-root? (recognition-relocation-value value))
    (event-program-value? value)))
(def (event-program-root-code root)
  (let loop ((value root) (delta 0) (moved? #f))
    (if (recognition-relocation? value)
      (loop (recognition-relocation-value value)
            (+ delta (recognition-relocation-delta value)) #t)
      (begin
        (unless (and (event-program-value? value) (event-program-value-kind value))
          (error "event program root must be a node"))
        (event-program-relocate (event-program-value-code value) delta moved?)))))
(defrule (with-event-program-walk tokens root trivia? source-byte-length
                                       node-emitter field-emitter token-emitter)
  (let ((remaining tokens) (next-token-id 0)
        (next-node-id 0))
    (let ()
      (def (emit-source-token!)
        (let (input (car remaining))
          (token-emitter next-token-id input)
          (set! next-token-id (+ next-token-id 1))
          (set! remaining (cdr remaining))))
      (def (emit-trivia-until! boundary)
        (let loop ()
          (when (and (pair? remaining) (<= (token-end (car remaining)) boundary))
            (unless (trivia? (car remaining))
              (error "unclaimed significant program token" (token-kind (car remaining))))
            (emit-source-token!) (loop))))
      (event-program-walk/inline
       (lambda (operation name offset delta moved? node-id)
         (let (position (+ offset delta))
           (case operation
             ((token)
              (emit-trivia-until! position)
              (unless (and (pair? remaining)
                           (if moved?
                             (let (actual (car remaining))
                               (and (= (token-start actual) (+ (token-start name) delta))
                                    (= (token-end actual) (+ (token-end name) delta))
                                    (eq? (token-kind actual) (token-kind name))
                                    (equal? (token-lexeme actual) (token-lexeme name))))
                             (eq? name (car remaining))))
                (error "event program token does not bind current source"))
              (emit-source-token!))
             ((open-node)
              (let ((start (if (= next-node-id 0) 0 position)) (id next-node-id))
                (emit-trivia-until! start)
                (node-emitter 'start-node id name start)
                (set! next-node-id (+ next-node-id 1))
                id))
             ((close-node)
              ;; The immutable node instruction supplies its own footer. Its
              ;; opening ID is retained by the traversal frame, including the
              ;; iterative depth fallback, rather than a second node-ID stack.
              (unless (integer? node-id) (error "event node has no opening ID"))
              (let (end (if (= node-id 0) source-byte-length position))
                (emit-trivia-until! end)
                (node-emitter 'finish-node node-id name end)))
             ((open-field)
              (emit-trivia-until! position)
              (field-emitter 'start-field name position))
             ((close-field) (field-emitter 'finish-field name position))
             ((boundary) (emit-trivia-until! position))
             (else (error "unknown committed event program operation" operation)))))
       (event-program-root-code root))
      (unless (null? remaining)
        (error "event program is incomplete"))
      (void))))

(def (event-program-events tokens root trivia? source-byte-length)
  (collect-canonical-events node-emitter field-emitter token-emitter
    (with-event-program-walk tokens root trivia? source-byte-length
                             node-emitter field-emitter token-emitter)))

;;; One source-order authority; sinks choose their own representation. Expansion
;;; keeps canonical emitters local and avoids imposing a generic event object.
(defrule (with-parse-event-walk tokens root trivia? source-byte-length
                              node-emitter field-emitter token-emitter)
  (if (or (event-program-value? root)
          (and (recognition-relocation? root) (event-program-root? root)))
    (with-event-program-walk tokens root trivia? source-byte-length
                             node-emitter field-emitter token-emitter)
    (with-recognition-event-walk tokens root trivia? source-byte-length
                                node-emitter field-emitter token-emitter)))

;;; Assemble already canonical events from a committed generated path.
(def (make-success-parse-artifact/canonical-events
      grammar-digest source events source-bytes)
  (artifact grammar-digest source 'accepted events '() source-bytes))

;;; Named raw descriptors remain available for inspection. The generated HCL
;;; hot path stores their operation, name, and byte offset in a flat tape.
(defstruct raw-parse-event (operation name byte-offset) transparent: #t)

;;; Resolve trivia and allocate canonical IDs once, after the speculative
;;; parser commits a populated prefix of the request-local tape.
(def (make-success-parse-artifact/raw-event-tape
      grammar-digest source tokens tape raw-count trivia? source-bytes)
  (let (events
        (with-parser-cost-stage 'artifact-event-publication
          (raw-event-tape-events tokens tape raw-count trivia?
                                 (u8vector-length source-bytes))))
    (artifact grammar-digest source 'accepted events '() source-bytes)))

;; Event resolution owns exactly the committed tape and canonical event list.
;; Keep source encoding/hash/header construction outside this boundary.
(def (raw-event-tape-events tokens tape raw-count trivia? source-byte-length)
  (let* ((remaining tokens)
         (events (cons #f '()))
         (tail events)
         (next-token-id 0)
         (next-node-id 0)
         (node-ids '()))
    (def (emit! event)
      (let (cell (cons event '()))
        (set-cdr! tail cell)
        (set! tail cell)))
    (def (emit-token! input)
      (unless (and (pair? remaining) (eq? input (car remaining)))
        (error "event token differs from source order" input))
      (emit! (vector 'token next-token-id
                     (token-kind input) (token-lexeme input)
                     (token-start input) (token-end input)))
      (set! next-token-id (fx+ next-token-id 1))
      (set! remaining (cdr remaining)))
    (def (emit-trivia-until! boundary)
      (let loop ()
        (when (and (pair? remaining)
                   (<= (token-end (car remaining)) boundary))
          (unless (trivia? (car remaining))
            (error "unclaimed significant event token"
                   (token-kind (car remaining))))
          (emit-token! (car remaining))
          (loop))))
    (let loop ((index 0))
      (when (< index raw-count)
        (let* ((base (* 3 index))
               (operation (vector-ref tape base))
               (name (vector-ref tape (fx+ base 1)))
               (byte-offset (vector-ref tape (fx+ base 2))))
          (case operation
            ((token)
             (emit-trivia-until! (token-start name))
             (emit-token! name))
            ((open-node)
             (let* ((start (if (= next-node-id 0) 0 byte-offset))
                    (id next-node-id))
               (emit-trivia-until! start)
               (emit! (vector 'start-node id name start))
               (set! next-node-id (fx+ next-node-id 1))
               (set! node-ids (cons id node-ids))))
            ((close-node)
             (let ((end (if (null? (cdr node-ids))
                          source-byte-length byte-offset))
                   (id (car node-ids)))
               (emit-trivia-until! end)
               (emit! (vector 'finish-node id name end))
               (set! node-ids (cdr node-ids))))
            ((open-field)
             (emit-trivia-until! byte-offset)
             (emit! (vector 'start-field name byte-offset)))
            ((close-field)
             (emit! (vector 'finish-field name byte-offset)))
            (else (error "unknown generated event" operation)))
          (loop (fx+ index 1)))))
    (unless (and (null? remaining) (null? node-ids))
      (error "generated event stream is incomplete"))
    (cdr events)))

;;; A certified same-width edit changes exactly one token event. The unchanged
;;; suffix remains shared, including node and field events.
(def (make-same-width-token-artifact old-artifact source token-id source-token)
  (unless (parse-artifact-success? old-artifact)
    (error "token event reuse requires an accepted artifact"))
  (let (events
        (let loop ((remaining (parse-artifact-events old-artifact))
                   (prefix '()))
          (cond
           ((null? remaining)
            (error "token event id is absent" token-id))
           ((and (token-event? (car remaining))
                 (= (token-event-id (car remaining)) token-id))
            (foldl cons
                   (cons (make-token-event token-id source-token)
                         (cdr remaining))
                   prefix))
           (else (loop (cdr remaining)
                       (cons (car remaining) prefix))))))
    (artifact (parse-artifact-ref old-artifact 'grammarDigest)
              source 'accepted events '())))

;;; Preserve the event topology while rebasing offsets after one token whose
;;; byte width changed. Event boundaries inside a token are not admissible.
(def (make-shifted-token-artifact old-artifact source token-id
                                  token-start token-end source-token delta)
  (unless (parse-artifact-success? old-artifact)
    (error "shifted token reuse requires an accepted artifact"))
  (let ((replaced? #f) (shared 0))
    (def (offset value)
      (cond
       ((<= value token-start) value)
       ((>= value token-end) (+ value delta))
       (else (error "event boundary is inside the edited token" value))))
    (def (keep event)
      (set! shared (+ shared 1))
      event)
    (let (events
          (map
           (lambda (event)
             (case (event-kind event)
               ((token)
                (let ((id (token-event-id event))
                      (start (event-start event))
                      (end (event-end event)))
                  (cond
                   ((= id token-id)
                    (set! replaced? #t)
                    (make-token-event id source-token))
                   ((<= end token-start) (keep event))
                   ((>= start token-end)
                    (vector 'token id (token-event-token-kind event)
                            (token-event-lexeme event)
                            (+ start delta) (+ end delta)))
                   (else (error "token overlaps the edited token" event)))))
               ((start-node finish-node)
                (let* ((value (vector-ref event 3))
                       (shifted (offset value)))
                  (if (= value shifted)
                    (keep event)
                    (vector (event-kind event) (vector-ref event 1)
                            (vector-ref event 2) shifted))))
               ((start-field finish-field)
                (let* ((value (vector-ref event 2))
                       (shifted (offset value)))
                  (if (= value shifted)
                    (keep event)
                    (vector (event-kind event) (vector-ref event 1)
                            shifted))))
               (else (error "unknown recognition event" event))))
           (parse-artifact-events old-artifact)))
      (unless replaced?
        (error "token event id is absent" token-id))
      (values
       (artifact (parse-artifact-ref old-artifact 'grammarDigest)
                 source 'accepted events '())
       shared))))

;;; Each old token boundary in a certified window has one new boundary. LR
;;; reductions can only place node and field boundaries at token boundaries,
;;; so the event topology remains valid when their offsets are remapped.
(def (make-certified-window-artifact old-artifact source first-id
                                      old-window new-window delta
                                      (assigned #f) (boundaries #f))
  (unless (and (parse-artifact-success? old-artifact)
               (pair? old-window)
               (or (not assigned)
                   (and (= (length assigned) (length old-window))
                        (equal? (apply append assigned) new-window))))
    (error "invalid certified token window"))
  (let* ((boundary-map (make-table test: equal?))
         (start (token-start (car old-window)))
         (end (token-end (car (reverse old-window))))
         (limit (+ first-id (length old-window)))
         (id-delta (- (length new-window) (length old-window)))
         (groups
          (or assigned
              (if (zero? id-delta)
                (map list new-window)
                (cons new-window
                      (make-list (- (length old-window) 1) '())))))
         (next-id first-id)
         (replacement-vector
          (list->vector
           (map
            (lambda (group)
              (map
               (lambda (source-token)
                 (let (event (make-token-event next-id source-token))
                   (set! next-id (+ next-id 1))
                   event))
               group))
            groups)))
         (shared 0))
    (table-set! boundary-map start start)
    (table-set! boundary-map end (+ end delta))
    (cond
     (boundaries
      (for-each (lambda (entry)
                  (table-set! boundary-map (car entry) (cdr entry)))
                boundaries))
     ((zero? id-delta)
      (for-each
       (lambda (old-token new-token)
         (table-set! boundary-map (token-start old-token)
                     (token-start new-token))
         (table-set! boundary-map (token-end old-token)
                     (token-end new-token)))
       old-window new-window)))
    (def (offset value)
      (cond
       ((< value start) value)
       ((> value end) (+ value delta))
       (else
        (let (mapped (table-ref boundary-map value 'missing))
          (if (eq? mapped 'missing)
            (error "event boundary is inside a certified token" value)
            mapped)))))
    (def (keep event)
      (set! shared (+ shared 1))
      event)
    (let (events
          (foldr
           (lambda (event tail)
             (case (event-kind event)
               ((token)
                (let ((id (token-event-id event))
                      (old-start (event-start event))
                      (old-end (event-end event)))
                  (cond
                   ((< id first-id) (cons (keep event) tail))
                   ((< id limit)
                    (append (vector-ref replacement-vector
                                        (- id first-id))
                            tail))
                   ((and (zero? delta) (zero? id-delta))
                    (cons (keep event) tail))
                   (else
                    (cons
                     (vector 'token (+ id id-delta)
                             (token-event-token-kind event)
                             (token-event-lexeme event)
                             (offset old-start) (offset old-end))
                     tail)))))
               ((start-node finish-node)
                (let* ((value (vector-ref event 3))
                       (mapped (offset value)))
                  (if (= value mapped)
                    (cons (keep event) tail)
                    (cons (vector (event-kind event) (vector-ref event 1)
                                  (vector-ref event 2) mapped) tail))))
               ((start-field finish-field)
                (let* ((value (vector-ref event 2))
                       (mapped (offset value)))
                  (if (= value mapped)
                    (cons (keep event) tail)
                    (cons (vector (event-kind event) (vector-ref event 1)
                                  mapped) tail))))
               (else (error "unknown recognition event" event))))
           '() (parse-artifact-events old-artifact)))
      (values
       (artifact (parse-artifact-ref old-artifact 'grammarDigest)
                 source 'accepted events '())
       shared))))

;; make-failure-parse-artifact
;; : (-> String String List Datum Alist)
(def (make-failure-parse-artifact grammar-digest source tokens diagnostic)
  (let (value
        (artifact grammar-digest source 'rejected
                  (with-parser-cost-stage 'artifact-event-publication
                    (flat-token-events tokens))
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
;;; concatenate the complete event stream a second time. Validated lexemes are
;;; copied into one exactly sized string; no general text port participates in
;;; reconstruction and no advertised byte length drives an eager allocation.
;;; Publication can instead bind the actual input: compare admitted lexemes
;;; directly, prove complete character coverage and hash that input once.
;;; Malformed streams
;;; still fail closed and never become observable parse artifacts.
;; validate-parse-artifact!
;; : (-> Alist String)
(def (validate-parse-artifact! artifact (expected-source #f))
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
        (source-chunks '())
        (character-coverage 0))
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
                            (string-utf8-length lexeme)))
              (error "invalid token event coverage" event coverage))
            (if expected-source
              (let (width (string-length lexeme))
                (unless (and (<= (+ character-coverage width) (string-length expected-source))
                             (source-lexeme-at? expected-source character-coverage lexeme))
                  (error "ParseArtifact lexeme differs from input source" event))
                (set! character-coverage (+ character-coverage width)))
              (set! source-chunks (cons lexeme source-chunks)))
            (set! coverage end)
            (set! expected-token-id (+ expected-token-id 1))))
         (else (error "unknown CST event" event))))
     events)
    (let (source (or expected-source (string-concatenate-reverse source-chunks)))
      (unless (and (null? stack)
                   (= coverage source-byte-length)
                   (or (not expected-source) (= character-coverage (string-length expected-source)))
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

(def (source-lexeme-at? source offset lexeme)
  ;; Callers prove containment first. Compare characters directly, without a
  ;; temporary substring or encoding; byte coverage is checked independently.
  (let (width (string-length lexeme))
    (let loop ((index 0))
      (or (= index width)
          (and (char=? (string-ref source (+ offset index)) (string-ref lexeme index))
               (loop (+ index 1)))))))

(def (parse-artifact-valid-for-source? artifact source)
  (and (string? source)
       (with-catch (lambda (_) #f)
         (lambda () (validate-parse-artifact! artifact source) #t))))

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
