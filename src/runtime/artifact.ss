;;; -*- Gerbil -*-
;;; Canonical backend-neutral ParseArtifact v1 and CST event authority.

(import ../modules/parser/types
        ./identity
        ./recognition
        ./token)
(export +parse-artifact-schema-v1+
        +diagnostic-schema-v1+
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

(def (parse-artifact-ref artifact key)
  (let (entry (assq key artifact))
    (and entry (cdr entry))))

(def (parse-artifact-events artifact)
  (parse-artifact-ref artifact 'events))

(def (parse-artifact-status artifact)
  (parse-artifact-ref artifact 'status))

(def (parse-artifact-success? artifact)
  (eq? (parse-artifact-status artifact) 'accepted))

(def (event-kind event)
  (and (vector? event)
       (positive? (vector-length event))
       (vector-ref event 0)))

(def (token-event? event)
  (and (vector? event)
       (= (vector-length event) 6)
       (eq? (event-kind event) 'token)))

(def (token-event-id event) (vector-ref event 1))
(def (token-event-token-kind event) (vector-ref event 2))
(def (token-event-lexeme event) (vector-ref event 3))

(def (event-start event)
  (case (event-kind event)
    ((start-node) (vector-ref event 3))
    ((start-field) (vector-ref event 2))
    ((token) (vector-ref event 4))
    (else #f)))

(def (event-end event)
  (case (event-kind event)
    ((finish-node) (vector-ref event 3))
    ((finish-field) (vector-ref event 2))
    ((token) (vector-ref event 5))
    (else #f)))

(def (make-token-event id source-token)
  (vector 'token id
          (token-kind source-token)
          (token-lexeme source-token)
          (token-start source-token)
          (token-end source-token)))

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

(def (flat-token-events tokens)
  (let loop ((rest tokens) (id 0) (events '()))
    (if (null? rest)
      (let reverse! ((pending events) (found '()))
        (if (null? pending)
          found
          (let (next (cdr pending))
            (set-cdr! pending found)
            (reverse! next pending))))
      (loop (cdr rest) (+ id 1)
            (cons (make-token-event id (car rest)) events)))))

(def (artifact grammar-digest source status events diagnostics)
  (list (cons 'schema +parse-artifact-schema-v1+)
        (cons 'grammarDigest grammar-digest)
        (cons 'sourceDigest (sha256-text source))
        (cons 'sourceByteLength
              (u8vector-length (string->utf8 source)))
        (cons 'status status)
        (cons 'events events)
        (cons 'diagnostics diagnostics)))

(def (make-success-parse-artifact grammar-digest source tokens root trivia?)
  (let* ((source-byte-length (u8vector-length (string->utf8 source)))
         (value
          (artifact grammar-digest source 'accepted
                    (recognition-events tokens root trivia? source-byte-length)
                    '())))
    value))

(def (make-failure-parse-artifact grammar-digest source tokens diagnostic)
  (let (value
        (artifact grammar-digest source 'rejected
                  (flat-token-events tokens)
                  (list diagnostic)))
    value))

(def (digest? value)
  (and (string? value)
       (= (string-length value) 71)
       (string=? (substring value 0 7) "sha256:")))

(def (require-event-shape event length)
  (unless (and (vector? event) (= (vector-length event) length))
    (error "invalid CST event shape" event)))

(def (validate-parse-artifact! artifact)
  (unless (and (list? artifact)
               (equal? (parse-artifact-ref artifact 'schema)
                       +parse-artifact-schema-v1+)
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
      #t)))

(def (parse-artifact-valid? artifact)
  (with-catch
   (lambda (_condition) #f)
   (lambda () (validate-parse-artifact! artifact))))

(def (parse-artifact-roundtrip artifact)
  (unless (parse-artifact-valid? artifact)
    (error "cannot roundtrip invalid ParseArtifact"))
  (call-with-output-string
   (lambda (port)
     (for-each
      (lambda (event)
        (when (token-event? event)
          (display (token-event-lexeme event) port)))
      (parse-artifact-events artifact)))))
