;;; Prepare ordered byte decision regions before procedure expansion.
;;; Only a validated loop byte and total guard prefixes may select a region.
(import (only-in :std/list/list delete-duplicates/hash)
        (only-in :std/list/list-builder with-list-builder)
        (only-in ./event-fold-analysis fold-expression-analysis))
(export fold-byte-dispatch fold-byte-prefilter fold-necessary-bytes fold-byte-regions fold-line-negative-region)

;; One shared negative classification across ordered line consumers. Retain
;; each original transition once, guarded by the frame-local classification.
;; No successful suffix/state match follows from a positive classification.
;; Frames, loops and helper handoffs remain opaque boundaries.
(def (fold-line-negative-region forms)
  (def prefixes '())
  (def (necessary predicate)
    (cond
      ((and (pair? predicate)
            (memq (car predicate) '(line-starts-with line-starts-with-ascii-ci
                                   line-prefix-boundary-ascii-ci line-marker-ascii-ci
                                   line-has-word-after-prefix? line-has-key-after-prefix?)))
       (let (bytes (u8vector->list (string->utf8 (cadr predicate))))
         (and (pair? bytes) (list (cons 'start bytes)))))
      ((and (pair? predicate) (eq? (car predicate) 'line-byte-equal?)
            (member (cadr predicate) '(start (line-skip-horizontal start))))
       (list (cons (if (eq? (cadr predicate) 'start) 'start 'horizontal)
                   (list (caddr predicate)))))
      ((and (pair? predicate) (eq? (car predicate) 'line-bytes-any-in?)
            (member (cadr predicate) '(start (line-skip-horizontal start)))
            (equal? (caddr predicate) (list 'line-step (cadr predicate))))
       (and (pair? (list-ref predicate 3))
            (map (lambda (byte)
                   (cons (if (eq? (cadr predicate) 'start) 'start 'horizontal)
                         (list byte))) (list-ref predicate 3))))
      ((and (pair? predicate) (eq? (car predicate) 'and))
       (let loop ((rest (conjunction-terms predicate)))
         (and (pair? rest)
              (or (necessary (car rest))
                  (and (null? (vector-ref (fold-expression-analysis (car rest)) 2))
                       (loop (cdr rest)))))))
      ((and (pair? predicate) (eq? (car predicate) 'or))
       (let (alternatives (map necessary (cdr predicate)))
         (and (every list? alternatives) (foldr append '() alternatives))))
      (else #f)))
  (def (walk body)
    (with-list-builder (emit)
      (for-each
       (lambda (form)
         (case (car form)
           ((if)
            (let (needed (necessary (cadr form)))
              (if needed
                (begin
                  (set! prefixes (append needed prefixes))
                  (emit (list 'if (list 'and '(%line-prefix-hit) (cadr form))
                              (walk (caddr form)) (walk (cadddr form)))))
                (emit (list 'if (cadr form) (walk (caddr form)) (walk (cadddr form)))))))
           ((join-once)
            (emit (list 'join-once (cadr form) (walk (caddr form)) (walk (cadddr form)))))
           (else (emit form)))) body)))
  (let ((residual (walk forms)))
    (and (pair? prefixes) (not (equal? forms residual))
         (vector (delete-duplicates/hash prefixes from-end?: #t) residual))))

;; One classification for the entire validated byte-loop body, not one guard
;; per independent state machine. Descend only through decisions and lexical
;; joins: source frames/helpers/nested iterations stay in their owners.
;; A mismatch bypasses total guard reads, never queries/errors/effects.
(def (fold-byte-regions forms cursor)
  (def (cofactor byte)
    (def (walk body)
     (with-list-builder (emit)
      (for-each
       (lambda (form)
         (case (car form)
           ((if)
            (let (members (fold-necessary-bytes (cadr form) cursor))
              (if (and members (not (memv byte members)))
                (for-each emit (walk (cadddr form)))
                (emit (list 'if (cadr form) (walk (caddr form)) (walk (cadddr form)))))))
           ((join-once)
            (emit (list 'join-once (cadr form) (walk (caddr form)) (walk (cadddr form)))))
           (else (emit form)))) body)))
    (walk forms))
  (let ((fallback (cofactor -1)) (by-body (make-hash-table)) (groups '()))
    (for-each
     (lambda (byte)
       (let (body (cofactor byte))
         (unless (equal? body fallback)
           (let (group (or (hash-get by-body body)
                          (let (created (vector '() body))
                            (hash-put! by-body body created)
                            (set! groups (cons created groups)) created)))
             (vector-set! group 0 (cons byte (vector-ref group 0)))))))
     (iota 256))
    (and (pair? groups)
         (vector (map (lambda (group)
                        (cons (reverse (vector-ref group 0)) (vector-ref group 1)))
                      (reverse groups)) fallback))))

;; Literal source patterns lower to nested conjunctions. Flatten only `and`;
;; the original short-circuit order and all effect barriers remain intact.
(def (conjunction-terms predicate)
  ;; The standard builder emits each leaf once, preserving short-circuit order
  ;; without copying flattened child lists at every enclosing conjunction.
  (with-list-builder (emit)
    (def (visit term)
      (if (and (pair? term) (eq? (car term) 'and))
        (for-each visit (cdr term))
        (emit term)))
    (visit predicate)))

;; A byte condition is necessary only before any observable leaf. For an
;; ordered choice every alternative must supply such a condition. Keep each
;; original alternative intact: a byte match is not a full pattern match.
(def (fold-necessary-bytes predicate cursor)
  (cond
   ((byte-leaf? predicate cursor) (list (caddr predicate)))
   ((and (pair? predicate) (eq? (car predicate) 'line-bytes-any-in?)
         (equal? (cadr predicate) (list 'line-index cursor))
         (equal? (caddr predicate) (list 'line-step (cadr predicate))))
    (list-ref predicate 3))
   ((and (pair? predicate) (eq? (car predicate) 'and))
    (let loop ((rest (conjunction-terms predicate)))
      (and (pair? rest)
           (or (fold-necessary-bytes (car rest) cursor)
               (and (null? (vector-ref (fold-expression-analysis (car rest)) 2))
                    (loop (cdr rest)))))))
   ((and (pair? predicate) (eq? (car predicate) 'or))
    (let (alternatives (map (cut fold-necessary-bytes <> cursor) (cdr predicate)))
      (and (every list? alternatives)
           (delete-duplicates/hash (foldr append '() alternatives) from-end?: #t))))
   (else #f)))

(def (necessary-test members cursor)
  `(line-bytes-any-in? (line-index ,cursor) (line-step (line-index ,cursor)) ,members))

;; A validated loop byte can reject an entire state-dependent decision before
;; reading its slots. Only total prefixes can move behind the byte guard;
;; opaque operations, queries and range errors remain in their original order.
(def (fold-byte-prefilter predicate cursor)
  (def (byte-test? value)
    (and (pair? value)
         (or (byte-leaf? value cursor)
             (and (eq? (car value) 'line-bytes-any-in?)
                  (equal? (cadr value) (list 'line-index cursor))
                  (equal? (caddr value) (list 'line-step (cadr value)))))))
  (cond
   ((and (pair? predicate) (eq? (car predicate) 'or))
    (let (members (fold-necessary-bytes predicate cursor))
      (and members `(and ,(necessary-test members cursor) ,predicate))))
   (else
    (and (pair? predicate) (eq? (car predicate) 'and)
       (let loop ((rest (conjunction-terms predicate)) (before '()))
         (cond ((null? rest) #f)
               ((byte-test? (car rest))
                (and (pair? before)
                     (cons 'and (cons (car rest)
                                      (append (reverse before) (cdr rest))))))
               ((and (pair? (car rest)) (eq? (caar rest) 'or)
                     (fold-necessary-bytes (car rest) cursor))
                => (lambda (members)
                     (and (pair? before)
                          (cons 'and (cons (necessary-test members cursor)
                                           (append (reverse before) rest))))))
               ((null? (vector-ref (fold-expression-analysis (car rest)) 2))
                (loop (cdr rest) (cons (car rest) before)))
               (else #f)))))))
(def (byte-leaf? predicate cursor)
  (and (pair? predicate) (eq? (car predicate) 'line-byte-equal?)
       (equal? (cadr predicate) (list 'line-index cursor))))
(def (guard predicate cursor)
  (cond ((byte-leaf? predicate cursor) (cons (caddr predicate) '(bool #t)))
        ((and (pair? predicate) (eq? (car predicate) 'and))
         (let loop ((rest (conjunction-terms predicate)) (before '()))
           (cond ((null? rest) #f)
                 ((byte-leaf? (car rest) cursor)
                  (cons (caddr (car rest))
                        (let ((terms (append (reverse before) (cdr rest))))
                          (if (null? terms) '(bool #t) (cons 'and terms)))))
                 ;; Never bypass errors, queries, allocation or opaque leaves.
                 ((null? (vector-ref (fold-expression-analysis (car rest)) 2))
                  (loop (cdr rest) (cons (car rest) before)))
                 (else #f))))
        (else #f)))
(def (fold-byte-dispatch form cursor)
  (def (finish entries fallback)
    (let ((bytes (delete-duplicates/hash (map car entries) from-end?: #t)))
      ;; A singleton keeps its original branch, not an extra dispatch layer.
      (and (> (length bytes) 1)
           (vector
            (map (lambda (byte)
                   (cons byte
                     (let choose ((rest entries))
                       (if (null? rest) fallback
                         (let ((entry (car rest)))
                           (cond ((not (= byte (car entry))) (choose (cdr rest)))
                                 ((equal? (cadr entry) '(bool #t)) (caddr entry))
                                 (else (list (list 'if (cadr entry) (caddr entry)
                                                   (choose (cdr rest))))))))))) bytes)
            fallback))))
  (let collect ((form form) (entries '()))
    (let ((fact (and (pair? form) (eq? (car form) 'if)
                     (guard (cadr form) cursor))))
      (if fact
        (let ((fallback (cadddr form))
              (next (cons (list (car fact) (cdr fact) (caddr form)) entries)))
          (if (and (= (length fallback) 1) (eq? (caar fallback) 'if))
            (collect (car fallback) next)
            (finish (reverse next) fallback)))
        (finish (reverse entries) (list form))))))
