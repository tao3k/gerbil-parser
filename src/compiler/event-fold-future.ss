;;; -*- Gerbil -*-
;;; Source-backed future-line queries for Scheme event folds.
(import (only-in ./funcs.ss ascii-lower-byte
                 compiler-position-index-add!
                 compiler-position-index-freeze!
                 compiler-position-vector-next
                 compiler-u8vector-prefix?))
(export ascii-lower-byte current-future-scan-cache
        fold-future-cache-for fold-future-index-for
        fold-future-named-index fold-future-named-index-result
        fold-future-heading-title? fold-future-heading-spec?
        fold-future-marker-before-boundary? fold-source-slices-equal?)

;; Cache suffix answers for one parse request; dynamic parameters keep nested
;; and concurrent parses independent.
(def current-future-scan-cache (make-parameter #f))

(def (fold-future-line-end bytes start)
  (let (size (u8vector-length bytes))
    (let loop ((cursor start))
      (if (or (= cursor size) (memv (u8vector-ref bytes cursor) '(10 13)))
        (if (= cursor size) cursor
          (if (and (= (u8vector-ref bytes cursor) 13)
                   (< (+ cursor 1) size)
                   (= (u8vector-ref bytes (+ cursor 1)) 10))
            (+ cursor 2) (+ cursor 1)))
        (loop (+ cursor 1))))))

(def (fold-future-marker-line? bytes start end marker indent?)
  (let* ((prefix (string->utf8 marker))
         (begin (if indent?
                  (let skip ((cursor start))
                    (if (and (< cursor end)
                             (memv (u8vector-ref bytes cursor) '(9 32)))
                      (skip (+ cursor 1)) cursor))
                  start))
         (prefix-end (+ begin (u8vector-length prefix))))
    (and (<= prefix-end end)
         (let match ((cursor begin) (index 0))
           (or (= index (u8vector-length prefix))
               (and (= (ascii-lower-byte (u8vector-ref bytes cursor))
                       (ascii-lower-byte (u8vector-ref prefix index)))
                    (match (+ cursor 1) (+ index 1)))))
         (let tail ((cursor prefix-end))
           (or (= cursor end)
               (and (memv (u8vector-ref bytes cursor) '(9 10 13 32))
                    (tail (+ cursor 1))))))))

(def (fold-source-slices-equal? bytes left-from left-until
                                right-from right-until ascii-ci?)
  (let (size (u8vector-length bytes))
    (and (<= 0 left-from left-until size)
         (<= 0 right-from right-until size)
         (= (- left-until left-from) (- right-until right-from))
         (let loop ((left left-from) (right right-from))
           (or (= left left-until)
               (and (let ((a (u8vector-ref bytes left))
                          (b (u8vector-ref bytes right)))
                      (= (if ascii-ci? (ascii-lower-byte a) a)
                         (if ascii-ci? (ascii-lower-byte b) b)))
                    (loop (+ left 1) (+ right 1))))))))

(def (fold-future-cache-for query)
  (let* ((request-cache
          (or (current-future-scan-cache)
              (let (created (make-hash-table))
                (current-future-scan-cache created)
                created)))
         (cached (hash-get request-cache query)))
    (or cached
        (let (created (make-hash-table))
          (hash-put! request-cache query created)
          created))))

(def (fold-future-index-for query build)
  (let* ((request-cache
          (or (current-future-scan-cache)
              (let (created (make-hash-table))
                (current-future-scan-cache created)
                created)))
         (cached (hash-get request-cache query)))
    (or cached
        (let (index (build))
          (hash-put! request-cache query index)
          index))))

(def (fold-normalize-marker bytes from until indent? ascii-ci?)
  (let* ((start (if indent?
                  (let skip ((cursor from))
                    (if (and (< cursor until)
                             (memv (u8vector-ref bytes cursor) '(9 32)))
                      (skip (+ cursor 1)) cursor))
                  from))
         (end (let trim ((cursor until))
                (if (and (> cursor start)
                         (memv (u8vector-ref bytes (- cursor 1))
                               '(9 10 13 32)))
                  (trim (- cursor 1)) cursor)))
         (key (subu8vector bytes start end)))
    (when ascii-ci?
      (let loop ((index 0))
        (when (< index (u8vector-length key))
          (u8vector-set! key index
                         (ascii-lower-byte (u8vector-ref key index)))
          (loop (+ index 1)))))
    key))

(def (fold-named-marker-key bytes from until prefix suffix ascii-ci?)
  (and (exact-integer? from) (exact-integer? until)
       (<= 0 from) (< from until) (<= until (u8vector-length bytes))
       (let (key
             (u8vector-append (string->utf8 prefix)
                              (subu8vector bytes from until)
                              (string->utf8 suffix)))
         (fold-normalize-marker key 0 (u8vector-length key) #f ascii-ci?))))

;; The map keys are complete normalized marker lines. This preserves the
;; existing prefix/name/suffix and whitespace rules while indexing candidate
;; names in one source scan, including documents with a different name per opener.
(def (fold-future-named-index bytes target-prefix parent-prefix
                              stop heading-marker heading-separator
                              indent? stop-at-heading? ascii-ci?
                              parent? stop-ascii-ci?)
  (let ((targets (make-hash-table))
        (parents (if (and parent? (not (eq? ascii-ci? stop-ascii-ci?)))
                   (make-hash-table)
                   #f))
        (target-keys '())
        (parent-keys '())
        (fixed-stops '())
        (headings '())
        (target-prefix-bytes (string->utf8 target-prefix))
        (parent-prefix-bytes (and parent? (string->utf8 parent-prefix))))
    (let scan ((cursor 0))
      (when (< cursor (u8vector-length bytes))
        (let* ((end (fold-future-line-end bytes cursor))
               (begin (if indent?
                        (let skip ((at cursor))
                          (if (and (< at end)
                                   (memv (u8vector-ref bytes at) '(9 32)))
                            (skip (+ at 1)) at))
                        cursor))
               (target-candidate?
                (compiler-u8vector-prefix?
                 bytes begin end target-prefix-bytes ascii-ci?))
               (parent-candidate?
                (and parent?
                     (compiler-u8vector-prefix?
                      bytes begin end parent-prefix-bytes stop-ascii-ci?))))
          (when (or target-candidate? (and (not parents) parent-candidate?))
            (set! target-keys
                  (compiler-position-index-add!
                   targets target-keys
                   (fold-normalize-marker bytes cursor end indent? ascii-ci?)
                   cursor)))
          (when (and parents parent-candidate?)
            (set! parent-keys
                  (compiler-position-index-add!
                   parents parent-keys
                   (fold-normalize-marker bytes cursor end indent?
                                          stop-ascii-ci?)
                   cursor)))
          (when (and stop
                     (fold-future-marker-line? bytes cursor end stop indent?))
            (set! fixed-stops (cons cursor fixed-stops)))
          (when (and stop-at-heading?
                     (fold-future-heading? bytes cursor end
                                           heading-marker heading-separator))
            (set! headings (cons cursor headings)))
          (scan end))))
    (compiler-position-index-freeze! targets target-keys)
    (when parents
      (compiler-position-index-freeze! parents parent-keys))
    (vector targets (or parents targets) (list->vector fixed-stops)
            (list->vector headings))))

;; Position vectors are descending because the source scan conses positions.
(def (fold-earlier-position left right)
  (cond
   ((not left) right)
   ((not right) left)
   (else (min left right))))

(def (fold-future-named-index-result index bytes from
                                     name-from name-until prefix suffix
                                     ascii-ci? stop-name-from stop-name-until
                                     stop-prefix stop-suffix stop-ascii-ci?)
  (let* ((target-key
          (fold-named-marker-key bytes name-from name-until
                                 prefix suffix ascii-ci?))
         (parent-key
          (and stop-name-from stop-name-until
               (fold-named-marker-key bytes stop-name-from stop-name-until
                                      stop-prefix stop-suffix stop-ascii-ci?)))
         (target (and target-key
                      (compiler-position-vector-next
                       (hash-get (vector-ref index 0) target-key) from)))
         (parent (and parent-key
                      (compiler-position-vector-next
                       (hash-get (vector-ref index 1) parent-key)
                       from)))
         (boundary
          (fold-earlier-position
           parent
           (fold-earlier-position
            (compiler-position-vector-next (vector-ref index 2) from)
            (compiler-position-vector-next (vector-ref index 3) from)))))
    (and target (or (not boundary) (< target boundary)))))

;; Each searched line receives the suffix result, so later queries start from
;; a known answer instead of scanning the same suffix again.
(def (fold-future-line-search bytes from cache classify)
  (def (finish result visited)
    (for-each (lambda (position)
                (hash-put! cache position (if result 1 2)))
              visited)
    result)
  (let search ((cursor from) (visited '()))
    (cond
     ((>= cursor (u8vector-length bytes)) (finish #f visited))
     ((hash-get cache cursor)
      (finish (= (hash-get cache cursor) 1) visited))
     (else
      (let* ((end (fold-future-line-end bytes cursor))
             (decision (classify cursor end))
             (visited (cons cursor visited)))
        (case decision
          ((match) (finish #t visited))
          ((stop) (finish #f visited))
          (else (search end visited))))))))

(def (fold-future-heading? bytes start end marker separator)
  (let run ((cursor start))
    (and (< cursor end)
         (if (= (u8vector-ref bytes cursor) marker)
           (run (+ cursor 1))
           (and (> cursor start)
                (= (u8vector-ref bytes cursor) separator))))))

(def (fold-heading-title-line? bytes start end marker separator
                               min-level title-bytes)
  (let (level
        (let count ((cursor start))
          (if (and (< cursor end)
                   (= (u8vector-ref bytes cursor) marker))
            (count (+ cursor 1)) (- cursor start))))
    (and (>= level min-level)
         (< (+ start level) end)
         (= (u8vector-ref bytes (+ start level)) separator)
         (let* ((begin
                 (let skip ((cursor (+ start level 1)))
                   (if (and (< cursor end)
                            (memv (u8vector-ref bytes cursor) '(9 32)))
                     (skip (+ cursor 1)) cursor)))
                (until
                 (let trim ((cursor end))
                   (if (and (> cursor begin)
                            (memv (u8vector-ref bytes (- cursor 1))
                                  '(9 10 13 32)))
                     (trim (- cursor 1)) cursor))))
           (and (= (- until begin) (u8vector-length title-bytes))
                (let compare ((cursor begin) (index 0))
                  (or (= index (u8vector-length title-bytes))
                      (and (= (u8vector-ref bytes cursor)
                              (u8vector-ref title-bytes index))
                           (compare (+ cursor 1) (+ index 1))))))))))

(def (fold-future-heading-title? bytes from marker separator min-level title
                                 cache)
  (let (title-bytes (string->utf8 title))
    (fold-future-line-search
     bytes from cache
     (lambda (start end)
       (if (fold-heading-title-line? bytes start end marker separator
                                     min-level title-bytes)
         'match 'continue)))))

(def (fold-future-heading-spec? expression)
  (and (= (length expression) 5)
       (string? (cadr expression))
       (string? (caddr expression))
       (not (equal? (cadr expression) (caddr expression)))
       (or (pair? (list-ref expression 3))
           (and (exact-integer? (list-ref expression 3))
                (> (list-ref expression 3) 0)))
       (string? (list-ref expression 4))
       (> (string-length (list-ref expression 4)) 0)
       (not (ormap (lambda (character)
                     (memv character '(#\newline #\return)))
                   (string->list (list-ref expression 4))))))

(def (fold-future-key-value-line? bytes start end marker)
  (let skip ((cursor start))
    (if (and (< cursor end) (memv (u8vector-ref bytes cursor) '(9 32)))
      (skip (+ cursor 1))
      (and (< cursor end) (= (u8vector-ref bytes cursor) marker)
           (let key ((cursor (+ cursor 1)) (key-start (+ cursor 1)))
             (and (< cursor end)
                  (let (byte (u8vector-ref bytes cursor))
                    (cond
                     ((= byte marker)
                      (if (or (= (+ cursor 1) end)
                              (memv (u8vector-ref bytes (+ cursor 1))
                                    '(9 10 13 32)))
                        (> cursor key-start)
                        (key (+ cursor 1) key-start)))
                     ((memv byte '(9 10 13 32)) #f)
                     (else (key (+ cursor 1) key-start))))))))))

(def (fold-future-marker-before-boundary? bytes from target stop
                                           heading-marker heading-separator
                                           indent? stop-at-heading? body-key-marker
                                           cache)
  (fold-future-line-search
   bytes from cache
   (lambda (cursor end)
     (cond
      ((and stop-at-heading?
            (fold-future-heading? bytes cursor end
                                  heading-marker heading-separator)) 'stop)
      ((and stop (fold-future-marker-line? bytes cursor end stop indent?))
       'stop)
      ((fold-future-marker-line? bytes cursor end target indent?) 'match)
      ((and body-key-marker
            (not (fold-future-key-value-line? bytes cursor end
                                              body-key-marker))) 'stop)
      (else 'continue)))))
