;;; -*- Gerbil -*-
;;; Prepared closed region plans. A single forward cursor and explicit frames
;;; own nested matching; callers declare delimiters, escaping and quote scope.
(import (only-in ./scan make-literal-end-scanner))
(export defregion-plan valid-region-specification? source-prefix-at? region-plan? prepare-region-plan region-plan-end
        region-plan-quote-end region-plan-pair-end region-plan-operator
        region-plan-specification prepare-region-source prepare-scoped-region-source region-source?
        region-source-pair-end region-source-quote-end)

(defstruct region-plan (data stops quotes pairs operators pair-index))

(defrules defregion-plan (stops quotes pairs consume-initial-stop)
  ((_ name (stops stop ...) (quotes quote-row ...) (pairs pair ...) (consume-initial-stop flag))
   (def name (prepare-region-plan '((stop ...) (quote-row ...) (pair ...) flag)))))

(def (copy-data value)
  (cond ((pair? value) (cons (copy-data (car value)) (copy-data (cdr value))))
        ((string? value) (string-copy value))
        (else value)))

(def (region-plan-specification plan) (copy-data (region-plan-data plan)))

(def (unique? rows key)
  (let loop ((rest rows) (seen '()))
    (or (null? rest)
        (let (value (key (car rest)))
          (and (not (member value seen))
               (loop (cdr rest) (cons value seen)))))))

(def (valid-region-specification? spec)
  (and (list? spec) (= (length spec) 4)
       (list? (car spec)) (pair? (car spec))
       (andmap (lambda (text) (and (string? text) (positive? (string-length text)))) (car spec))
       (unique? (car spec) (lambda (value) value))
       (list? (cadr spec)) (list? (caddr spec)) (boolean? (cadddr spec))
       (andmap
        (lambda (row)
          (and (list? row) (= (length row) 4) (string? (car row))
               (positive? (string-length (car row))) (char? (cadr row))
               (char? (caddr row)) (exact-integer? (cadddr row))
               (<= 1 (cadddr row) (string-length (car row)))
               (char=? (string-ref (car row) (- (string-length (car row)) 1)) (cadr row))))
        (caddr spec))
       (unique? (caddr spec) car)
       (andmap
        (lambda (row)
          (and (list? row) (= (length row) 3) (char? (car row))
               (boolean? (cadr row)) (list? (caddr row))
               (andmap (lambda (prefix) (and (string? prefix) (assoc prefix (caddr spec)))) (caddr row))
               (unique? (caddr row) (lambda (value) value))))
        (cadr spec))
       (unique? (cadr spec) car)))

(def (prepare-pair-index pairs)
  ;; Buckets retain the owned declaration rows and their stable ordering.
  ;; Most input characters have no opener candidate; they need no prefix walk.
  (let ((ascii (make-vector 128 '())) (wide (make-hash-table-eqv)))
    (for-each
     (lambda (row)
       (let (code (char->integer (string-ref (car row) 0)))
         (if (< code 128)
           (vector-set! ascii code (cons row (vector-ref ascii code)))
           (hash-put! wide code (cons row (or (hash-get wide code) '()))))))
     (reverse pairs))
    (cons ascii wide)))

(def (prepare-region-plan spec)
  (unless (valid-region-specification? spec) (error "invalid closed region plan" spec))
  (let (owned (copy-data spec))
    (make-region-plan owned (car owned) (cadr owned) (caddr owned)
                      (make-literal-end-scanner (car owned))
                      (prepare-pair-index (caddr owned)))))

(def (source-prefix-at? source at text)
  (let ((width (string-length text)) (limit (string-length source)))
    (and (exact-integer? at) (<= 0 at) (<= (+ at width) limit)
         (let loop ((index 0))
           (or (= index width)
               (and (char=? (string-ref text index) (string-ref source (+ at index)))
                    (loop (+ index 1))))))))

(def (region-plan-operator plan source start)
  (let (end ((region-plan-operators plan) source start))
    (and end (substring source start end))))

(def (pair-at plan source at (allowed #f))
  (and (not (null? allowed)) (exact-integer? at) (<= 0 at) (< at (string-length source))
       (let* ((index (region-plan-pair-index plan))
              (code (char->integer (string-ref source at)))
              (rows (if (< code 128) (vector-ref (car index) code)
                      (or (hash-get (cdr index) code) '()))))
         (let loop ((rest rows) (selected #f))
           (if (null? rest) selected
             (let (row (car rest))
               (loop (cdr rest)
                 (if (and (or (not allowed) (member (car row) allowed))
                          (source-prefix-at? source at (car row))
                          (or (not selected)
                              (> (string-length (car row)) (string-length (car selected)))))
                   row selected))))))))

;;; Frames are immutable quote rows or pair rows with a current depth. There
;;; is no recursive call on source nesting and no source-prefix substring.
(def (cached-pair-end ends start row)
  (and ends (let (entries (hash-get (vector-ref ends 1) row)) (and entries (hash-get entries start)))))
(def (cache-pair-end! ends start row end)
  (let (entries (or (hash-get (vector-ref ends 1) row)
                   (let (table (make-hash-table-eqv)) (hash-put! (vector-ref ends 1) row table) table)))
    (hash-put! entries start end)))
(def (pair-frame row depth start scopes)
  (vector 'pair row depth start (let (entry (assoc (car row) scopes)) (and entry (cdr entry)))))
(def (region-match plan source start frames word? (ends #f) (scopes '()))
  (def (close! frame end)
    (when ends
      (if (eq? (vector-ref frame 0) 'quote)
        (hash-put! (vector-ref ends 0) (vector-ref frame 2) end)
        (cache-pair-end! ends (vector-ref frame 3) (vector-ref frame 1) end))))
  (let (limit (string-length source))
    (let loop ((at start) (stack frames))
      (cond
       ((= at limit)
        (if (null? stack) at (error "unterminated declared region" start)))
       ((pair? stack)
        (let* ((frame (car stack)) (row (vector-ref frame 1))
               (ch (string-ref source at)) (quote? (eq? (vector-ref frame 0) 'quote))
               (nested (pair-at plan source at (if quote? (caddr row) (vector-ref frame 4)))))
          (cond
           ((and (char=? ch #\\) (or (not quote?) (cadr row)))
            (loop (min limit (+ at 2)) stack))
           ((and quote? (char=? ch (car row)))
            (close! frame (+ at 1))
            (if (and (null? (cdr stack)) (not word?)) (+ at 1)
              (loop (+ at 1) (cdr stack))))
           ((and (not quote?) (assv ch (region-plan-quotes plan))) =>
            (lambda (quote-row) (loop (+ at 1) (cons (vector 'quote quote-row at) stack))))
           ((and nested (cached-pair-end ends at nested)) =>
            (lambda (after) (loop after stack)))
           (nested
            (loop (+ at (string-length (car nested)))
                  (cons (pair-frame nested (cadddr nested) at scopes) stack)))
           ((and (not quote?) (char=? ch (cadr row)))
            (loop (+ at 1) (cons (vector 'pair row (+ (vector-ref frame 2) 1) (vector-ref frame 3) (vector-ref frame 4)) (cdr stack))))
           ((and (not quote?) (char=? ch (caddr row)))
            (if (= (vector-ref frame 2) 1)
              (begin
                (close! frame (+ at 1))
                (if (and (null? (cdr stack)) (not word?)) (+ at 1)
                  (loop (+ at 1) (cdr stack))))
              (loop (+ at 1) (cons (vector 'pair row (- (vector-ref frame 2) 1) (vector-ref frame 3) (vector-ref frame 4)) (cdr stack)))))
           (else (loop (+ at 1) stack)))))
       ((not word?) at)
       (else
        (let* ((ch (string-ref source at)) (nested (pair-at plan source at))
               (quote-row (assv ch (region-plan-quotes plan))))
          (cond
           ;; Declared region openers shield overlapping operator stops.
           (nested (loop (+ at (string-length (car nested)))
                         (list (pair-frame nested (cadddr nested) at scopes))))
           ((and (or (> at start) (not (cadddr (region-plan-data plan))))
                 (or (char-whitespace? ch) ((region-plan-operators plan) source at)))
            (and (> at start) at))
           ((char=? ch #\\) (loop (min limit (+ at 2)) '()))
           (quote-row (loop (+ at 1) (list (vector 'quote quote-row at))))
           (else (loop (+ at 1) '())))))))))

(def (region-plan-end plan source start)
  (and (exact-integer? start) (<= 0 start) (< start (string-length source))
       (region-match plan source start '() #t)))

(def (region-plan-quote-end plan source start delimiter)
  (let (row (assv delimiter (region-plan-quotes plan)))
    (unless (and row (exact-integer? start) (<= 0 start) (< start (string-length source))
                 (char=? delimiter (string-ref source start)))
      (error "invalid declared quote entry" start delimiter))
    (region-match plan source (+ start 1) (list (vector 'quote row start)) #f)))

(def (region-plan-pair-end plan source start)
  (let (row (pair-at plan source start))
    (unless row (error "missing declared region opener" start))
    (region-match plan source (+ start (string-length (car row)))
                  (list (pair-frame row (cadddr row) start '())) #f)))

;;; A source-local boundary cache records every nested frame closed by a match.
;;; Decomposing an outer region subsequently visits its children by lookup,
;;; rather than walking each suffix again. Literal quote characters outside a
;;; requested region are never eagerly interpreted (e.g. here-document text).
(defstruct region-source (plan text ends scopes))
(def (prepare-region-source plan text)
  (prepare-scoped-region-source plan text '()))
(def (prepare-scoped-region-source plan text scopes)
  (unless (and (region-plan? plan) (string? text))
    (error "invalid region source" plan text))
  (unless (and (list? scopes)
               (andmap (lambda (entry)
                         (and (list? entry) (pair? entry) (assoc (car entry) (region-plan-pairs plan))
                              (andmap (lambda (prefix) (assoc prefix (region-plan-pairs plan))) (cdr entry)))) scopes)
               (unique? scopes car)) (error "invalid region pair scopes"))
  (make-region-source plan (string-copy text)
                      (vector (make-hash-table-eqv) (make-hash-table-eq)) (copy-data scopes)))

(def (region-source-quote-end source start delimiter)
  (let* ((plan (region-source-plan source)) (text (region-source-text source))
         (row (assv delimiter (region-plan-quotes plan))))
    (unless (and row (exact-integer? start) (<= 0 start) (< start (string-length text))
                 (char=? delimiter (string-ref text start)))
      (error "invalid declared quote entry" start delimiter))
    (or (hash-get (vector-ref (region-source-ends source) 0) start)
        (region-match plan text (+ start 1) (list (vector 'quote row start)) #f
                      (region-source-ends source) (region-source-scopes source)))))

(def (region-source-pair-end source start)
  (let* ((plan (region-source-plan source)) (text (region-source-text source))
         (row (and (exact-integer? start) (pair-at plan text start))))
    (unless row (error "missing declared region opener" start))
    (or (cached-pair-end (region-source-ends source) start row)
        (region-match plan text (+ start (string-length (car row)))
                      (list (pair-frame row (cadddr row) start (region-source-scopes source))) #f
                      (region-source-ends source) (region-source-scopes source)))))
