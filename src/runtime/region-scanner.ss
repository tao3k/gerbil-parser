;;; -*- Gerbil -*-
;;; Prepared closed region plans. A single forward cursor and explicit frames
;;; own nested matching; callers declare delimiters, escaping and quote scope.
(import (only-in ./scan make-literal-end-scanner))
(export defregion-plan valid-region-specification? source-prefix-at? region-plan? prepare-region-plan region-plan-end
        region-plan-quote-end region-plan-pair-end region-plan-operator
        region-plan-specification prepare-region-source region-source?
        region-source-pair-end region-source-quote-end)

(defstruct region-plan (data stops quotes pairs operators))

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

(def (prepare-region-plan spec)
  (unless (valid-region-specification? spec) (error "invalid closed region plan" spec))
  (let (owned (copy-data spec))
    (make-region-plan owned (car owned) (cadr owned) (caddr owned)
                      (make-literal-end-scanner (car owned)))))

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
  (foldl
   (lambda (row selected)
     (if (and (or (not allowed) (member (car row) allowed))
              (source-prefix-at? source at (car row))
              (or (not selected) (> (string-length (car row)) (string-length (car selected)))))
       row selected)) #f (region-plan-pairs plan)))

;;; Frames are immutable quote rows or pair rows with a current depth. There
;;; is no recursive call on source nesting and no source-prefix substring.
(def (region-match plan source start frames word? (ends #f))
  (def (close! frame end)
    (when ends
      (let (quote? (eq? (vector-ref frame 0) 'quote))
        (hash-put! (vector-ref ends (if quote? 0 1))
                   (vector-ref frame (if quote? 2 3)) end))))
  (let (limit (string-length source))
    (let loop ((at start) (stack frames))
      (cond
       ((= at limit)
        (if (null? stack) at (error "unterminated declared region" start)))
       ((pair? stack)
        (let* ((frame (car stack)) (row (vector-ref frame 1))
               (ch (string-ref source at)) (quote? (eq? (vector-ref frame 0) 'quote))
               (nested (pair-at plan source at (if quote? (caddr row) #f))))
          (cond
           ((and (char=? ch #\\) (or (not quote?) (cadr row)))
            (loop (min limit (+ at 2)) stack))
           ((and quote? (char=? ch (car row)))
            (close! frame (+ at 1))
            (if (and (null? (cdr stack)) (not word?)) (+ at 1)
              (loop (+ at 1) (cdr stack))))
           ((and (not quote?) (assv ch (region-plan-quotes plan))) =>
            (lambda (quote-row) (loop (+ at 1) (cons (vector 'quote quote-row at) stack))))
           (nested
            (loop (+ at (string-length (car nested)))
                  (cons (vector 'pair nested (cadddr nested) at) stack)))
           ((and (not quote?) (char=? ch (cadr row)))
            (loop (+ at 1) (cons (vector 'pair row (+ (vector-ref frame 2) 1) (vector-ref frame 3)) (cdr stack))))
           ((and (not quote?) (char=? ch (caddr row)))
            (if (= (vector-ref frame 2) 1)
              (begin
                (close! frame (+ at 1))
                (if (and (null? (cdr stack)) (not word?)) (+ at 1)
                  (loop (+ at 1) (cdr stack))))
              (loop (+ at 1) (cons (vector 'pair row (- (vector-ref frame 2) 1) (vector-ref frame 3)) (cdr stack)))))
           (else (loop (+ at 1) stack)))))
       ((not word?) at)
       (else
        (let* ((ch (string-ref source at)) (nested (pair-at plan source at))
               (quote-row (assv ch (region-plan-quotes plan))))
          (cond
           ;; Declared region openers shield overlapping operator stops.
           (nested (loop (+ at (string-length (car nested)))
                         (list (vector 'pair nested (cadddr nested) at))))
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
                  (list (vector 'pair row (cadddr row) start)) #f)))

;;; A source-local boundary cache records every nested frame closed by a match.
;;; Decomposing an outer region subsequently visits its children by lookup,
;;; rather than walking each suffix again. Literal quote characters outside a
;;; requested region are never eagerly interpreted (e.g. here-document text).
(defstruct region-source (plan text ends))
(def (prepare-region-source plan text)
  (unless (and (region-plan? plan) (string? text))
    (error "invalid region source" plan text))
  (make-region-source plan (string-copy text)
                      (vector (make-hash-table-eqv) (make-hash-table-eqv))))

(def (region-source-quote-end source start delimiter)
  (let* ((plan (region-source-plan source)) (text (region-source-text source))
         (row (assv delimiter (region-plan-quotes plan))))
    (unless (and row (exact-integer? start) (<= 0 start) (< start (string-length text))
                 (char=? delimiter (string-ref text start)))
      (error "invalid declared quote entry" start delimiter))
    (or (hash-get (vector-ref (region-source-ends source) 0) start)
        (region-match plan text (+ start 1) (list (vector 'quote row start)) #f
                      (region-source-ends source)))))

(def (region-source-pair-end source start)
  (let* ((plan (region-source-plan source)) (text (region-source-text source))
         (row (and (exact-integer? start) (pair-at plan text start))))
    (unless row (error "missing declared region opener" start))
    (or (hash-get (vector-ref (region-source-ends source) 1) start)
        (region-match plan text (+ start (string-length (car row)))
                      (list (vector 'pair row (cadddr row) start)) #f
                      (region-source-ends source)))))
