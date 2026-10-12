;;; Diagnostic products only. Ordinary generated products do not import this.
(import :std/os/time)
(export current-fold-cost-session make-fold-cost-session
        call-with-fold-cost fold-cost-count! fold-cost-rows)

;; Session = private aggregation table and current frame. Frames never escape
;; a synchronous call. Nested parsers and exceptions restore their caller.
(def current-fold-cost-session (make-parameter #f))
(def (make-fold-cost-session) (vector (make-hash-table-eq) #f))
(def (clock-ns)
  (let (time (current-system-time-precise))
    (+ (* (timespec-tv_sec time) 1000000000) (timespec-tv_nsec time))))

;; Count-only products avoid timing thunks and frame allocation at every leaf.
;; They still incur table updates and are never admitted as timing products.
(def (fold-cost-count! name)
  (let (session (current-fold-cost-session))
    (when session
      (let* ((table (vector-ref session 0))
             (row (or (hash-get table name)
                      (let (created (vector 0 0 0 0))
                        (hash-put! table name created) created))))
        (vector-set! row 0 (+ 1 (vector-ref row 0)))))))

(def (call-with-fold-cost name thunk)
  (let (session (current-fold-cost-session))
    (if (not session) (thunk)
      (let* ((parent (vector-ref session 1))
             (frame (vector (clock-ns) 0)))
        (def (finish completed?)
          (let* ((elapsed (- (clock-ns) (vector-ref frame 0)))
                 (children (vector-ref frame 1))
                 (table (vector-ref session 0))
                 (row (or (hash-get table name)
                          (let (created (vector 0 0 0 0))
                            (hash-put! table name created) created))))
            (vector-set! session 1 parent)
            (when parent
              (vector-set! parent 1 (+ (vector-ref parent 1) elapsed)))
            (unless (<= 0 children elapsed)
              (error "invalid diagnostic Fold clock interval" name elapsed children))
            (vector-set! row 0 (+ 1 (vector-ref row 0)))
            (vector-set! row 1 (+ elapsed (vector-ref row 1)))
            (vector-set! row 2 (+ (- elapsed children) (vector-ref row 2)))
            (unless completed? (vector-set! row 3 (+ 1 (vector-ref row 3))))))
        (vector-set! session 1 frame)
        (let (value (with-catch
                      (lambda (exception) (finish #f) (raise exception)) thunk))
          (finish #t) value)))))

(def (fold-cost-rows session)
  (unless (not (vector-ref session 1)) (error "Fold diagnostic frame still active"))
  (let (rows '())
    (hash-for-each
     (lambda (name row)
       (set! rows (cons (list name (vector-ref row 0)
                             (/ (vector-ref row 1) 1000.0)
                             (/ (vector-ref row 2) 1000.0)
                             (vector-ref row 3)) rows)))
     (vector-ref session 0))
    (list-sort (lambda (left right) (> (list-ref left 3) (list-ref right 3))) rows)))
