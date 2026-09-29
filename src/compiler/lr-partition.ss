;;; -*- Gerbil -*-
;;; Conflict-preserving LR(1) quotient using standard tables and growable vectors.

(import (only-in :std/hash/misc hash-ensure-ref)
        (only-in :std/vector/extensible
                 list->ExtensibleVector ExtensibleVector-fill-pointer
                 ExtensibleVector-push! ExtensibleVector-ref)
        (only-in ./funcs
                 compiler-index-set-for-each compiler-index-set-union)
        (only-in ./lr terminal-symbol?)
        (only-in ./lr-lookahead build-states-via-canonical-lr1))
(export build-states-via-partitioned-lr1)

;; One bit names each raw parser action. Shift destinations are tested by the
;; transition-congruence pass; states with the same LR(0) core have identical
;; shift terminals and precedence-bearing shift productions.
(def (raw-action-masks states lookaheads offsets state-count terminal-values
                       terminal-index layout core-symbols)
  (let (result (make-vector state-count #f))
    (let state-loop ((state 0))
      (when (< state state-count)
        (let ((masks (make-vector (vector-length terminal-values) 0))
              (node (vector-ref offsets state)))
          (for-each
           (lambda (core)
             (let ((symbol (vector-ref core-symbols core))
                   (production-id (quotient core (cdr layout))))
               (cond
                ((and symbol (terminal-symbol? symbol))
                 (let (terminal-id (table-ref terminal-index symbol))
                   (vector-set! masks terminal-id
                                (bitwise-ior
                                 1 (vector-ref masks terminal-id)))))
                ((not symbol)
                 (let (action-bit
                       (if (zero? production-id)
                         2
                         (arithmetic-shift 1 (+ production-id 2))))
                   (compiler-index-set-for-each
                    (vector-ref lookaheads node)
                    (lambda (terminal-id)
                      (vector-set!
                       masks terminal-id
                       (bitwise-ior action-bit
                                    (vector-ref masks terminal-id))))))))
               (set! node (+ node 1))))
           (vector-ref states state))
          (vector-set! result state masks))
        (state-loop (+ state 1))))
    result))

(def (single-action? mask)
  (zero? (bitwise-and mask (- mask 1))))

;; A merged cell with multiple actions must be an action set that one of its
;; canonical members already had. Thus packing introduces no new conflict.
(def (safe-to-add? members union-actions candidate action-masks terminal-count)
  (let terminal-loop ((terminal 0))
    (if (= terminal terminal-count)
      #t
      (let* ((candidate-action
              (vector-ref (vector-ref action-masks candidate) terminal))
             (combined
              (bitwise-ior (vector-ref union-actions terminal)
                           candidate-action)))
        (and (or (single-action? combined)
                 (= candidate-action combined)
                 (let member-loop ((index 0))
                   (and (< index (ExtensibleVector-fill-pointer members))
                        (or (= combined
                               (vector-ref
                                (vector-ref
                                 action-masks
                                 (ExtensibleVector-ref members index))
                                terminal))
                            (member-loop (+ index 1))))))
             (terminal-loop (+ terminal 1)))))))

(def (make-group state action-masks)
  (let (members (list->ExtensibleVector '()))
    (ExtensibleVector-push! members state)
    (vector members
            (vector-copy (vector-ref action-masks state)))))

(def (pack-canonical-states states action-masks state-count terminal-count)
  (let ((groups (list->ExtensibleVector '()))
        (core-groups (make-table test: equal?))
        (ids (make-vector state-count 0)))
    (let state-loop ((state 0))
      (when (< state state-count)
        (let* ((candidates
                (hash-ensure-ref
                 core-groups (vector-ref states state)
                 (lambda () (list->ExtensibleVector '()))))
               (admitted #f))
          (let candidate-loop ((index 0))
            (when (and (not admitted)
                       (< index (ExtensibleVector-fill-pointer candidates)))
              (let* ((group-id (ExtensibleVector-ref candidates index))
                     (group (ExtensibleVector-ref groups group-id))
                     (members (vector-ref group 0))
                     (union-actions (vector-ref group 1)))
                (if (safe-to-add? members union-actions state action-masks
                                  terminal-count)
                  (begin
                    (set! admitted group-id)
                    (ExtensibleVector-push! members state)
                    (let union-loop ((terminal 0))
                      (when (< terminal terminal-count)
                        (vector-set!
                         union-actions terminal
                         (bitwise-ior
                          (vector-ref union-actions terminal)
                          (vector-ref (vector-ref action-masks state)
                                      terminal)))
                        (union-loop (+ terminal 1)))))
                  (candidate-loop (+ index 1)))))
          (unless admitted
            (set! admitted
                  (ExtensibleVector-push!
                   groups (make-group state action-masks)))
            (ExtensibleVector-push! candidates admitted))
          (vector-set! ids state admitted))
        (state-loop (+ state 1))))
    (values groups ids))))

(def (index-transitions transitions state-count)
  (let (rows (make-vector state-count '()))
    (for-each
     (lambda (row)
       (let (state (car row))
         (vector-set! rows state
                      (cons (cons (cadr row) (caddr row))
                            (vector-ref rows state)))))
     transitions)
    (let loop ((state 0))
      (when (< state state-count)
        (vector-set! rows state (reverse (vector-ref rows state)))
        (loop (+ state 1))))
    rows))

(def (transition-signature rows state ids)
  (map (lambda (row) (cons (car row) (vector-ref ids (cdr row))))
       (vector-ref rows state)))

;; Refine by transition targets until every quotient edge has one destination.
;; The lookup table and ExtensibleVector handle grouping without list copying.
(def (refine-partition ids rows state-count)
  (let ((next (list->ExtensibleVector '()))
        (next-ids (make-vector state-count 0))
        (index (make-table test: equal?)))
    (let state-loop ((state 0))
      (when (< state state-count)
        (let* ((key (cons (vector-ref ids state)
                          (transition-signature rows state ids)))
               (group-id (table-ref index key #f)))
          (unless group-id
            (set! group-id
                  (ExtensibleVector-push!
                   next (list->ExtensibleVector '())))
            (table-set! index key group-id))
          (ExtensibleVector-push!
           (ExtensibleVector-ref next group-id) state)
          (vector-set! next-ids state group-id))
        (state-loop (+ state 1))))
    (values next next-ids)))

(def (materialize-partition groups ids states lookaheads offsets rows
                            terminal-values layout core-symbols
                            canonical-visits)
  (let* ((count (ExtensibleVector-fill-pointer groups))
         (merged-states (make-vector count '()))
         (merged-offsets (make-vector (+ count 1) 0))
         (merged-transitions '())
         (node-count 0))
    (let group-loop ((group-id 0))
      (when (< group-id count)
        (let* ((members (ExtensibleVector-ref groups group-id))
               (first (ExtensibleVector-ref members 0))
               (cores (vector-ref states first)))
          (vector-set! merged-states group-id cores)
          (vector-set! merged-offsets group-id node-count)
          (set! node-count (+ node-count (length cores)))
          (for-each
           (lambda (row)
             (set! merged-transitions
                   (cons (list group-id (car row)
                               (vector-ref ids (cdr row)))
                         merged-transitions)))
           (vector-ref rows first)))
        (group-loop (+ group-id 1))))
    (vector-set! merged-offsets count node-count)
    (let (merged-lookaheads (make-vector node-count 0))
      (let group-loop ((group-id 0))
        (when (< group-id count)
          (let* ((members (ExtensibleVector-ref groups group-id))
                 (first (ExtensibleVector-ref members 0))
                 (item-count (length (vector-ref states first))))
            (let item-loop ((item 0))
              (when (< item item-count)
                (let ((target (+ (vector-ref merged-offsets group-id)
                                 item))
                      (mask 0))
                  (let member-loop ((member 0))
                    (when (< member
                             (ExtensibleVector-fill-pointer members))
                      (let (state (ExtensibleVector-ref members member))
                        (set! mask
                              (compiler-index-set-union
                               mask
                               (vector-ref
                                lookaheads
                                (+ (vector-ref offsets state) item)))))
                      (member-loop (+ member 1))))
                  (vector-set! merged-lookaheads target mask))
                (item-loop (+ item 1)))))
          (group-loop (+ group-id 1))))
      (values merged-states count merged-lookaheads merged-offsets
              (reverse merged-transitions) terminal-values layout core-symbols
              canonical-visits node-count))))

(def (build-states-via-partitioned-lr1 productions table first nullable)
  (let-values (((states state-count lookaheads offsets transitions
                        terminal-values layout core-symbols
                        canonical-visits item-visits)
                (build-states-via-canonical-lr1
                 productions table first nullable)))
    (let ((terminal-index (make-table test: equal?))
          (rows (index-transitions transitions state-count)))
      (let index-loop ((terminal 0))
        (when (< terminal (vector-length terminal-values))
          (table-set! terminal-index
                      (vector-ref terminal-values terminal) terminal)
          (index-loop (+ terminal 1))))
      (let (action-masks
            (raw-action-masks
             states lookaheads offsets state-count terminal-values
             terminal-index layout core-symbols))
        (let-values (((initial-groups initial-ids)
                      (pack-canonical-states
                       states action-masks state-count
                       (vector-length terminal-values))))
          (let refine ((groups initial-groups) (ids initial-ids))
            (let-values (((next next-ids)
                          (refine-partition ids rows state-count)))
              (if (= (ExtensibleVector-fill-pointer groups)
                     (ExtensibleVector-fill-pointer next))
                (materialize-partition
                 next next-ids states lookaheads offsets rows terminal-values
                 layout core-symbols canonical-visits)
                (refine next next-ids)))))))))
