;;; -*- Gerbil -*-
;;; Direct LR(0)-derived LR(1) NFA partition and determinization.

(import (prefix-in :std/struct/queue stdq-)
        (only-in :std/vector/extensible
                 list->ExtensibleVector ExtensibleVector->vector
                 ExtensibleVector-fill-pointer ExtensibleVector-push!
                 ExtensibleVector-ref ExtensibleVector-set!)
        (only-in ./funcs
                 compiler-index-set-add compiler-index-set-for-each
                 compiler-index-set-singleton compiler-index-set-union)
        (only-in ./lr
                 +lr-eof+ nonterminal-name nonterminal-symbol?
                 production-id production-index-by-lhs production-rhs
                 production-terminal-catalog sequence-first sequence-nullable?)
        (only-in ./lr-automaton
                 make-core-item make-core-symbol-catalog make-item-layout
                 materialize-transitions)
        (only-in ./lr-conflict-candidates
                 initial-backward-follow-partitions/from-lr0
                 raw-conflict-cells)
        (only-in ./lr-lookahead
                 build-states-via-lr0 build-states-via-canonical-lr1))
(export build-states-via-follow-partition-lr1)

(def (make-follow-core-metadata productions table first nullable
                                layout core-symbols terminal-index)
  (let ((result (make-vector (vector-length core-symbols) #f))
        (by-lhs (production-index-by-lhs productions)))
    (let production-loop ((id 0))
      (when (< id (vector-length table))
        (let dot-loop ((tail (production-rhs (vector-ref table id)))
                       (dot 0))
          (let* ((core (make-core-item id dot layout))
                 (symbol (vector-ref core-symbols core)))
            (if (and symbol (nonterminal-symbol? symbol))
              (let ((after (cdr tail)))
                (vector-set!
                 result core
                 (vector
                  symbol
                  (foldl (lambda (terminal mask)
                           (compiler-index-set-add
                            mask (table-ref terminal-index terminal)))
                         0 (sequence-first after first nullable))
                  (sequence-nullable? after nullable)
                  (map (lambda (child)
                         (make-core-item (production-id child) 0 layout))
                       (table-ref by-lhs (nonterminal-name symbol) '())))))
              (vector-set! result core (vector symbol 0 #f '())))
            (unless (null? tail)
              (dot-loop (cdr tail) (+ dot 1)))))
        (production-loop (+ id 1))))
    result))

;; Each block is (core . follow-mask). The index maps one (core, follow)
;; vertex to its current block; no canonical LR(1) DFA state is constructed.
(def (index-follow-blocks partitions terminal-count)
  (let ((blocks (list->ExtensibleVector '()))
        (index (make-vector (vector-length partitions) #f)))
    (let core-loop ((core 0))
      (when (< core (vector-length partitions))
        (let (parts (vector-ref partitions core))
          (when parts
            (let (row (make-vector terminal-count #f))
              (for-each
               (lambda (part)
                 (let ((id (ExtensibleVector-push!
                            blocks (cons core (cdr part)))))
                   (compiler-index-set-for-each
                    (cdr part)
                    (lambda (lookahead)
                      (vector-set! row lookahead id)))))
               parts)
              (vector-set! index core row))))
        (core-loop (+ core 1))))
    (values (ExtensibleVector->vector blocks) index)))

(def (follow-block-ref index core lookahead)
  (let ((row (vector-ref index core)))
    (unless row
      (error "missing dotted core in follow partition" core))
    (let (block (vector-ref row lookahead))
      (unless block
        (error "missing follow vertex in partition" core lookahead))
      block)))

;; Return the labeled shift target and the epsilon target set of one NFA
;; vertex. For k=1, FIRST(tail follow) is a cached FIRST mask plus the current
;; follow bit when the tail is nullable.
(def (follow-successors core lookahead metadata index)
  (let* ((item (vector-ref metadata core))
         (symbol (vector-ref item 0))
         (shift (and symbol
                     (follow-block-ref index (+ core 1) lookahead)))
         (closure-mask
          (if (null? (vector-ref item 3))
            0
            (let ((child-follows
                   (if (vector-ref item 2)
                     (compiler-index-set-add
                      (vector-ref item 1) lookahead)
                     (vector-ref item 1))))
              (foldl
               (lambda (child mask)
                 (let (next mask)
                   (compiler-index-set-for-each
                    child-follows
                    (lambda (terminal)
                      (set! next
                            (compiler-index-set-add
                             next
                             (follow-block-ref index child terminal)))))
                   next))
               0 (vector-ref item 3))))))
    (cons shift closure-mask)))

;; Backward refinement: vertices in a block must agree on which target blocks
;; they can reach by a shift or epsilon edge. Splitting is monotone and stops
;; because there are finitely many reachable (core, follow) pairs.
(def (refine-follow-blocks partitions metadata terminal-count)
  ;; A split can affect only cores whose shift or closure points to it. Keep
  ;; that reverse dependency graph at dotted-core granularity: the follow
  ;; block IDs are rebuilt each round, but unrelated cores need no new
  ;; successor signatures.
  (let ((dependents (make-vector (vector-length partitions) '())))
    (let core-loop ((core 0))
      (when (< core (vector-length partitions))
        (when (vector-ref partitions core)
          (let (item (vector-ref metadata core))
            (when (vector-ref item 0)
              (let (target (+ core 1))
                (vector-set! dependents target
                             (cons core (vector-ref dependents target)))))
            (for-each
             (lambda (target)
               (vector-set! dependents target
                            (cons core (vector-ref dependents target))))
             (vector-ref item 3))))
        (core-loop (+ core 1))))
  (let refine ((current partitions)
               (active (make-vector (vector-length partitions) #t))
               (rounds 0))
    (let-values (((blocks index)
                  (index-follow-blocks current terminal-count)))
      (let ((next (make-vector (vector-length current) #f))
            (next-active (make-vector (vector-length current) #f))
            (changed? #f))
        (let core-loop ((core 0))
          (when (< core (vector-length current))
            (let (parts (vector-ref current core))
              (if (and parts (vector-ref active core))
                (let (new-parts '())
                  (for-each
                   (lambda (part)
                     (if (and (positive? (cdr part))
                              (zero? (bitwise-and (cdr part)
                                                  (- (cdr part) 1))))
                       ;; A one-lookahead block cannot split, even when a
                       ;; successor block changed in the previous round.
                       (set! new-parts (cons part new-parts))
                       (let ((groups (make-table test: equal?))
                             (keys '()))
                         (compiler-index-set-for-each
                          (cdr part)
                          (lambda (lookahead)
                            (let* ((key
                                    (follow-successors
                                     core lookahead metadata index))
                                   (known (table-ref groups key #f)))
                              (unless known (set! keys (cons key keys)))
                              (table-set!
                               groups key
                               (compiler-index-set-add
                                (or known 0) lookahead)))))
                         (when (pair? (cdr keys))
                           (set! changed? #t)
                           (for-each
                            (lambda (dependent)
                              (vector-set! next-active dependent #t))
                            (vector-ref dependents core)))
                         (for-each
                          (lambda (key)
                            (set! new-parts
                                  (cons (cons key (table-ref groups key))
                                        new-parts)))
                          (reverse keys)))))
                   parts)
                  (vector-set! next core (reverse new-parts)))
                (vector-set! next core parts)))
            (core-loop (+ core 1))))
        (if changed?
          (refine next next-active (+ rounds 1))
          (values next blocks index rounds)))))))

;; The backward blocks are the atomic vertices for the forward pass. Begin
;; with one group per dotted core (and a singleton start), then split by the
;; predecessor groups that can reach each vertex on each edge label.
(def (forward-refine-blocks blocks shifts epsilons start core-symbols)
  (let* ((count (vector-length blocks))
         (predecessors (make-vector count '())))
    (let ((label-index (make-table test: equal?))
          (next-label 1))
      (let source-loop ((source 0))
        (when (< source count)
          (let ((shift (vector-ref shifts source))
                (symbol (vector-ref
                         core-symbols (car (vector-ref blocks source)))))
            (when shift
              (let (label (table-ref label-index symbol #f))
                (unless label
                  (set! label next-label)
                  (set! next-label (+ next-label 1))
                  (table-set! label-index symbol label))
                (vector-set!
                 predecessors shift
                 (cons (cons label source)
                       (vector-ref predecessors shift)))))
            (compiler-index-set-for-each
             (vector-ref epsilons source)
             (lambda (target)
               (vector-set!
                predecessors target
                (cons (cons 0 source)
                      (vector-ref predecessors target)))))
            (source-loop (+ source 1))))))
      (def (group-ids key-procedure)
        (let ((ids (make-vector count 0))
              (keys (make-table test: equal?))
              (group-count 0))
          (let block-loop ((block 0))
            (when (< block count)
              (let* ((key (key-procedure block))
                     (found (table-ref keys key #f)))
                (unless found
                  (set! found group-count)
                  (table-set! keys key found)
                  (set! group-count (+ group-count 1)))
                (vector-set! ids block found))
              (block-loop (+ block 1))))
          (values ids group-count)))
      (def (predecessor-signature block ids)
        (let ((sources (make-table test: eq?))
              (labels '()))
          (for-each
           (lambda (edge)
             (let* ((label (car edge))
                    (source-group (vector-ref ids (cdr edge)))
                    (known (table-ref sources label #f)))
               (unless known
                 (set! labels (cons label labels)))
               (table-set!
                sources label
                (compiler-index-set-add (or known 0) source-group))))
           (vector-ref predecessors block))
          (map (lambda (label)
                 (cons label (table-ref sources label)))
               (list-sort < labels))))
      ;; Once a group has one block, its predecessor signature cannot split
      ;; it. Preserve first-seen group numbering while constructing keys only
      ;; for groups that can still split.
      (def (refine-group-ids ids group-count)
        (let ((sizes (make-vector group-count 0))
              (next-ids (make-vector count 0))
              (keys (make-table test: equal?))
              (next-count 0))
          (let count-loop ((block 0))
            (when (< block count)
              (let (group (vector-ref ids block))
                (vector-set! sizes group (+ 1 (vector-ref sizes group))))
              (count-loop (+ block 1))))
          (let block-loop ((block 0))
            (when (< block count)
              (let* ((group (vector-ref ids block))
                     (singleton? (= (vector-ref sizes group) 1)))
                (if singleton?
                  (begin
                    (vector-set! next-ids block next-count)
                    (set! next-count (+ next-count 1)))
                  (let* ((key (cons group
                                    (predecessor-signature block ids)))
                         (found (table-ref keys key #f)))
                    (unless found
                      (set! found next-count)
                      (set! next-count (+ next-count 1))
                      (table-set! keys key found))
                    (vector-set! next-ids block found))))
              (block-loop (+ block 1))))
          (values next-ids next-count)))
      (let-values (((initial initial-count)
                    (group-ids
                     (lambda (block)
                       (cons (car (vector-ref blocks block))
                             (= block start))))))
        (let refine ((ids initial) (group-count initial-count))
          (let-values (((next next-count)
                        (refine-group-ids ids group-count)))
            ;; A singleton partition is already stable: no block can split
            ;; further, so skip the otherwise mandatory confirmation round.
            (if (and (> next-count group-count) (< next-count count))
              (refine next next-count)
              (let ((merged (make-vector next-count #f))
                    (merged-shifts (make-vector next-count 0))
                    (merged-epsilons (make-vector next-count 0)))
                (let block-loop ((block 0))
                  (when (< block count)
                    (let* ((group (vector-ref next block))
                           (entry (vector-ref blocks block))
                           (prior (vector-ref merged group))
                           (shift (vector-ref shifts block)))
                      (if prior
                        (vector-set!
                         merged group
                         (cons (car prior)
                               (compiler-index-set-union
                                (cdr prior) (cdr entry))))
                        (vector-set! merged group entry))
                      (when shift
                        (vector-set!
                         merged-shifts group
                         (compiler-index-set-add
                          (vector-ref merged-shifts group)
                          (vector-ref next shift))))
                      (compiler-index-set-for-each
                       (vector-ref epsilons block)
                       (lambda (target)
                         (vector-set!
                          merged-epsilons group
                          (compiler-index-set-add
                           (vector-ref merged-epsilons group)
                           (vector-ref next target)))))
                      (block-loop (+ block 1)))))
                (values merged merged-shifts merged-epsilons
                        (vector-ref next start)))))))))

(def (epsilon-closure initial epsilon-edges)
  (let ((reached initial)
        (queue (stdq-make-Queue)))
    (compiler-index-set-for-each
     initial (lambda (block) (stdq-enqueue! queue block)))
    (let drain ()
      (unless (stdq-queue-empty? queue)
        (let (block (stdq-dequeue! queue))
          (compiler-index-set-for-each
           (vector-ref epsilon-edges block)
           (lambda (target)
             (let (bit (compiler-index-set-singleton target))
               (when (zero? (bitwise-and reached bit))
                 (set! reached (compiler-index-set-union reached bit))
                 (stdq-enqueue! queue target)))))
          (drain))))
    reached))

(def (determinize-follow-blocks blocks index metadata terminal-values
                                layout core-symbols)
  (let* ((block-count (vector-length blocks))
         (shift-edges (make-vector block-count #f))
         (epsilon-edges (make-vector block-count 0))
         (start #f))
    (let block-loop ((block 0))
      (when (< block block-count)
        (let* ((entry (vector-ref blocks block))
               (core (car entry))
               (follow-mask (cdr entry))
               (edge #f))
          (compiler-index-set-for-each
           follow-mask
           (lambda (lookahead)
             (let (candidate
                   (follow-successors core lookahead metadata index))
               (if edge
                 (unless (equal? edge candidate)
                   (error "backward follow block was not stable" block))
                 (set! edge candidate)))))
          (vector-set! shift-edges block (car edge))
          (vector-set! epsilon-edges block (cdr edge)))
        (block-loop (+ block 1))))
    (let ((terminal-index (make-table test: equal?)))
      (let loop ((i 0))
        (when (< i (vector-length terminal-values))
          (table-set! terminal-index (vector-ref terminal-values i) i)
          (loop (+ i 1))))
      (set! start
            (follow-block-ref
             index 0 (table-ref terminal-index +lr-eof+))))
    (let-values (((merged merged-shifts merged-epsilons merged-start)
                  (forward-refine-blocks
                   blocks shift-edges epsilon-edges start core-symbols)))
      (set! blocks merged)
      (set! shift-edges merged-shifts)
      (set! epsilon-edges merged-epsilons)
      (set! start merged-start)
      (set! block-count (vector-length merged)))
    (let ((states (list->ExtensibleVector '()))
          (transitions (list->ExtensibleVector '()))
          (state-index (make-table test: equal?))
          (queue (stdq-make-Queue)))
      (def (intern! closure)
        (let (existing (table-ref state-index closure #f))
          (if existing
            existing
            (let (id (ExtensibleVector-push! states closure))
              (ExtensibleVector-push! transitions '())
              (table-set! state-index closure id)
              (stdq-enqueue! queue id)
              id))))
      (intern! (epsilon-closure
                (compiler-index-set-singleton start) epsilon-edges))
      (let drain ()
        (unless (stdq-queue-empty? queue)
          (let* ((state (stdq-dequeue! queue))
                 (closure (ExtensibleVector-ref states state))
                 (symbol-targets (make-table test: equal?))
                 (symbols '()))
            (compiler-index-set-for-each
             closure
             (lambda (block)
               (let* ((core (car (vector-ref blocks block)))
                      (symbol (vector-ref core-symbols core))
                      (target (vector-ref shift-edges block)))
                 (unless (zero? target)
                   (unless (table-ref symbol-targets symbol #f)
                     (set! symbols (cons symbol symbols)))
                   (table-set!
                    symbol-targets symbol
                    (compiler-index-set-union
                     (table-ref symbol-targets symbol 0) target))))))
            (for-each
             (lambda (symbol)
               (let* ((next-closure
                       (epsilon-closure
                        (table-ref symbol-targets symbol) epsilon-edges))
                      (target (intern! next-closure)))
                 (ExtensibleVector-set!
                  transitions state
                  (cons (cons symbol target)
                        (ExtensibleVector-ref transitions state)))))
             (reverse symbols))
            (ExtensibleVector-set!
             transitions state
             (reverse (ExtensibleVector-ref transitions state)))
            (drain))))
      (let* ((count (ExtensibleVector-fill-pointer states))
             (output-states (make-vector count '()))
             (offsets (make-vector (+ count 1) 0))
             (core-masks (make-vector (vector-length metadata) 0))
             (item-count 0))
        (let state-loop ((state 0))
          (when (< state count)
            (let ((cores '()))
              (compiler-index-set-for-each
               (ExtensibleVector-ref states state)
               (lambda (block)
                 (let* ((entry (vector-ref blocks block))
                        (core (car entry))
                        (known (vector-ref core-masks core)))
                   (when (zero? known)
                     (set! cores (cons core cores)))
                   (vector-set! core-masks core
                                (compiler-index-set-union
                                 known (cdr entry))))))
              (let (ordered (list-sort < cores))
                (vector-set! output-states state
                             (cons ordered
                                   (map (lambda (core)
                                          (let (mask (vector-ref core-masks core))
                                            (vector-set! core-masks core 0)
                                            mask))
                                        ordered)))
                (vector-set! offsets state item-count)
                (set! item-count (+ item-count (length ordered)))))
            (state-loop (+ state 1))))
        (vector-set! offsets count item-count)
        (let ((lookaheads (make-vector item-count 0))
              (state-cores (make-vector count '()))
              (node 0))
          (let fill ((state 0))
            (when (< state count)
              (let ((entry (vector-ref output-states state)))
                (vector-set! state-cores state (car entry))
                (for-each
                 (lambda (mask)
                   (vector-set! lookaheads node mask)
                   (set! node (+ node 1)))
                 (cdr entry)))
              (fill (+ state 1))))
          (values state-cores count lookaheads offsets
                  (materialize-transitions
                   (ExtensibleVector->vector transitions) count)
                  terminal-values layout core-symbols
                  block-count item-count))))))

(def (canonical-at-conflict-lower-bound? trial)
  (let* ((states (list-ref trial 0))
         (count (list-ref trial 1))
         (lookaheads (list-ref trial 2))
         (offsets (list-ref trial 3))
         (terminal-values (list-ref trial 5))
         (layout (list-ref trial 6))
         (core-symbols (list-ref trial 7))
         (groups (make-table test: equal?))
         (group-cores (list->ExtensibleVector '()))
         (group-masks (list->ExtensibleVector '())))
    (let state-loop ((state 0))
      (when (< state count)
        (let* ((cores (vector-ref states state))
               (known (table-ref groups cores #f))
               (group
                (if known known
                    (let (id (ExtensibleVector-push! group-cores cores))
                      (ExtensibleVector-push!
                       group-masks (make-vector (length cores) 0))
                      (table-set! groups cores id)
                      id)))
               (row (ExtensibleVector-ref group-masks group)))
          (let item-loop ((index 0) (node (vector-ref offsets state))
                          (remaining cores))
            (unless (null? remaining)
              (vector-set!
               row index
               (compiler-index-set-union
                (vector-ref row index) (vector-ref lookaheads node)))
              (item-loop (+ index 1) (+ node 1) (cdr remaining)))))
        (state-loop (+ state 1))))
    (let* ((group-count (ExtensibleVector-fill-pointer group-cores))
           (merged-states (ExtensibleVector->vector group-cores))
           (merged-offsets (make-vector (+ group-count 1) 0))
           (item-count 0))
      (let group-loop ((group 0))
        (when (< group group-count)
          (vector-set! merged-offsets group item-count)
          (set! item-count
                (+ item-count
                   (vector-length (ExtensibleVector-ref group-masks group))))
          (group-loop (+ group 1))))
      (vector-set! merged-offsets group-count item-count)
      (let ((merged-lookaheads (make-vector item-count 0))
            (node 0))
        (let group-loop ((group 0))
          (when (< group group-count)
            (let (row (ExtensibleVector-ref group-masks group))
              (let item-loop ((index 0))
                (when (< index (vector-length row))
                  (vector-set! merged-lookaheads node
                               (vector-ref row index))
                  (set! node (+ node 1))
                  (item-loop (+ index 1)))))
            (group-loop (+ group 1))))
        (let (candidates
              (raw-conflict-cells
               merged-states group-count merged-lookaheads merged-offsets
               terminal-values layout core-symbols))
          (= count
             (+ group-count
                (let loop ((group 0) (total 0))
                  (if (= group group-count)
                    total
                    (loop (+ group 1)
                          (if (zero? (vector-ref candidates group))
                            total (+ total 1))))))))))))

(def (build-states-via-follow-partition-lr1/from-lr0
      productions table first nullable (canonical-trial? #t))
  (let-values (((states count lookaheads offsets transitions
                        terminal-values layout core-symbols
                        state-visits item-visits)
                (build-states-via-lr0 productions table first nullable)))
    (let (candidates
          (raw-conflict-cells
           states count lookaheads offsets terminal-values layout core-symbols))
      (def (build-direct)
        (let-values (((catalogue terminal-index)
                      (production-terminal-catalog productions)))
          (unless (equal? terminal-values catalogue)
            (error "LR terminal catalogues disagree"))
          (let (metadata
                (make-follow-core-metadata
                 productions table first nullable layout core-symbols
                 terminal-index))
            (let-values (((initial seed-candidates seed-terminals)
                          (initial-backward-follow-partitions/from-lr0
                           productions table first nullable
                           states count terminal-values candidates)))
              (let-values (((refined blocks index rounds)
                            (refine-follow-blocks
                             initial metadata (vector-length terminal-values))))
                (determinize-follow-blocks
                 blocks index metadata terminal-values layout
                 core-symbols))))))
      (let (conflict-count
            (let loop ((state 0) (total 0))
              (if (= state count)
                total
                (loop (+ state 1)
                      (if (zero? (vector-ref candidates state))
                        total (+ total 1))))))
       (if (zero? conflict-count)
        ;; A conflict-free LALR table is already adequate. Reuse the same
        ;; LR(0) graph and propagated lookaheads instead of determinizing an
        ;; equivalent follow NFA.
        (values states count lookaheads offsets transitions
                terminal-values layout core-symbols 0
                (vector-length lookaheads))
        (if (and canonical-trial? (>= conflict-count 64))
          ;; Every conflicting LR(0) state needs at least one split in an
          ;; LR(1) construction. Stop the canonical trial at that lower bound;
          ;; an exact match leaves no state compression for follow refinement.
          (let* ((lower-bound (+ count conflict-count))
                 (trial
                  (call-with-values
                   (lambda ()
                     (build-states-via-canonical-lr1
                      productions table first nullable lower-bound))
                   list)))
            (if (and (= (length trial) 10)
                     (= (cadr trial) lower-bound))
              (apply values
                     (append (take trial 8)
                             (list 0 (list-ref trial 9))))
              (build-direct)))
          (build-direct)))))))

(def (build-states-via-follow-partition-lr1 productions table first nullable)
  ;; At medium size, independent conflict contexts can already make the LR(0)
  ;; seed and follow refinement more expensive than a bounded canonical trial.
  ;; The state budget still sends grammars needing further compression to the
  ;; direct follow construction.
  (if (>= (vector-length table) 256)
    (let (trial
          (call-with-values
           (lambda ()
             (build-states-via-canonical-lr1
              productions table first nullable
              (* 3 (vector-length table))))
           list))
      (if (and (= (length trial) 10)
               (canonical-at-conflict-lower-bound? trial))
        (apply values
               (append (take trial 8)
                       (list 0 (list-ref trial 9))))
        (build-states-via-follow-partition-lr1/from-lr0
         productions table first nullable #f)))
    (build-states-via-follow-partition-lr1/from-lr0
     productions table first nullable)))
