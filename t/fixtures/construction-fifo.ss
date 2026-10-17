;;; Passive observations of real initialization and FIFO control. Primitive
;;; queue operations and every original source instruction still execute.
(import (only-in "event-walk-source.ss" source-procedure read-source-definition)
        (only-in "construction-layout.ss" construction-layout)
        (only-in "prediction-witness.ss" prediction-dependencies)
        :gerbil-parser/src/compiler/lr-automaton :gerbil-parser/src/compiler/funcs
        (only-in :gerbil-parser/src/compiler/lr production-index-by-lhs nonterminal-symbol? nonterminal-name production-id)
        (only-in :std/struct/queue make-Queue enqueue! dequeue! queue-empty?))
(export fifo-source-observation existing-zero-observation)

(def (existing-zero-observation)
  (def (targets form)
    (cond ((not (pair? form)) '())
          ((eq? (car form) 'quote) '())
          ((and (eq? (car form) 'for-each) (= (length form) 3)
                (pair? (caddr form)) (eq? (caaddr form) 'lr0-state-kernels)) (list form))
          (else (apply append (map targets form)))))
  (let (forms (targets (read-source-definition "src/compiler/lr-automaton.ss" 'build-lr0-automaton)))
    (unless (= (length forms) 1) (error "expected one source target callback"))
    ((eval `(lambda ()
       (let ((state-index (make-table test: equal?)) (rows '()) (plan '(0 . #f))
             (symbols '#(token)) (symbol-ids '#()) (masks '#()) (first-items '#()) (queue #f))
         (table-set! state-index 17 0)
         (def (append-state! _) (error "existing zero must not append"))
         (def (enqueue! _queue _id) (error "existing zero must not enqueue"))
         (def (lr0-state-kernels _kernel _template _ids _masks _firsts) (list (vector 0 0 17)))
         ,(car forms)
         rows))))))

(def (observe-source source)
  (def (walk form)
    (cond ((not (pair? form)) form)
          ((eq? (car form) 'quote) form)
          (else
           (let* ((original (map walk form))
                  (initial (cond
                    ((and (eq? (car form) 'vector-set!) (equal? (caddr form) 0)
                          (memq (cadr form) '(states kernel-plans)))
                     (if (eq? (cadr form) 'states) 'init-state 'init-plan))
                    ((equal? form '(table-set! state-index initial-kernel 0)) 'init-register)
                    (else #f))))
             (cond
              (initial `(begin ,original (observe-initial (quote ,initial) state-count processed-count
                           states state-transitions kernel-plans state-index initial-kernel)))
              ((and (eq? (car form) 'set!) (eq? (cadr form) 'rows))
               `(begin ,original (observe-row symbol mask existing target-index state-count processed-count)))
              ((equal? form '(vector-set! state-transitions index (reverse rows)))
               `(begin ,original (observe-publication index rows (vector-ref state-transitions index))))
              (else original))))))
  (walk source))

(def (valid-history? events count)
  (and (>= (length events) 4)
       (equal? (take events 4) '((init-state #t) (init-plan #t) (init-register #t) (enqueue 0)))
       (let loop ((events (drop events 4)) (pending '(0)) (allocated 1) (processed 0) (current #f) (rows '()))
         (if (null? events)
           (and (not current) (null? pending) (= allocated count processed))
           (let ((event (car events)) (rest (cdr events)))
             (case (car event)
               ((dequeue)
                (and (not current) (pair? pending) (= (cadr event) (car pending) processed)
                  (loop rest (cdr pending) allocated (+ processed 1) (cadr event) '())))
               ((row)
                (let ((symbol (list-ref event 1)) (existing (list-ref event 2))
                      (target (list-ref event 3)) (size (list-ref event 4)) (done (list-ref event 5)))
                  (and current (= done processed)
                    (if existing
                      (and (= existing target) (= size allocated)
                        (loop rest pending allocated processed current (cons (cons symbol target) rows)))
                      (and (= target allocated) (= size (+ allocated 1)) (pair? rest)
                        (equal? (car rest) (list 'enqueue target))
                        (loop (cdr rest) (append pending (list target)) (+ allocated 1) processed current
                          (cons (cons symbol target) rows)))))))
               ((publish)
                (and current (= (cadr event) current) (equal? (caddr event) rows)
                  (equal? (cadddr event) (reverse rows))
                  (loop rest pending allocated processed #f '())))
               (else #f)))))))

(def (fifo-source-observation productions initial)
  (let* ((table (list->vector productions)) (layout (make-item-layout table '#()))
         (cores (make-core-symbol-catalog table layout)) (events '())
         (path "src/compiler/lr-automaton.ss")
         (prediction (source-procedure path 'make-core-prediction-catalog prediction-dependencies))
         (symbols (source-procedure path 'make-lr0-symbol-catalog '()))
         (template (source-procedure path 'make-prediction-template
           '(compiler-index-set-for-each make-core-item compiler-index-set-add)))
         (closure (source-procedure path 'lr0-closure '(compiler-index-set-for-each)))
         (groups (source-procedure path 'lr0-state-kernels
           '(compiler-index-set-for-each compiler-index-set-add compiler-index-set-union)))
         (builder (source-procedure "src/compiler/lr-automaton.ss" 'build-lr0-automaton
           '(production-index-by-lhs make-core-prediction-catalog compiler-index-set-add make-core-item
             make-lr0-symbol-catalog make-prediction-template compiler-index-set-for-each compiler-index-set-union
             lr0-closure make-Queue enqueue! dequeue! queue-empty? lr0-state-kernels
             observe-initial observe-row observe-publication)
           (lambda (source) (observe-source ((construction-layout initial) source))))))
    (def (record event) (set! events (cons event events)))
    (let-values (((_states _rows count _index processed)
      (builder productions table layout cores production-index-by-lhs
        (lambda (core-symbols by-lhs layout)
          (prediction core-symbols by-lhs layout nonterminal-symbol? nonterminal-name production-id make-core-item
            compiler-index-set-add compiler-index-set-difference compiler-index-set-union compiler-index-set-for-each
            make-Queue enqueue! dequeue! queue-empty? make-vector))
        compiler-index-set-add make-core-item symbols
        (lambda (mask layout ids) (template mask layout ids compiler-index-set-for-each make-core-item compiler-index-set-add))
        compiler-index-set-for-each compiler-index-set-union
        (lambda (mask value) (closure mask value compiler-index-set-for-each)) make-Queue
        (lambda (queue id) (record (list 'enqueue id)) (enqueue! queue id))
        (lambda (queue) (let (id (dequeue! queue)) (record (list 'dequeue id)) id))
        queue-empty?
        (lambda (mask value ids masks firsts)
          (groups mask value ids masks firsts compiler-index-set-for-each compiler-index-set-add compiler-index-set-union))
        (lambda (tag size done states rows plans index kernel)
          (let ((state (vector-ref states 0)) (plan (vector-ref plans 0)) (id (table-ref index kernel #f)))
            (record (list tag (and (= size 1) (= done 0) state (null? (vector-ref rows 0))
              (if (eq? tag 'init-state) (not plan) (and (pair? plan) (= (car plan) kernel) (vector? (cdr plan))))
              (if (eq? tag 'init-register) (eqv? id 0) (not id)))))))
        (lambda (symbol _mask existing target size done) (record (list 'row symbol existing target size done)))
        (lambda (node rows published) (record (list 'publish node rows published))))))
      (let (ordered (reverse events))
        (list count (= count processed) (valid-history? ordered count)
          (and (not (valid-history? (reverse ordered) count))
               (not (valid-history? (filter (lambda (entry) (not (equal? entry '(enqueue 0)))) ordered) count)))
          (not (valid-history? (map (lambda (entry)
            (if (eq? (car entry) 'publish) (list 'publish (+ 1 (cadr entry)) (caddr entry) (cadddr entry)) entry)) ordered) count)))))))
