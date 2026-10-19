;;; Reader adapter only: preserve source syntax for the Lean admission kernel.
(import (only-in "../../src/modules/loader.ss" load-parser-module)
        (only-in "./prediction-witness.ss"
                 prediction-observation prediction-dependencies prediction-fixtures)
        (only-in "../../src/testing/source-witness.ss"
                 parser-source-data write-parser-source-data source-data-current?))
(export read-event-walk-source read-event-walk-source-port read-event-iterative-source read-source-definition read-source-definition-port source-declaration-matches? transform-source-declaration source-procedure source-expression-procedure source-expression-dependencies read-source-subdefinition source-mutation-sites write-event-walk-source-data event-walk-data-current?)

(def (read-event-walk-source path)
  (call-with-input-file path read-event-walk-source-port))

(def (read-event-walk-source-port port)
  (cadddr (read-event-walker-scope-port port)))

(def (read-event-iterative-source path)
  (caddr (call-with-input-file path read-event-walker-scope-port)))

(def (read-event-walker-scope-port port)
  (let (definitions
        (let loop ((found '()))
          (let (form (read port))
            (cond
             ((eof-object? form) (reverse found))
             ((and (pair? form) (eq? (car form) 'defsyntax)
                   (pair? (cdr form)) (pair? (cadr form))
                   (eq? (caadr form) 'event-program-walk/inline))
              (loop (cons form found)))
             (else (loop found))))))
    (unless (= (length definitions) 1) (error "expected exactly one event walker macro"))
    (let* ((definition (car definitions))
           (form (and (= (length definition) 3) (caddr definition))))
      (unless (and (equal? (cadr definition) '(event-program-walk/inline stx))
                   (list? form) (= (length form) 4)
                   (equal? (list (car form) (cadr form) (caddr form)) '(syntax-case stx (lambda))))
        (error "unsupported event walker macro envelope"))
      (let (clause (cadddr form))
        (unless (and (list? clause) (= (length clause) 2)
                     (equal? (car clause) '(_ (lambda (op name offset shift moved context) effect ...) initial-code)))
          (error "unsupported event walker syntax pattern"))
        (let (template (cadr clause))
          (unless (and (list? template) (= (length template) 2) (eq? (car template) 'syntax))
            (error "unsupported event walker template"))
          (let (body (cadr template))
            (unless (and (list? body) (= (length body) 5)
                         (equal? (list (car body) (cadr body)) '(let ()))
                         (equal? (last body) '(walk initial-code 0 #f 0))
                         (list? (caddr body)) (>= (length (caddr body)) 3)
                         (equal? (list (car (caddr body)) (cadr (caddr body)))
                                 '(def (iterative initial delta moved?))))
              (error "unsupported event walker procedure scope"))
            ;; Keep the whole lexical scope. Public readers project one complete
            ;; declaration, preserving every operand and trailing expression.
            body))))))

;;; Keep the complete declaration, including both passes and final publication.
(def (source-declaration-matches? form name)
  (and (list? form) (>= (length form) 3) (eq? (car form) 'def)
       (or (eq? (cadr form) name)
           (and (pair? (cadr form)) (eq? (caadr form) name)))))

(def (read-source-definition-port port name)
  (let (found
        (let loop ((found '()))
          (let (form (read port))
            (cond ((eof-object? form) (reverse found))
                  ((source-declaration-matches? form name) (loop (cons form found)))
                  (else (loop found))))))
    (unless (= (length found) 1) (error "expected one source declaration" name))
    (car found)))

(def (read-source-definition path name)
  (call-with-input-file path (lambda (port) (read-source-definition-port port name))))

;;; The installed host codec gives each stage independent object storage,
;;; including mutable strings and vectors.
(def (copy-source-data value)
  (u8vector->object (object->u8vector value)))

;;; A transformation executes once and may change the body, never its owner or
;;; lexical interface. Dependency admission checks its complete output below.
(def (transform-source-declaration declaration transform)
  (unless (and (list? declaration) (>= (length declaration) 3)
               (eq? (car declaration) 'def) (pair? (cadr declaration))
               (symbol? (caadr declaration)) (list? (cdadr declaration)))
    (error "source witness requires a procedure declaration"))
  (let* ((header (copy-source-data (cadr declaration)))
         (declaration (copy-source-data declaration)))
    (let* ((output (transform declaration))
           (transformed (copy-source-data output)))
      (unless (and (list? transformed) (>= (length transformed) 3)
                   (eq? (car transformed) 'def)
                   (equal? (cadr transformed) header))
        (error "source transformation changed the declaration interface"))
      transformed)))

(def (source-execution-binding name)
  (load-parser-module "gerbil-parser/src/testing/source-execution")
  (eval name))

(def (source-expression-procedure expression)
  ((source-execution-binding 'gerbil-parser/src/testing/source-execution#prepare-source-procedure) expression))

(def (source-expression-dependencies expression)
  ((source-execution-binding 'gerbil-parser/src/testing/source-execution#source-expression-dependencies) expression))

;;; Keep every body form and reject omissions before the procedure is created.
(def (source-procedure path name dependencies (transform identity))
  (let (declaration (transform-source-declaration (read-source-definition path name) transform))
    (source-expression-procedure
      (cons 'lambda
        (cons (append (cdr (cadr declaration)) dependencies) (cddr declaration))))))

;;; Select a named nested declaration without positional AST paths. Quoted
;;; data is excluded; duplicates and missing definitions fail closed.
(def (read-source-subdefinition path outer name)
  (def (collect form)
    (cond ((not (pair? form)) '())
          ((eq? (car form) 'quote) '())
          (else
            (append (if (and (list? form) (>= (length form) 3)
                             (eq? (car form) 'def) (pair? (cadr form))
                             (eq? (caadr form) name)) (list form) '())
                    (apply append (map collect form))))))
  (let (found (collect (read-source-definition path outer)))
    (unless (= (length found) 1) (error "expected one nested source declaration" outer name))
    (car found)))

;;; Independent reader-level control; Lean proves its own collector completeness.
(def (source-mutation-sites form)
  (cond ((not (pair? form)) '())
        ((eq? (car form) 'quote) '())
        (else (append (if (memq (car form) '(set! vector-set!)) (list form) '())
                      (apply append (map source-mutation-sites form))))))

(def (source-bindings source)
  (parser-source-data
    (ParserEngine.WalkSourceWitness source source)
    (ParserEngine.LoweringPublicationWitness emitterSource
      (read-source-subdefinition "src/compiler/lr.ss" 'lower-rules 'emit-production))
    (ParserEngine.LoweringPublicationWitness constructorSource
      (read-source-definition "src/compiler/lr.ss" 'make-production))
    (ParserEngine.LoweringPublicationWitness initialSource
      (cadr (caddr (read-source-definition "src/compiler/lr.ss" 'lower-rules))))
    (ParserEngine.LoweringPublicationWitness finalizerSource
      (last (caddr (read-source-definition "src/compiler/lr.ss" 'lower-rules))))
    (ParserEngine.BitTraversalWitness source
      (read-source-definition "src/compiler/funcs.ss" 'compiler-index-set-for-each))
    (ParserEngine.BitTraversalWitness widthSource
      (read-source-definition "src/compiler/funcs.ss" 'compiler-index-field-width))
    (ParserEngine.SparseGroupWitness symbolSource
      (read-source-definition "src/compiler/lr-automaton.ss" 'make-lr0-symbol-catalog))
    (ParserEngine.SparseGroupWitness groupSource
      (read-source-definition "src/compiler/lr-automaton.ss" 'lr0-state-kernels))
    (ParserEngine.LRTransitionWitness builderSource
      (read-source-definition "src/compiler/lr-automaton.ss" 'build-lr0-automaton))
    (ParserEngine.LRTemplateWitness templateSource
      (read-source-definition "src/compiler/lr-automaton.ss" 'make-prediction-template))
    (ParserEngine.LRTemplateWitness cacheSource
      (read-source-subdefinition "src/compiler/lr-automaton.ss" 'build-lr0-automaton 'template-for))
    (ParserEngine.PredictionObservationWitness observations
      (map (lambda (productions)
             (prediction-observation
               (source-procedure "src/compiler/lr-automaton.ss" 'make-core-prediction-catalog prediction-dependencies)
               productions)) prediction-fixtures))
    (ParserEngine.LRConstructionWitness growSource
      (read-source-subdefinition "src/compiler/lr-automaton.ss" 'build-lr0-automaton 'grow!))
    (ParserEngine.LRConstructionWitness appendSource
      (read-source-subdefinition "src/compiler/lr-automaton.ss" 'build-lr0-automaton 'append-state!))
    (ParserEngine.LRConstructionWitness predictionSource
      (read-source-definition "src/compiler/lr-automaton.ss" 'make-core-prediction-catalog))
    (ParserEngine.EffectSourceWitness iterativeSource (read-event-iterative-source "src/runtime/event-program.ss"))
    (ParserEngine.EffectSourceWitness rowSource (read-source-definition "src/compiler/lr-compiler.ss" 'index-state-rows))
    (ParserEngine.ItemLayoutWitness layoutSource (read-source-definition "src/compiler/lr-automaton.ss" 'make-item-layout))
    (ParserEngine.ItemLayoutWitness encodeSource (read-source-definition "src/compiler/lr-automaton.ss" 'make-core-item))
    (ParserEngine.ItemLayoutWitness ownerSource (read-source-definition "src/compiler/lr-automaton.ss" 'core-item-production-id))
    (ParserEngine.ItemLayoutWitness dotSource (read-source-definition "src/compiler/lr-automaton.ss" 'core-item-dot))
    (ParserEngine.CompilerSemanticsWitness suffixSource (read-source-definition "src/compiler/lr-lookahead.ss" 'make-core-suffix-catalog))
    (ParserEngine.CompilerSemanticsWitness maskSource (read-source-definition "src/compiler/lr-lookahead.ss" 'terminal-list->mask))
    (ParserEngine.CompilerSemanticsWitness closureSource (read-source-definition "src/compiler/lr-automaton.ss" 'lr0-closure))
    (ParserEngine.LookaheadExecutionWitness searchSource (read-source-definition "src/compiler/lr-lookahead.ss" 'lookahead-node-index))
    (ParserEngine.LookaheadExecutionWitness graphSource (read-source-definition "src/compiler/lr-lookahead.ss" 'make-lookahead-propagation-graph))
    (ParserEngine.LookaheadExecutionWitness scheduleSource (read-source-definition "src/compiler/lr-lookahead.ss" 'propagate-lalr-lookaheads))
    (ParserEngine.ProducerAdmissionWitness baseSource (read-source-definition "src/compiler/lr.ss" 'base-symbol))
    (ParserEngine.ProducerAdmissionWitness markedSource (read-source-definition "src/compiler/lr.ss" 'marked-symbol?))
    (ParserEngine.ProducerAdmissionWitness terminalTagSource (read-source-definition "src/compiler/lr.ss" 'terminal-symbol?))
    (ParserEngine.ProducerAdmissionWitness layoutEndSource (read-source-definition "src/compiler/lr.ss" 'layout-end-action?))
    (ParserEngine.ProducerAdmissionWitness terminalSource (read-source-definition "src/compiler/lr-table.ss" 'terminal?))
    (ParserEngine.ProducerAdmissionWitness outerSource (read-source-definition "src/compiler/lr-table.ss" 'validate-lr-tables))
    (ParserEngine.ProducerAdmissionWitness fallbackSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-compiler.ss" 'build-actions) '(2 1 9)))
    (ParserEngine.ProducerAdmissionWitness layoutSource (read-source-definition "src/compiler/lr-table.ss" 'layout-productions?))
    (ParserEngine.ProducerAdmissionWitness vocabularySource (read-source-definition "src/compiler/lr-table.ss" 'production-vocabulary))
    (ParserEngine.ProducerAdmissionWitness catalogSource (read-source-definition "src/compiler/lr.ss" 'production-terminal-catalog))
    (ParserEngine.ProducerAdmissionWitness coreSymbolsSource (read-source-definition "src/compiler/lr-automaton.ss" 'make-core-symbol-catalog))
    (ParserEngine.ProducerAdmissionWitness itemIndexSource (read-source-definition "src/compiler/lr-lookahead.ss" 'index-lr0-items))
    (ParserEngine.ProducerAdmissionWitness canonicalSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-lookahead.ss" 'build-states-via-canonical-lr1) '(2 1 2 2 2 4 5)))
    (ParserEngine.CompilerExecutionWitness queueSource (read-source-definition "src/compiler/lr-table.ss" 'validate-accept-reachability))
    (ParserEngine.CompilerExecutionWitness visitSource (read-source-definition "src/compiler/lr-table.ss" 'for-each-shift-target))
    (ParserEngine.CompilerExecutionWitness acceptSource (read-source-definition "src/compiler/lr-table.ss" 'accepting-action?))
    (ParserEngine.CompilerExecutionWitness stateItemsSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-compiler.ss" 'build-actions) '(2 4 2 1)))
    (ParserEngine.CompilerExecutionWitness stateGuardSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-compiler.ss" 'build-actions) '(2 4 2 2)))
    (ParserEngine.CompilerExecutionWitness stateBindingsSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-compiler.ss" 'build-actions) '(2 4 2 3 1)))
    (ParserEngine.CompilerExecutionWitness itemWalkSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-compiler.ss" 'build-actions) '(2 4 2 3 4)))
    (ParserEngine.CompilerExecutionWitness installSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-compiler.ss" 'build-actions) '(2 4 2 3 2)))
    (ParserEngine.CompilerExecutionWitness precedenceSource (read-source-definition "src/compiler/lr.ss" 'production-precedence))
    (ParserEngine.NativeActionsWitness dispatcherSource (read-source-definition "src/compiler/lr-compiler.ss" 'resolve-action))
    (ParserEngine.NativeActionsWitness dynamicSource (read-source-definition "src/compiler/lr-compiler.ss" 'dynamic-precedence?))
    (ParserEngine.NativeActionsWitness forkSource (read-source-definition "src/compiler/lr-compiler.ss" 'fork-action))
    (ParserEngine.NativeActionsWitness unresolvedSource (read-source-definition "src/compiler/lr-compiler.ss" 'fork-or-reject))
    (ParserEngine.NativeActionsWitness shiftReduceSource (read-source-definition "src/compiler/lr-compiler.ss" 'resolve-shift-reduce))
    (ParserEngine.NativeActionsWitness reduceReduceSource (read-source-definition "src/compiler/lr-compiler.ss" 'resolve-reduce-reduce))
    (ParserEngine.NativeActionsWitness eventSource
      (foldl (lambda (index form) (list-ref form index))
             (read-source-definition "src/compiler/lr-compiler.ss" 'build-actions) '(2 4 2 3 4 1)))
    (ParserEngine.NativeActionsWitness appendUniqueSource (read-source-definition "src/compiler/lr.ss" 'append-unique))
    (ParserEngine.NativeActionsWitness unionSource (read-source-definition "src/compiler/lr.ss" 'union-values))
    (ParserEngine.NativeActionsWitness rowsSource (read-source-definition "src/compiler/lr-table.ss" 'validate-rows))
    (ParserEngine.NativeActionsWitness boundedSource (read-source-definition "src/compiler/lr-table.ss" 'bounded-index?))
    (ParserEngine.NativeActionsWitness precedenceSource (read-source-definition "src/compiler/lr-table.ss" 'precedence?))
    (ParserEngine.NativeActionsWitness validatorSource (read-source-definition "src/compiler/lr-table.ss" 'make-action-validator))
    (ParserEngine.TablePublicationWitness publicationSource (read-source-definition "src/compiler/lr-compiler.ss" 'compile-lr-spec/selected))
    (ParserEngine.ItemLayoutWitness countSource (read-source-definition "src/compiler/lr-automaton.ss" 'core-item-count))))
(def (write-event-walk-source-data source port)
  (write-parser-source-data (source-bindings source) port))
(def (event-walk-data-current? source path)
  (source-data-current? (source-bindings source) path))
