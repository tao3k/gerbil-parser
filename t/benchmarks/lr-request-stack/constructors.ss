;;; Complete source requests against the construction algorithm at 4f50dd3.
;;; Compile with -O, matching the measured production constructor modules.
(import (only-in :gerbil-parser/src/runtime/lr-parser/state
                 lr-runtime-action-selector lr-runtime-reduction-plans-set!))
(import (only-in :std/vector/vector vector-map/index)
        (only-in :gerbil-parser/src/compiler/lr production-table production-rhs operand-actions lr-spec-ref)
        (only-in :gerbil-parser/src/runtime/machine parser-machine-runtime parser-machine-ir
                 parser-machine-with-lr-action-selector)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled?)
        (only-in :gerbil-parser/src/runtime/parser parse-source)
        (only-in :gerbil-parser/src/runtime/parse-cost current-parser-cost-observer)
        (only-in :gerbil-parser/src/runtime/recognition make-recognition-child make-recognition-node
                 recognition-child-field recognition-child-value)
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence-arity
                 recognition-sequence->list recognition-sequence-for-action
                 recognition-sequence-start recognition-sequence-end)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid-for-source?)
        (only-in :gerbil-parser/languages/gql/parser gql-parser)
        (only-in :gerbil-parser/languages/fhirpath/parser fhirpath-parser)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-parser)
        (only-in :gerbil-parser/t/benchmarks/parser-stage-cost/benchmark measure-parser-cpu-pairs))
(export benchmark-lr-constructor-case)

;;; Frozen reference: normalize first, then independently read both bounds.
;;; The production constructors, stack driver and publication stay authoritative.
(def (reference-field name children offset constructor)
  (case (recognition-sequence-arity children)
    ((0) '())
    ((1)
     (let (child (car (recognition-sequence->list children)))
       (if (recognition-child-field child)
         (list (make-recognition-child name
                 (constructor (recognition-sequence-start children offset)
                              (recognition-sequence-end children offset) children)))
         (list (make-recognition-child name (recognition-child-value child))))))
    (else
     (list (make-recognition-child name
             (constructor (recognition-sequence-start children offset)
                          (recognition-sequence-end children offset) children))))))

(def (reference-alias kind children offset)
  (list (make-recognition-child #f
          (make-recognition-node kind (recognition-sequence-start children offset)
                                 (recognition-sequence-end children offset) children))))

(def (reference-action action)
  (let (name (cadr action))
    (case (car action)
      ((field) (lambda (value offset constructor)
                 (reference-field name (recognition-sequence-for-action value) offset constructor)))
      ((alias) (lambda (value offset _constructor)
                 (reference-alias name (recognition-sequence-for-action value) offset))))))

(def (reference-chain actions)
  (and (pair? actions)
       (let ((first (reference-action (car actions))) (remaining (reference-chain (cdr actions))))
         (if remaining
           (lambda (value offset constructor)
             (remaining (first value offset constructor) offset constructor))
           first))))

(def (reference-plans table)
  (vector-map/index
   (lambda (_id production)
     (list->vector (map (lambda (operand) (reference-chain (operand-actions operand)))
                       (production-rhs production)))) table))

;;; These two private record bindings are resolved once, outside timing. Only
;;; newly cloned qualification runtimes are modified; grammar tables and the
;;; public language machines remain shared and untouched. Record migrations
;;; belong in this qualification owner.
(def runtime-selector lr-runtime-action-selector)
(def install-reference-plans! lr-runtime-reduction-plans-set!)

(def (source-executor-witness machine source expected)
  (let (stages '())
    (parameterize ((current-parser-cost-observer
                     (lambda (stage row)
                       (set! stages (cons (cons stage (cdr (assq 'completed row))) stages)))))
      (unless (equal? (parse-source machine source) expected)
        (error "constructor executor witness changes complete artifact")))
    (reverse stages)))

(def (compare-constructor-request family units machine source calls)
  (let* ((clone (lambda ()
                 (parser-machine-with-lr-action-selector machine
                   (lambda (_rows _index _casefold _layout)
                     (runtime-selector (parser-machine-runtime machine))))))
         (original (clone)) (candidate (clone))
         (spec (cdr (assq 'lr-spec (parser-machine-ir machine))))
         (expected (parse-source machine source)))
    (install-reference-plans! (parser-machine-runtime original)
      (reference-plans (production-table (lr-spec-ref spec 'productions))))
    (let ((public-stages (source-executor-witness machine source expected))
          (original-stages (source-executor-witness original source expected))
          (candidate-stages (source-executor-witness candidate source expected)))
      (unless (and (parse-artifact-valid-for-source? expected source)
                   (equal? original-stages candidate-stages)
                   (or (eq? family 'arithmetic-addition) (equal? public-stages candidate-stages)))
        (error "constructor comparison changes source executor" family units))
      (write (list 'LR-CONSTRUCTOR-EXECUTORS family units
                   (if (eq? family 'arithmetic-addition) 'representation-control 'public-source-request)
                   public-stages original-stages candidate-stages))
      (newline) (force-output))
    (write (list 'LR-CONSTRUCTOR-CPU family units
             (measure-parser-cpu-pairs 'lr-construction (list family units) 20 calls expected
               (lambda () (parse-source original source)) (lambda () (parse-source candidate source)))))
    (newline) (force-output)))

;;; Separate cases fit the unchanged ninety-second native process budget.
;;; Larger small-request batches retain natural GC while resolving its variance.
(def (benchmark-lr-constructor-case family units)
  (unless (memq units '(1 64 512)) (error "unsupported constructor input scale" units))
  (when (memq family '(gql-match-filter fhirpath-property-invocation))
    (unless (= units 1) (error "structured request has one fixed source scale" family units)))
  (let ((addition (string-join (make-list units "1") " + "))
        (calls (case units ((1) 16384) ((64) 256) (else 64))))
    (parameterize ((current-lr-event-program-enabled? #f))
      (case family
        ((gql-return)
         (compare-constructor-request family units gql-parser
           (string-append "RETURN "
             (string-join (map (lambda (i) (string-append "v" (number->string i))) (iota units)) ", ")
             "\n") calls))
        ((fhirpath-addition)
         (compare-constructor-request family units fhirpath-parser addition
           (if (= units 1) 32768 calls)))
        ((arithmetic-addition)
         (compare-constructor-request family units arithmetic-parser addition calls))
        ((gql-match-filter)
         (compare-constructor-request family units gql-parser
           "MATCH (n) WHERE n.score > 2 AND n.active = TRUE RETURN n" 512))
        ((fhirpath-property-invocation)
         (compare-constructor-request family units fhirpath-parser
           "Patient.name.where(use = 'official').given" 2048))
        (else (error "unsupported constructor request family" family)))))
  (displayln "LR-CONSTRUCTOR-CASE-OK " family " " units) (force-output))
