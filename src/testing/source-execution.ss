;;; Selected source-witness capability; never imported by the test facade.
;;; The installed expander owns hygiene and the compiler owns core traversal.
(import :gerbil/expander :gerbil/compiler/compile
        :gerbil/compiler/method :gerbil/compiler/base)
(export prepare-source-procedure source-expression-dependencies)

(defcompile-method (collect-source-dependencies record: record)
  (::source-dependency-collector ::collect-expression-refs) (record)
  final:
  (%#ref source-dependency-reference%)
  (%#set! source-dependency-assignment%))

;;; A Gambit primitive need not have an expander binding. The primitive slot
;;; is distinct from an ambient global value; a ## spelling alone proves nothing.
(def (source-reference-resolved? id)
  (or (resolve-identifier id)
      (let (name (stx-e id))
        (and (symbol? name) (string-prefix? "##" (symbol->string name))
             (procedure? (##global-var-primitive-ref (##make-global-var name)))))))

(def (source-dependency-reference% self stx)
  (ast-case stx ()
    ((_ id)
     (unless (source-reference-resolved? #'id)
       ((@ self record) (stx-e #'id))))))

(def (source-dependency-assignment% self stx)
  (ast-case stx ()
    ((_ id expression)
     (unless (source-reference-resolved? #'id)
       ((@ self record) (stx-e #'id)))
     ;; An assignment depends on both its target and its complete RHS.
     (compile-e self #'expression))))

(def (expanded-source-dependencies core)
  (let (references '())
    (collect-source-dependencies core
      record: (lambda (name) (set! references (cons name references))))
    (reverse references)))

(def (source-expression-dependencies expression)
  (expanded-source-dependencies (core-expand (datum->syntax #f expression))))

(def (prepare-source-procedure expression)
  (let* ((core (core-expand (datum->syntax #f expression)))
         (missing (expanded-source-dependencies core)))
    (unless (and (stx-pair? core)
                 (memq (stx-e (stx-car core)) '(%#lambda %#case-lambda)))
      (error "source witness must expand to a procedure literal"))
    (unless (null? missing)
      (error "source procedure has undeclared dependencies" missing))
    ;; Keep the exact checked syntax: do not expand a macro a second time.
    (let (procedure (eval-syntax* core))
      (unless (procedure? procedure) (error "source witness is not a procedure"))
      procedure)))
