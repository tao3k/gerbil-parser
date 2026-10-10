;;; -*- Gerbil -*-
;;; Nominal parser machine, prepared execution and checked backend binding.

(import (only-in ../runtime/contextual-ir contextual-ir-ref contextual-ir-copy)
        (only-in ../runtime/lexical-source
                 prepare-lexical-source-plan call-with-lexical-source)
        (only-in ../runtime/lr-parser
                 lr-prepare lr-parse/prepared lr-runtime-for-current-semantic-backend
                 lr-runtime-lexical-mode-catalog lr-runtime-direct-step lr-runtime-event-step
                 lr-runtime-event-program? install-lr-runtime-direct-step!
                 install-lr-runtime-event-step!)
        (only-in ../runtime/token
                 token-kind))
(import (only-in ./lexical-expression lexical-end lexical-choice
                 lexical-dispatch lexical-dispatch/ranked)
        (only-in ./lexical-lexer generated-lexer current-lexical-plan-sharing-enabled?))
(export call-with-parser-machine-source
        parser-machine-for-current-semantic-backend
        current-lexical-plan-sharing-enabled? parser-machine-prepare-lexer
        parser-machine-lexical-plans parser-machine-lexical-modes-compatible?
        defgeneral-parser-machine
        lexical-end
        lexical-choice
        lexical-dispatch
        lexical-dispatch/ranked
        parser-machine?
        parser-machine-ir
        parser-machine-contextual-ir
        parser-machine-grammar-digest
        parser-machine-lex
        parser-machine-trivia
        parser-machine-runtime
        parser-machine-parse
        parser-machine-direct-drive
        parser-machine-direct-source
        parser-machine-backend-representation
        install-parser-machine-backends!
        install-parser-machine-direct-drive!
        install-parser-machine-direct-source!
        install-parser-machine-direct-step! install-parser-machine-event-step!)

;; parser-machine
;;   : ParserMachine
;;   | doc m%
;;       Holds the immutable AOT IR and its lexer/parser entry procedures.
;;
;;       # Examples
;;
;;       ```scheme
;;       (parser-machine? generated-machine)
;;       ;; => #t for a generated parser machine
;;       ```
;;     %
(defstruct parser-machine
  (ir grammar-digest lex trivia runtime parse direct-drive direct-source lexical-plans lexical-certificates lexer-factory source-plan owned-program)
  transparent: #t)

;;; The machine caches the exact instruction object in its canonical owner.
;;; Both fields reference the same object; parser hot paths retain a native accessor.
(def (parser-machine-contextual-ir machine)
  (contextual-ir-copy (parser-machine-owned-program machine)))

;;; A captured session owns the selected runtime together with its parser machine.
;;; This preserves the exact runtime identity required by certified fragment reuse.
(def (parser-machine-for-current-semantic-backend machine)
  (let (runtime (lr-runtime-for-current-semantic-backend (parser-machine-runtime machine)))
    (if (eq? runtime (parser-machine-runtime machine)) machine
      (make-parser-machine
       (parser-machine-ir machine) (parser-machine-grammar-digest machine)
       (parser-machine-lex machine) (parser-machine-trivia machine) runtime
       (lambda (tokens . maybe-observability)
         (lr-parse/prepared runtime tokens
           (if (null? maybe-observability) #f (car maybe-observability))))
       (parser-machine-direct-drive machine) (parser-machine-direct-source machine)
       (parser-machine-lexical-plans machine) (parser-machine-lexical-certificates machine)
       (parser-machine-lexer-factory machine) (parser-machine-source-plan machine)
       (parser-machine-owned-program machine)))))

(def (call-with-parser-machine-source machine source thunk)
 (call-with-lexical-source (parser-machine-source-plan machine) source thunk))

(def (parser-machine-prepare-lexer machine)
  ((parser-machine-lexer-factory machine)))
(def (parser-machine-lexical-modes-compatible? machine old-mode new-mode (first-character #f))
  (or (= old-mode new-mode)
      (let (plans (parser-machine-lexical-plans machine))
        (eq? (vector-ref plans old-mode) (vector-ref plans new-mode)))
      (and first-character (< (char->integer first-character) 128)
           (let* ((certificates (force (parser-machine-lexical-certificates machine)))
                  (class (vector-ref (vector-ref certificates 0) (char->integer first-character)))
                  (modes (vector-ref certificates 1)))
             (eq? (vector-ref (vector-ref modes old-mode) class)
                  (vector-ref (vector-ref modes new-mode) class))))))

;;; A generated driver produces recognition values, independently of the
;;; optional prepared event executor. It is admitted only for the Parser IR whose digest
;;; was embedded in its source. Install during language-module initialization,
;;; before the machine is shared with parser requests.
(def (install-parser-machine-direct-drive! machine digest drive)
  (unless (and (parser-machine? machine)
               (string? digest)
               (equal? digest (parser-machine-grammar-digest machine))
               (procedure? drive)
               (not (parser-machine-direct-drive machine)))
    (error "generated LR driver does not match parser machine" digest))
  (parser-machine-direct-drive-set! machine drive))

;;; Grammar-derived source parsers produce a ParseArtifact or decline with #f.
;;; They may admit a complete artifact directly for
;;; fresh unobserved requests. Returning #f leaves the ordinary LR path to
;;; own rejection, diagnostics, checkpoints, and unsupported source shapes.
(def (install-parser-machine-direct-source! machine digest parse)
  (unless (and (parser-machine? machine)
               (string? digest)
               (equal? digest (parser-machine-grammar-digest machine))
               (procedure? parse)
               (not (parser-machine-direct-source machine)))
    (error "generated source parser does not match parser machine" digest))
  (parser-machine-direct-source-set! machine parse))

(def (install-parser-machine-direct-step! machine digest step)
  (unless (and (parser-machine? machine)
               (string? digest)
               (equal? digest (parser-machine-grammar-digest machine))
               (procedure? step)
               (not (lr-runtime-event-program? (parser-machine-runtime machine))))
    (error "generated LR step does not match parser machine" digest))
  (install-lr-runtime-direct-step!
   (parser-machine-runtime machine) step))

(def (install-parser-machine-event-step! machine digest step)
  (unless (and (parser-machine? machine) (string? digest)
               (equal? digest (parser-machine-grammar-digest machine))
               (procedure? step))
    (error "generated LR event step does not match parser machine" digest))
  (install-lr-runtime-event-step! (parser-machine-runtime machine) step))

;;; Installed kinds bind representation obligations without an author annotation.
(def (backend-semantic-representation kind)
  (case kind
    ((drive step) 'recognition)
    ((event-step) 'event-program)
    ((source) 'parse-artifact)
    (else #f)))

;;; Describe an installed entry, not a requested preference. Generated drive
;;; and source entries retain their own product contracts on a selected machine.
;;; This reads captured runtime identity; it does not select or prepare a backend.
(def (parser-machine-backend-representation machine kind)
  (unless (and (parser-machine? machine)
               (or (eq? kind 'prepared) (backend-semantic-representation kind)))
    (error "unknown parser backend representation" kind))
  (let (runtime (parser-machine-runtime machine))
    (case kind
      ((prepared) (if (lr-runtime-event-program? runtime) 'event-program 'recognition))
      ((drive) (and (parser-machine-direct-drive machine) 'recognition))
      ((source) (and (parser-machine-direct-source machine) 'parse-artifact))
      ((step) (and (lr-runtime-direct-step runtime)
                   (if (lr-runtime-event-program? runtime) 'event-program 'recognition)))
      ((event-step) (and (lr-runtime-event-step runtime) 'event-program)))))

;;; Check the entire declaration before mutating any machine/runtime slot.
;;; Each row is (kind exact-parser-ir-digest generated-procedure).
(def (install-parser-machine-backends! machine rows)
  (unless (and (parser-machine? machine) (list? rows))
    (error "invalid parser backend declaration" rows))
  (let ((seen '()) (runtime (parser-machine-runtime machine)))
    (for-each
     (lambda (row)
       (unless (and (list? row) (= (length row) 3)
                    (backend-semantic-representation (car row))
                    (not (memq (car row) seen))
                    (string? (cadr row))
                    (equal? (cadr row) (parser-machine-grammar-digest machine))
                    (procedure? (caddr row)))
         (error "invalid or stale parser backend declaration" row))
       (when (and (eq? (car row) 'step) (lr-runtime-event-program? runtime))
         (error "recognition step requires a recognition runtime" row))
       (when (case (car row)
               ((drive) (parser-machine-direct-drive machine))
               ((source) (parser-machine-direct-source machine))
               ((step) (lr-runtime-direct-step runtime))
               ((event-step) (or (lr-runtime-event-step runtime)
                                (lr-runtime-event-program? runtime))))
         (error "parser backend slot is already installed" (car row)))
       (set! seen (cons (car row) seen))) rows)
    (for-each
     (lambda (row)
       ((case (car row)
          ((drive) install-parser-machine-direct-drive!)
          ((source) install-parser-machine-direct-source!)
          ((step) install-parser-machine-direct-step!)
          ((event-step) install-parser-machine-event-step!))
        machine (cadr row) (caddr row))) rows)))

;;; Connects immutable parser IR to generated lexer and LR runtime entrypoints once.
;;; Runtime calls receive the compiled machine and never re-enter grammar expansion.
;; defgeneral-parser-machine
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `defgeneral-parser-machine` expands a complete parser machine binding.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defgeneral-parser-machine parser contextual-program ...)
;;       ;; => immutable parser-machine binding
;;       ```
;;     %
(defrules defgeneral-parser-machine
  (lexical-rules rules extras parser-entrypoints)
  ((_ binding program-expression
      (lexical-rules lexical-row ...)
      (rules (rule-name rule-expression) ...)
      (extras extra-name ...)
      (parser-entrypoints
       (root-rule root-action root-effect) entry-row ...))
   (def binding
     (let* ((program program-expression)
            (grammar-digest (contextual-ir-ref program 'base-grammar-digest))
            (owned-ir (contextual-ir-ref (contextual-ir-ref program 'recognition) 'program))
            (runtime (lr-prepare (cdr (assq 'lr-spec owned-ir))))
            (factory (lambda ()
                       (generated-lexer
                        (lexical-rules lexical-row ...)
                        (extras extra-name ...)
                        (cdr (assq 'case-insensitive? owned-ir))
                        (lr-runtime-lexical-mode-catalog runtime)))))
       (let-values (((lexer plans certificates) (factory)))
         (make-parser-machine
          owned-ir grammar-digest lexer
          (lambda (input-token) (memq (token-kind input-token) '(extra-name ...)))
          runtime
          (lambda (tokens . maybe-observability)
            (lr-parse/prepared runtime tokens
              (if (null? maybe-observability) #f (car maybe-observability))))
          #f #f plans certificates factory
          (prepare-lexical-source-plan (cdr (assq 'lexical-rules owned-ir))) program))))))
