;;; Closed structured text scanners and proof policies shared by language recipes.
(import (only-in :clan/poo/object .o .ref .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/src/runtime/token token? token-lexeme))
(export StructuredProofPolicy. StructuredProofPolicyContract bind-structured-proof-policy)
(def StructuredProofPolicy.
 (.o code: "GERBIL-PARSER-STRUCTURED-PROOF" proof-kind: 'Proof label-kind: 'LabelExpression
     name-field: 'name body-field: 'body step-field: 'step operator-field: 'operator argument-field: 'argument
     name-kind: 'NameExpression application-kind: 'OperatorApplication close: #\> terminal-command: "QED"
     denied-commands: '("USE" "HIDE" "INSTANCE" "DEFINE" "HAVE" "TAKE" "WITNESS")
     denied-kinds: '(LocalDefinition LocalFunctionDefinition OperatorDefinition FunctionDefinition InstanceDeclaration)))
(def (proof-policy? value)
 (with-catch (lambda (_) #f) (lambda ()
  (and (object? value)
       (andmap (lambda (slot) (symbol? (.ref value slot)))
               '(proof-kind label-kind name-field body-field step-field operator-field argument-field name-kind application-kind))
       (char? (.ref value 'close)) (string? (.ref value 'code)) (string? (.ref value 'terminal-command))
       (list? (.ref value 'denied-commands)) (andmap string? (.ref value 'denied-commands))
       (list? (.ref value 'denied-kinds)) (andmap symbol? (.ref value 'denied-kinds))))))
(define-type (StructuredProofPolicyContract @ PooFlowContract.)
 identity: 'gerbil-parser/structured-proof
 .classify: (lambda (candidate context)
  (let (ok? (proof-policy? candidate))
   (poo-flow-classification-evidence 'gerbil-parser/structured-proof candidate ok?
    (if ok? '() '((expected structured-proof))) context))))
(def (bind-structured-proof-policy profile)
 (validate StructuredProofPolicyContract profile)
 (let ((code (string-copy (.ref profile 'code))) (proof-kind (.ref profile 'proof-kind))
       (label-kind (.ref profile 'label-kind)) (name-field (.ref profile 'name-field))
       (body-field (.ref profile 'body-field)) (step-field (.ref profile 'step-field))
       (operator-field (.ref profile 'operator-field)) (argument-field (.ref profile 'argument-field))
       (name-kind (.ref profile 'name-kind)) (application-kind (.ref profile 'application-kind))
       (close-char (.ref profile 'close)) (terminal-command (string-copy (.ref profile 'terminal-command)))
       (denied-commands (map string-copy (.ref profile 'denied-commands))) (denied-kinds (append (.ref profile 'denied-kinds) '())))
(def (children value)
  (cond ((syntax-node? value) (syntax-node-children value))
        ((syntax-field? value) (syntax-field-children value))
        (else '())))

(def (first-token value)
  (if (token? value) value
    (let loop ((rest (children value)))
      (and (pair? rest) (or (first-token (car rest)) (loop (cdr rest)))))))

(def (fields node name)
  (filter (lambda (child)
            (and (syntax-field? child) (eq? (syntax-field-name child) name)))
          (syntax-node-children node)))

(def (proof-error step reason)
  (list (cons 'schema "gerbil-parser.diagnostic.v1")
        (cons 'code code)
        (cons 'reasonKind 'parse-rejected)
        (cons 'failureKind 'proof-level-rejected)
        (cons 'message reason)
        (cons 'byteOffset (syntax-node-start step))))

(def (field-node value)
  (let loop ((rest (children value)))
    (and (pair? rest)
         (if (syntax-node? (car rest)) (car rest)
           (or (field-node (car rest)) (loop (cdr rest)))))))

(def (proof-step-level step current)
  (let* ((name (token-lexeme (first-token (car (fields step name-field)))))
         (close (let loop ((at 1))
                  (if (char=? (string-ref name at) close-char) at (loop (+ at 1)))))
         (level (substring name 1 close)))
    (cond ((equal? level "*") (or current 1))
          ((equal? level "+") (+ (or current 0) 1))
          (else (string->number level)))))

(def (step-command step)
  (token-lexeme (first-token (car (fields step body-field)))))

(def (step-admits-proof? step)
  (let* ((body (car (fields step body-field))) (node (field-node body)))
    (and (not (member (step-command step)
                     denied-commands))
         (not (and node (memq (syntax-node-kind node)
                             denied-kinds))))))

;; Each frame is (level . closed?). A greater level attaches a subproof to
;; the previous step; a following sibling can only resume an open frame.
;; QED may itself have a subproof, so closing is delayed until the next level.
(def (validate-proof proof)
  (let (steps (map field-node (fields proof step-field)))
    (let loop ((rest steps) (stack '()) (previous #f))
      (if (null? rest)
        (if (every cdr stack) #f
          (proof-error (or previous proof) "structured proof is missing QED"))
        (let* ((step (car rest))
               (level (proof-step-level step (and (pair? stack) (caar stack))))
               (higher? (and (pair? stack) (> level (caar stack))))
               (open-stack
                (if higher? stack
                  (let pop ((frames stack))
                    (if (and (pair? frames) (cdar frames))
                      (pop (cdr frames)) frames)))))
          (cond
           ((and previous higher? (not (step-admits-proof? previous)))
            (proof-error step "this proof step cannot have a subproof"))
           ((and previous (not higher?) (null? open-stack))
            (proof-error step "proof step follows the completed outer QED"))
           ((and (pair? open-stack) (not higher?) (not (= level (caar open-stack))))
            (proof-error step "proof step has an inconsistent level"))
           (else
            (let* ((frames (if (or (null? open-stack) higher?)
                             (cons (cons level #f) open-stack) open-stack))
                   (next (cons (cons level (equal? (step-command step) terminal-command))
                               (cdr frames))))
              (loop (cdr rest) next step)))))))))

(def (validate-label node)
  (let (name (field-node (car (fields node name-field))))
    (and (not
          (or (and name (eq? (syntax-node-kind name) name-kind))
              (and name (eq? (syntax-node-kind name) application-kind)
                   (not (field-node (car (fields name operator-field))))
                   (every (lambda (argument)
                            (let (value (field-node argument))
                              (and value (eq? (syntax-node-kind value) name-kind))))
                          (fields name argument-field)))))
         (proof-error node "label requires a name and identifier parameters"))))

(def (structured-proof-diagnostic value)
  (or (and (syntax-node? value)
           (cond ((eq? (syntax-node-kind value) proof-kind) (validate-proof value))
                 ((eq? (syntax-node-kind value) label-kind) (validate-label value))
                 (else #f)))
      (let loop ((rest (children value)))
        (and (pair? rest)
             (or (structured-proof-diagnostic (car rest)) (loop (cdr rest)))))))

structured-proof-diagnostic))
