;;; -*- Gerbil -*-
;;; Pure validation of the level references retained by SANY proof syntax.
(import :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/src/runtime/token token? token-lexeme))
(export sany-proof-diagnostic)

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
        (cons 'code "GERBIL-PARSER-TLA-PLUS-SANY-PROOF")
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
  (let* ((name (token-lexeme (first-token (car (fields step 'name)))))
         (close (let loop ((at 1))
                  (if (char=? (string-ref name at) #\>) at (loop (+ at 1)))))
         (level (substring name 1 close)))
    (cond ((equal? level "*") (or current 1))
          ((equal? level "+") (+ (or current 0) 1))
          (else (string->number level)))))

(def (step-command step)
  (token-lexeme (first-token (car (fields step 'body)))))

(def (step-admits-proof? step)
  (let* ((body (car (fields step 'body))) (node (field-node body)))
    (and (not (member (step-command step)
                     '("USE" "HIDE" "INSTANCE" "DEFINE" "HAVE" "TAKE" "WITNESS")))
         (not (and node (memq (syntax-node-kind node)
                             '(LocalDefinition LocalFunctionDefinition OperatorDefinition
                               FunctionDefinition InstanceDeclaration)))))))

;; Each frame is (level . closed?). A greater level attaches a subproof to
;; the previous step; a following sibling can only resume an open frame.
;; QED may itself have a subproof, so closing is delayed until the next level.
(def (validate-proof proof)
  (let (steps (map field-node (fields proof 'step)))
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
                   (next (cons (cons level (equal? (step-command step) "QED"))
                               (cdr frames))))
              (loop (cdr rest) next step)))))))))

(def (validate-label node)
  (let (name (field-node (car (fields node 'name))))
    (and (not
          (or (and name (eq? (syntax-node-kind name) 'NameExpression))
              (and name (eq? (syntax-node-kind name) 'OperatorApplication)
                   (not (field-node (car (fields name 'operator))))
                   (every (lambda (argument)
                            (let (value (field-node argument))
                              (and value (eq? (syntax-node-kind value) 'NameExpression))))
                          (fields name 'argument)))))
         (proof-error node "label requires a name and identifier parameters"))))

(def (sany-proof-diagnostic value)
  (or (and (syntax-node? value)
           (case (syntax-node-kind value)
             ((Proof) (validate-proof value))
             ((LabelExpression) (validate-label value))
             (else #f)))
      (let loop ((rest (children value)))
        (and (pair? rest)
             (or (sany-proof-diagnostic (car rest)) (loop (cdr rest)))))))
