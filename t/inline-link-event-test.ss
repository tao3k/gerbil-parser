;;; Non-Org link declarations exercise the engine's native commit boundary.
(import :std/test
        (only-in :gerbil-parser/src/modules/parser/interface
                 make-text-line make-inline-link source-inline-link-initial
                 source-inline-link-scan-forms)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold))
(export inline-link-event-test)
(def rule (make-text-line 'Text 'Raw
             (make-inline-link "<<" "::" ">>" 'Reference 'Target 'Label 'Boundary)))
(def (events source (literal? #f) (invalid-target 'recover-span))
  (run-event-fold source 'Document
    (append (source-inline-link-initial rule) '((inline-cursor 0) (policy 7)))
    `((for-line-bytes inline-byte-index start (line-content-end)
        ,(source-inline-link-scan-forms rule
          description-helper: 'label parameters: '(policy)
          description-node: 'Description literal-description?: literal?
          invalid-target: invalid-target))
      (token Raw (state-offset inline-cursor) end))
    '()
    '((label ((policy 0))
       ((if (uint-equal? (state policy) (uint 7))
            ((token Label start end)) ((token Wrong start end)))) (policy)))))
(def (starts result node)
  (length (filter (lambda (event) (equal? event (list 'start node))) result)))
(def (covers? result source)
  (let loop ((rest result) (offset 0) (depth 0))
    (if (null? rest)
      (and (= offset (u8vector-length (string->utf8 source))) (= depth 0))
      (let (event (car rest))
        (case (car event)
          ((token) (and (= offset (caddr event))
                        (<= offset (cadddr event))
                        (loop (cdr rest) (cadddr event) depth)))
          ((start) (loop (cdr rest) offset (+ depth 1)))
          ((finish) (and (> depth 0) (loop (cdr rest) offset (- depth 1)))))))))
(def inline-link-event-test
  (test-suite "native inline link declarations"
    (test-case "custom markers and Unicode commit one source-backed node"
      (let* ((source "α <<β::γ>> z") (result (events source)))
        (check (starts result 'Reference) => 1)
        (check (starts result 'Description) => 1)
        (check (covers? result source) => #t)))
    (test-case "adjacent links do not reconsume committed delimiters"
      (let* ((source "<<a>><<b::c>>") (result (events source)))
        (check (starts result 'Reference) => 2)
        (check (covers? result source) => #t)))
    (test-case "target-only form has no artificial description"
      (check (starts (events "<<a>>") 'Description) => 0)
      (check (covers? (events "<<a>>\r\n") "<<a>>\r\n") => #t))
    (test-case "literal description bypasses the registered nested helper"
      (let* ((source "<<a::b>>") (result (events source #t)))
        (check (starts result 'Reference) => 1)
        (check (starts result 'Description) => 0)
        (check (and (member '(token Label 5 6) result) #t) => #t)
        (check (covers? result source) => #t)))
    (test-case "invalid and truncated candidates never leak partial nodes"
      (for-each
       (lambda (source)
         (let (result (events source))
           (check (starts result 'Reference) => 0)
           (check (covers? result source) => #t)))
       '("" "<<" "<<a" "<<a::b" "<<>>" "<<::b>>")))
    (test-case "malformed policy and missing link declarations are rejected"
      (check-exception (source-inline-link-initial (make-text-line 'Text 'Raw)) true)
      (check-exception (source-inline-link-scan-forms rule parameters: '(policy)) true)
      (check-exception (source-inline-link-scan-forms rule description-helper: 'label parameters: '(p p)) true)
      (check-exception (source-inline-link-scan-forms rule description-node: "bad") true)
      (check-exception (source-inline-link-scan-forms rule invalid-target: 'unknown) true))
    (test-case "recovery scope is declared rather than inherited from Org"
      (let (source "<<::x>> then <<a>>")
        (check (starts (events source) 'Reference) => 1)
        (check (starts (events source #f 'recover-region) 'Reference) => 0)
        (check (covers? (events source) source) => #t)
        (check (covers? (events source #f 'recover-region) source) => #t)))
    (test-case "opening marker bytes cannot become a premature separator"
      (let* ((alternate (make-text-line 'Text 'Raw
                          (make-inline-link "<:" ":" ">" 'Reference 'Target 'Label 'Boundary)))
             (source "<:a:b>")
             (result (run-event-fold source 'Document
                       (append (source-inline-link-initial alternate) '((inline-cursor 0)))
                       `((for-line-bytes inline-byte-index start (line-content-end)
                           ,(source-inline-link-scan-forms alternate))
                         (token Raw (state-offset inline-cursor) end)) '())))
        (check (starts result 'Reference) => 1)
        (check (and (member '(token Target 2 3) result) #t) => #t)
        (check (covers? result source) => #t)))))
