(import :gerbil-parser/language-test-support)
;;; Non-Org link declarations exercise the engine's native commit boundary.
(import :std/test
        (only-in :gerbil-parser/src/modules/parser/interface
                 make-source-event-scope make-text-line make-inline-link source-inline-link-initial
                 source-inline-link-scan-forms)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold))
(def scope (make-source-event-scope 'native-test))

(def rule (make-text-line 'Text 'Raw
             (make-inline-link "<<" "::" ">>" 'Reference 'Target 'Label 'Boundary)))
(def (events source (literal? #f) (invalid-target 'recover-span))
  (run-event-fold source 'Document
    (append (source-inline-link-initial rule scope) '((scan-cursor 0) (policy 7)))
    `((for-line-bytes scan-index start (line-content-end)
        ,(source-inline-link-scan-forms rule scope index: 'scan-index cursor: 'scan-cursor
          description-helper: 'label parameters: '(policy)
          description-node: 'Description literal-description?: literal?
          invalid-target: invalid-target))
      (token Raw (state-offset scan-cursor) end))
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
(deflanguage-parser-tests inline-link-event-test "native inline link declarations"
  (engine)
    (property "custom markers and Unicode commit one source-backed node"
      (bindings (source "α <<β::γ>> z") (result (events source)))
      (equal (starts result 'Reference)  1)
      (equal (starts result 'Description)  1)
      (equal (covers? result source)  #t))
    (property "adjacent links do not reconsume committed delimiters"
      (bindings (source "<<a>><<b::c>>") (result (events source)))
      (equal (starts result 'Reference)  2)
      (equal (covers? result source)  #t))
    (property "target-only form has no artificial description"
      (bindings)
      (equal (starts (events "<<a>>") 'Description)  0)
      (equal (covers? (events "<<a>>\r\n") "<<a>>\r\n")  #t))
    (property "literal description bypasses the registered nested helper"
      (bindings (source "<<a::b>>") (result (events source #t)))
      (equal (starts result 'Reference)  1)
      (equal (starts result 'Description)  0)
      (equal (and (member '(token Label 5 6) result) #t)  #t)
      (equal (covers? result source)  #t))
    (scenario "invalid and truncated candidates never leak partial nodes"
      (each (source) ('("" "<<" "<<a" "<<a::b" "<<>>" "<<::b>>"))
         (scope ((result (events source)))
           (equal (starts result 'Reference)  0)
           (equal (covers? result source)  #t))))
    (scenario "malformed policy and missing link declarations are rejected"
      (raises (source-inline-link-scan-forms rule scope) true)
      (raises (source-inline-link-initial (make-text-line 'Text 'Raw) scope) true)
      (raises (source-inline-link-scan-forms rule scope index: 'scan-index cursor: 'scan-cursor parameters: '(policy)) true)
      (raises (source-inline-link-scan-forms rule scope index: 'scan-index cursor: 'scan-cursor description-helper: 'label parameters: '(p p)) true)
      (raises (source-inline-link-scan-forms rule scope index: 'scan-index cursor: 'scan-cursor description-node: "bad") true)
      (raises (source-inline-link-scan-forms rule scope index: 'scan-index cursor: 'scan-cursor invalid-target: 'unknown) true))
    (property "recovery scope is declared rather than inherited from Org"
      (bindings (source "<<::x>> then <<a>>"))
      (equal (starts (events source) 'Reference)  1)
      (equal (starts (events source #f 'recover-region) 'Reference)  0)
      (equal (covers? (events source) source)  #t)
      (equal (covers? (events source #f 'recover-region) source)  #t))
    (property "opening marker bytes cannot become a premature separator"
      (bindings (alternate (make-text-line 'Text 'Raw
                          (make-inline-link "<:" ":" ">" 'Reference 'Target 'Label 'Boundary))) (source "<:a:b>") (result (run-event-fold source 'Document
                       (append (source-inline-link-initial alternate scope) '((scan-cursor 0)))
                       `((for-line-bytes scan-index start (line-content-end)
                           ,(source-inline-link-scan-forms alternate scope index: 'scan-index cursor: 'scan-cursor))
                         (token Raw (state-offset scan-cursor) end)) '())))
      (equal (starts result 'Reference)  1)
      (equal (and (member '(token Target 2 3) result) #t)  #t)
      (equal (covers? result source)  #t)))
