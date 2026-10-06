;;; Admitted command roles are inherited values, never runtime callbacks.
(import :std/test (only-in :clan/poo/object .o .ref)
        :gerbil-parser/src/language/command-profile
        (only-in :gerbil-parser/src/language/result-profile compile-result-profile)
        (only-in :gerbil-parser/src/language/part-profile compile-part-profile)
        (only-in :gerbil-parser/src/runtime/shell-parser make-shell-parser)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/recognition recognition-node-kind)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-events parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/language/source source-language-scanner-factory source-language-digest declare-source-language)
        (only-in :gerbil-parser/src/runtime/source-engines ShellSourceStrategy.)
        (only-in :gerbil-parser/languages/bash/grammar bash-commands bash-results bash-parts bash-command-scanner bash-word-regions bash-source-language))
(export command-profile-test)
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def (scan source) (source-scanner-tokens ((source-language-scanner-factory bash-source-language) source) 'command))
(def (plan profile) (admit-command-plan (compile-command-profile profile) (compile-result-profile bash-results) bash-command-scanner))
(def command-profile-test
 (test-suite "closed command profile admission and extension"
  (test-case "token roles and complete descriptor profiles retain exact boundaries"
   (let (prepared (plan bash-commands))
    (check (and (command-plan-token? prepared 'pipeline (make-token 'operator "|&" 0 2)) #t) => #t)
    (check (command-plan-token? prepared 'pipeline (make-token 'word "|&" 0 2)) => #f)
    (for-each (lambda (text) (check (command-plan-text? prepared 'descriptor text) => #t)) '("123" "{α_123}"))
    (for-each (lambda (text) (check (command-plan-text? prepared 'descriptor text) => #f)) '("" "123x" "{x}tail" "{1x}"))))
  (test-case "inherited role overrides use the same engine and owned recipes"
   (let* ((declarations (map (lambda (row) (if (eq? (car row) 'pipeline) (list 'pipeline (list 'operator (string-copy "||"))) row)) (.ref bash-commands 'roles)))
          (profile (.o (:: self bash-commands) roles: declarations)) (prepared (plan profile))
          (recipe (command-plan-recipe prepared)))
     (string-set! (cadr (cadr (assq 'pipeline declarations))) 0 #\!)
     (string-set! (cadr (cadr (assq 'pipeline (car recipe)))) 0 #\!)
     (check (and (command-plan-token? prepared 'pipeline (make-token 'operator "||" 0 2)) #t) => #t)
     (check (command-plan-token? prepared 'pipeline (make-token 'operator "!|" 0 2)) => #f)
     (let-values (((parse receipt) (make-shell-parser bash-word-regions (compile-result-profile bash-results) (compile-part-profile bash-parts) prepared)))
       (check (parse-artifact-success? (parse "echo x || echo y\n" scan (source-language-digest bash-source-language))) => #t))))
  (test-case "projection values inherit and reject missing fields before parsing"
   (let* ((profile (.o (:: self bash-commands) projections: (map (lambda (row) (if (eq? (car row) 'BashFile) '(BashFile CommandList) row)) (.ref bash-commands 'projections))))
          (prepared (plan profile)))
     (let-values (((parse receipt) (make-shell-parser bash-word-regions (compile-result-profile bash-results) (compile-part-profile bash-parts) prepared)))
       (check (vector-ref (car (parse-artifact-events (parse "echo α\n" scan (source-language-digest bash-source-language)))) 2) => 'CommandList)))
   (check (rejects? (lambda () (plan (.o (:: self bash-commands) projections: (map (lambda (row) (if (eq? (car row) 'BashFile) '(BashFile Word) row)) (.ref bash-commands 'projections)))))) => #t))
  (test-case "malformed callback roles and foreign scanner terminals reject at admission"
   (for-each (lambda (profile) (check (rejects? (lambda () (plan profile))) => #t))
     (list (.o (:: self bash-commands) roles: '())
           (.o (:: self bash-commands) roles: (append (.ref bash-commands 'roles) '((pipeline (operator "|")))))
           (.o (:: self bash-commands) texts: '((descriptor (run (numeric) 0 #f))))
           (.o (:: self bash-commands) roles: (map (lambda (row) (if (eq? (car row) 'pipeline) '(pipeline (foreign "|")) row)) (.ref bash-commands 'roles)))
           (.o (:: self bash-commands) roles: (lambda () '())))))
  (test-case "command values participate in Source identity without a schema bump"
   (let (changed (declare-source-language "bash" "5.3" "bash-5.3-structured-source.v1"
    (.o (:: self ShellSourceStrategy.) regions: bash-word-regions scanner: bash-command-scanner results: bash-results parts: bash-parts
      commands: (.o (:: self bash-commands) projections: (map (lambda (row) (if (eq? (car row) 'BashFile) '(BashFile CommandList) row)) (.ref bash-commands 'projections))))))
     (check (equal? (source-language-digest changed) (source-language-digest bash-source-language)) => #f)))))
