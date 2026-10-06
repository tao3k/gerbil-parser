;;; Result declarations constrain construction and are ordinary inheritable POO slots.
(import :std/test
        (only-in :clan/poo/object .o)
        :gerbil-parser/src/language/result-profile
        (only-in :gerbil-parser/src/language/scanner-profile ScannerProfile. defscanner-profile)
        :gerbil-parser/src/runtime/recognition
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/language/source declare-source-language source-language-digest source-language-result-catalog)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.))
(export result-profile-test)
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(defresult-profile (base-results :: self ResultProfile.)
  (nodes (File item) (Item text)) (tokens text))
(defresult-profile (extended-results :: self base-results)
  (nodes (File item extra) (Item text) (Extra text)))
(defscanner-profile (independent-scanner :: self ScannerProfile.)
  (modes main) (initial-mode main) (tokens raw)
  (rules (value main raw (literal "x") 0 keep)))
(defresult-profile (scanner-results :: self base-results) (scanner independent-scanner))
(defresult-profile (token-results :: self base-results) (tokens other))
(def result-profile-test
  (test-suite "declarative result construction"
    (test-case "inheritance compiles execution operations and the same catalog"
      (let* ((plan (compile-result-profile extended-results)) (token (make-token 'text "α" 0 2))
             (node (result-plan-node plan 'Extra 0 2 (list (make-recognition-child 'text token)))))
        (check (recognition-node-kind node) => 'Extra)
        (check (result-plan-catalog plan)
               => '((syntax-kinds (File result (item extra)) (Item result (text)) (Extra result (text)))
                    (terminals (text token))))))
    (test-case "single value slots override and scanner terminals derive without repeated declarations"
      (check (result-plan-catalog (compile-result-profile scanner-results))
             => '((syntax-kinds (File result (item)) (Item result (text))) (terminals (text token) (raw token))))
      (check (result-plan-catalog (compile-result-profile token-results))
             => '((syntax-kinds (File result (item)) (Item result (text))) (terminals (other token)))))
    (test-case "undeclared kinds, fields and terminals reject at construction"
      (let ((plan (compile-result-profile base-results)) (token (make-token 'text "x" 0 1)))
        (for-each (lambda (thunk) (check (rejects? thunk) => #t))
          (list (lambda () (result-plan-node plan 'Other 0 1 '()))
                (lambda () (result-plan-node plan 'Item 0 1 (list (make-recognition-child 'other token))))
                (lambda () (result-plan-token plan (make-token 'other "x" 0 1)))
                (lambda () (result-plan-node plan 'Item 0 1 (list (make-recognition-child 'text (make-recognition-node 'Other 0 1 '())))))))))
    (test-case "invalid bounds and nonrecognition children reject"
      (let (plan (compile-result-profile base-results))
        (for-each (lambda (thunk) (check (rejects? thunk) => #t))
          (list (lambda () (result-plan-node plan 'Item -1 1 '()))
                (lambda () (result-plan-node plan 'Item 0 0 (list (make-recognition-child 'text (make-token 'text "x" 0 1)))))
                (lambda () (result-plan-node plan 'Item 0 1 (list (make-recognition-child 'text #f))))))))
    (test-case "duplicate declarations reject before binding"
      (for-each (lambda (profile) (check (rejects? (lambda () (compile-result-profile profile))) => #t))
        (list (.o (:: self base-results) nodes: '((File (item item))))
              (.o (:: self base-results) nodes: '((File ()) (File ())))
              (.o (:: self base-results) tokens: '(text text)))))
    (test-case "input and returned catalogs cannot change an admitted plan"
      (let* ((declarations (list (list 'File (list 'item))))
             (plan (compile-result-profile (.o (:: self ResultProfile.) nodes: declarations tokens: '(text))))
             (recipe (result-plan-recipe plan)) (catalog (result-plan-catalog plan)))
        (set-car! (cadar declarations) 'other)
        (check (result-plan-catalog plan) => '((syntax-kinds (File result (item))) (terminals (text token))))
        (set-car! (cadar (car recipe)) 'other)
        (check (result-plan-catalog plan) => '((syntax-kinds (File result (item))) (terminals (text token))))
        (set-car! (caddr (cadar catalog)) 'other)
        (check (result-plan-catalog plan) => '((syntax-kinds (File result (item))) (terminals (text token))))))
    (test-case "source identity includes construction operations, independent line language derives catalog"
      (let* ((strategy (.o (:: self LineSourceStrategy.) root-kind: 'RecordFile token-kind: 'Record))
             (source (declare-source-language "records" "1" "records.v1" strategy))
             (changed (declare-source-language "records" "1" "records.v1"
                        (.o (:: self strategy) results: (.o (:: result ResultProfile.) nodes: '((RecordFile (line extra))) tokens: '(Record))))))
        (check (source-language-result-catalog source)
               => '((syntax-kinds (RecordFile result (line))) (terminals (Record token))))
        (check (equal? (source-language-digest source) (source-language-digest changed)) => #f)))))
