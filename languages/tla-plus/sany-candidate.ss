;;; -*- Gerbil -*-
;;; Candidate syntax entry; grammar metadata owns its independent identity.

(import (only-in ./sany-proof sany-proof-diagnostic)
        (only-in :gerbil-parser/src/runtime/cst parse-artifact->cst)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-ref parse-artifact-events
                 token-event? token-event-token-kind token-event-lexeme event-start event-end
                 make-failure-parse-artifact)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-branch-budget)
        (only-in :gerbil-parser/src/language/entry deflanguage-parser)
        (only-in ./grammars/sany-candidate
                 tla-plus-sany-candidate-language-grammar
                 tla-plus-sany-candidate-parser))
(export tla-plus-sany-candidate-language
        tla-plus-sany-candidate-parser
        parse-tla-plus-sany-candidate)

(deflanguage-parser tla-plus-sany-candidate-language
  (grammar tla-plus-sany-candidate-language-grammar)
  (parse parse-tla-plus-sany-candidate/default-budget))

(def (parse-tla-plus-sany-candidate source)
  (parameterize ((current-lr-branch-budget 4096))
    (let (artifact (parse-tla-plus-sany-candidate/default-budget source))
      (let (diagnostic (and (parse-artifact-success? artifact)
                           (sany-proof-diagnostic (parse-artifact->cst artifact))))
        (if diagnostic
          (make-failure-parse-artifact
           (parse-artifact-ref artifact 'grammarDigest) source
           (map (lambda (event)
                  (make-token (token-event-token-kind event) (token-event-lexeme event)
                              (event-start event) (event-end event)))
                (filter token-event? (parse-artifact-events artifact)))
           diagnostic)
          artifact)))))
