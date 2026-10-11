;;; -*- Gerbil -*-
;;; Engine descriptor dispatch and lossless policy execution.

(import (only-in ../runtime/parser parse-source)
        (only-in ../runtime/lr-parser current-lr-branch-budget)
        (only-in ../runtime/cst parse-artifact->cst)
        (only-in ../runtime/token make-token)
        (only-in ../runtime/artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-ref
                 parse-artifact-roundtrip parse-artifact-status event-kind
                 parse-artifact-events token-event? token-event-token-kind
                 token-event-lexeme event-start event-end make-failure-parse-artifact)
        (only-in ./descriptor
                 language-grammar? language-grammar-contract language-grammar-language
                 language-grammar-machine language-grammar-observability
                 language-grammar-version language-grammar-parser-policy
                 language-parser-policy-branch-budget language-parser-policy-cst-validator)
        (only-in ./source
                 parse-source-language source-language?
                 source-language-contract source-language-language
                 source-language-version source-language-digest source-language-scanner-factory))
(export parse-language-source call-with-language-parser-policy)

(def (call-with-language-parser-policy descriptor source recognize)
  (let (policy (language-grammar-parser-policy descriptor))
    (if (not policy) (recognize)
      (parameterize ((current-lr-branch-budget (language-parser-policy-branch-budget policy)))
        (let* ((artifact (recognize))
               (diagnostic
                (and (parse-artifact-success? artifact)
                     ((language-parser-policy-cst-validator policy)
                      (parse-artifact->cst artifact)))))
          (if (not diagnostic) artifact
            (let (rejected
                  (make-failure-parse-artifact
                   (parse-artifact-ref artifact 'grammarDigest) source
                   (map (lambda (event)
                          (make-token (token-event-token-kind event)
                                      (token-event-lexeme event)
                                      (event-start event) (event-end event)))
                        (filter token-event? (parse-artifact-events artifact)))
                   diagnostic))
              (unless (parse-artifact-valid? rejected)
                (error "language parser policy returned an invalid diagnostic" diagnostic))
              rejected)))))))

(def (parse-language-source descriptor source)
  (cond
   ((source-language? descriptor) (parse-source-language descriptor source))
   ((language-grammar? descriptor)
    (call-with-language-parser-policy
     descriptor source
     (lambda ()
       (parse-source (language-grammar-machine descriptor) source
                     (language-grammar-observability descriptor)))))
   (else (error "language loader requires a language descriptor" descriptor))))
