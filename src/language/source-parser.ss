;;; Execute an already admitted source descriptor; never compile a strategy.
(import ./source-descriptor
        (only-in ../runtime/parse-cost with-parser-cost-stage)
        (only-in ../runtime/artifact parse-artifact-ref parse-artifact-valid-for-source?)
        (only-in ../runtime/source-scanner source-scanner-contextual?
                 make-source-scan-session source-scan-session-tokens
                 source-scan-session-scanned-token-count source-scan-session-reused-token-count))
(export parse-source-language parse-source-language/receipt deflanguage-parser-receipt
        parse-source-language/session source-language-session? source-language-session-artifact
        source-language-history? source-language-session-history
        source-language-session-scanned-token-count source-language-session-reused-token-count)

(def (validate-source-publication! descriptor source artifact)
  (unless (and (equal? (parse-artifact-ref artifact 'grammarDigest) (source-language-digest descriptor))
               (parse-artifact-valid-for-source? artifact source))
    (error "source parser published an invalid artifact"))
  artifact)
(def (parse-source-language/receipt descriptor source)
  (unless (and (string? source) (source-language? descriptor) (procedure? (source-language-receipt descriptor)))
    (error "source engine does not declare receipts for this input"))
  (let-values (((artifact receipt)
                ((source-language-receipt descriptor) source (source-language-scanner descriptor)
                 (source-language-digest descriptor))))
    (with-parser-cost-stage 'artifact-validation
      (validate-source-publication! descriptor source artifact))
    (values artifact receipt)))

(defrules deflanguage-parser-receipt ()
  ((_ binding descriptor)
   (def (binding source) (parse-source-language/receipt descriptor source))))

(def (parse-source-language descriptor source)
  (unless (string? source)
    (error "source language requires a string" source))
  (let* ((digest (source-language-digest descriptor))
         (artifact
          ((source-language-parse descriptor)
           source (source-language-scanner descriptor) digest)))
    (with-parser-cost-stage 'artifact-validation
      (validate-source-publication! descriptor source artifact))))

;;; Source sessions reuse only certified lexical suffixes. Recognition and
;;; canonical publication run through the existing engine on the new source.
(defstruct source-language-history (descriptor snapshot))
(defstruct (source-language-session source-language-history) (artifact))
;;; Retain only lexical history between foreign calls, not the published tree.
(def (source-language-session-history session)
  (make-source-language-history (source-language-history-descriptor session)
                               (source-language-history-snapshot session)))
(def (source-language-session-scanned-token-count session)
  (let (snapshot (source-language-history-snapshot session))
    (and snapshot (source-scan-session-scanned-token-count snapshot))))
(def (source-language-session-reused-token-count session)
  (let (snapshot (source-language-history-snapshot session))
    (if snapshot (source-scan-session-reused-token-count snapshot) 0)))
(def (parse-source-language/session descriptor source (previous #f))
  (unless (and (source-language? descriptor) (string? source)
               (or (not previous)
                   (and (source-language-history? previous)
                        (eq? descriptor (source-language-history-descriptor previous)))))
    (error "source session requires the same language declaration"))
  (let* ((owned (with-parser-cost-stage 'source-copy (string-copy source)))
         (worker (with-parser-cost-stage 'source-prepare ((source-language-factory descriptor) owned)))
         (snapshot #f)
         (artifact
          ((source-language-parse descriptor) owned
           (lambda (requested)
             (unless (eq? requested owned) (error "source session scanner received foreign input"))
             (if (source-scanner-contextual? worker)
               (begin
                 (set! snapshot (with-parser-cost-stage 'source-scan
                   (make-source-scan-session worker #f
                     (and previous (source-language-history-snapshot previous)))))
                 (source-scan-session-tokens snapshot))
               ((source-language-scanner descriptor) requested)))
           (source-language-digest descriptor))))
    ;; A lexical failure is handled by the original parser's diagnostic path;
    ;; it produces no cache and cannot authorize reuse in the next edit.
    (with-parser-cost-stage 'artifact-validation
      (validate-source-publication! descriptor owned artifact))
    (make-source-language-session descriptor snapshot artifact)))
