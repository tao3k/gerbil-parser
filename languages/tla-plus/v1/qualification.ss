;;; -*- Gerbil -*-
;;; Native TLA+ syntax and official TLC model-check qualification boundary.

(import (only-in :std/misc/ports read-all-as-string)
        (only-in :std/misc/process run-process)
        (only-in :std/srfi/1 find last)
        (only-in :std/srfi/13
                 string-contains string-prefix? string-trim-right)
        (only-in ./parser parse-tla-plus-v1)
        (only-in ../../../src/runtime/artifact
                 parse-artifact-roundtrip
                 parse-artifact-success?
                 parse-artifact-valid?)
        (only-in ../../../src/runtime/identity sha256-text))

(export +tla-plus-model-qualification-schema+
        qualify-tla-plus-model
        tla-plus-model-receipt?
        tla-plus-model-receipt-admitted
        tla-plus-model-receipt-output
        tla-plus-model-receipt->alist)

(def +tla-plus-model-qualification-schema+
  "gerbil-parser.tla-plus-model-qualification.v1")

(defstruct tla-plus-model-receipt
  (schema syntax-contract model source-digest config-digest
   syntax-accepted roundtrip tool tool-path tool-digest tlc-version workers
   exit-status completed states-generated distinct-states states-left
   graph-depth admitted output-digest output)
  transparent: #t)

(def (read-text path)
  (call-with-input-file path read-all-as-string))

(def (decimal-prefix token)
  (let loop ((offset 0))
    (if (and (< offset (string-length token))
             (char-numeric? (string-ref token offset)))
      (loop (+ offset 1))
      (and (> offset 0)
           (string->number (substring token 0 offset))))))

(def (line-with-prefix lines prefix)
  (find (lambda (line) (string-prefix? prefix line)) lines))

(def (line-containing lines fragment)
  (find (lambda (line) (string-contains line fragment)) lines))

(def (parse-tlc-output output)
  (let* ((lines (string-split output #\newline))
         (version-prefix "TLC2 Version ")
         (version-line (line-with-prefix lines version-prefix))
         (states-line (line-containing lines " states generated, "))
         (depth-line
          (line-with-prefix lines
                            "The depth of the complete state graph search is "))
         (states (and states-line (string-split states-line #\space)))
         (depth (and depth-line (string-split depth-line #\space))))
    (list
     (cons 'completed
           (and (member "Model checking completed. No error has been found."
                        lines)
                #t))
     (cons 'tlc-version
           (and version-line
                (substring version-line
                           (string-length version-prefix)
                           (string-length version-line))))
     (cons 'states-generated
           (and states (>= (length states) 8)
                (decimal-prefix (list-ref states 0))))
     (cons 'distinct-states
           (and states (>= (length states) 8)
                (decimal-prefix (list-ref states 3))))
     (cons 'states-left
           (and states (>= (length states) 8)
                (decimal-prefix (list-ref states 7))))
     (cons 'graph-depth
           (and depth (decimal-prefix (last depth)))))))

(def (summary-ref summary key)
  (let (entry (assq key summary))
    (and entry (cdr entry))))

(def (natural-number? value)
  (and (exact-integer? value) (>= value 0)))

(def (run-command command directory)
  (let (status #f)
    (let (output
          (run-process command
                       directory: directory
                       stderr-redirection: #t
                       check-status:
                       (lambda (exit-status _settings)
                         (set! status exit-status))))
      (values output status))))

(def (call-with-tlc-temporary-directory procedure)
  (let* ((template
          (path-expand "gerbil-parser-tlc.XXXXXX" (getenv "TMPDIR" "/tmp")))
         (directory
          (string-trim-right (run-process ["mktemp" "-d" template]))))
    (unwind-protect
      (procedure directory)
      (when (file-exists? directory)
        (delete-file-or-directory directory #t)))))

(def (resolve-tool executable)
  (path-normalize
   (string-trim-right (run-process ["which" executable]))))

(def (qualification-admitted? syntax-accepted roundtrip exit-status summary)
  (and syntax-accepted
       roundtrip
       (zero? exit-status)
       (summary-ref summary 'completed)
       (summary-ref summary 'tlc-version)
       (natural-number? (summary-ref summary 'states-generated))
       (natural-number? (summary-ref summary 'distinct-states))
       (equal? (summary-ref summary 'states-left) 0)
       (natural-number? (summary-ref summary 'graph-depth))))

(def (syntax-rejection-receipt model source config-source
                               syntax-accepted roundtrip tlc workers)
  (make-tla-plus-model-receipt
   +tla-plus-model-qualification-schema+
   "tla-plus.native-core.v1"
   model
   (sha256-text source)
   (sha256-text config-source)
   syntax-accepted
   roundtrip
   tlc
   #f #f #f
   workers
   #f #f #f #f #f #f
   #f
   (sha256-text "")
   ""))

(def (qualify-tla-plus-model spec-path config-path
                             tlc: (tlc "tlc")
                             workers: (workers 1))
  (unless (and (exact-integer? workers) (> workers 0))
    (error "TLC workers must be a positive integer" workers))
  (let* ((spec (path-normalize spec-path))
         (config (path-normalize config-path))
         (directory (path-directory spec)))
    (unless (equal? directory (path-directory config))
      (error "TLA+ specification and configuration must share a directory"
             spec config))
    (let* ((source (read-text spec))
           (config-source (read-text config))
           (artifact (parse-tla-plus-v1 source))
           (syntax-accepted
            (and (parse-artifact-success? artifact)
                 (parse-artifact-valid? artifact)))
           (roundtrip
            (and syntax-accepted
                 (equal? source (parse-artifact-roundtrip artifact))))
           (model (path-strip-extension (path-strip-directory spec))))
      (if (not roundtrip)
        (syntax-rejection-receipt
         model source config-source syntax-accepted roundtrip tlc workers)
        (let* ((tool-path (resolve-tool tlc))
               (tool-source (read-text tool-path)))
          (call-with-tlc-temporary-directory
           (lambda (metadir)
             (let-values (((output exit-status)
                           (run-command
                            [tool-path
                             "-config" (path-strip-directory config)
                             "-workers" (number->string workers)
                             "-metadir" metadir
                             model]
                            directory)))
               (let* ((summary (parse-tlc-output output))
                      (admitted
                       (qualification-admitted?
                        syntax-accepted roundtrip exit-status summary)))
                 (make-tla-plus-model-receipt
                  +tla-plus-model-qualification-schema+
                  "tla-plus.native-core.v1"
                  model
                  (sha256-text source)
                  (sha256-text config-source)
                  syntax-accepted
                  roundtrip
                  tlc
                  tool-path
                  (sha256-text tool-source)
                  (summary-ref summary 'tlc-version)
                  workers
                  exit-status
                  (summary-ref summary 'completed)
                  (summary-ref summary 'states-generated)
                  (summary-ref summary 'distinct-states)
                  (summary-ref summary 'states-left)
                  (summary-ref summary 'graph-depth)
                  admitted
                  (sha256-text output)
                  output))))))))))

(def (tla-plus-model-receipt->alist receipt)
  (list
   (cons 'schema (tla-plus-model-receipt-schema receipt))
   (cons 'syntax-contract
         (tla-plus-model-receipt-syntax-contract receipt))
   (cons 'model (tla-plus-model-receipt-model receipt))
   (cons 'source-digest (tla-plus-model-receipt-source-digest receipt))
   (cons 'config-digest (tla-plus-model-receipt-config-digest receipt))
   (cons 'syntax-accepted
         (tla-plus-model-receipt-syntax-accepted receipt))
   (cons 'roundtrip (tla-plus-model-receipt-roundtrip receipt))
   (cons 'tool (tla-plus-model-receipt-tool receipt))
   (cons 'tool-path (tla-plus-model-receipt-tool-path receipt))
   (cons 'tool-digest (tla-plus-model-receipt-tool-digest receipt))
   (cons 'tlc-version (tla-plus-model-receipt-tlc-version receipt))
   (cons 'workers (tla-plus-model-receipt-workers receipt))
   (cons 'exit-status (tla-plus-model-receipt-exit-status receipt))
   (cons 'completed (tla-plus-model-receipt-completed receipt))
   (cons 'states-generated
         (tla-plus-model-receipt-states-generated receipt))
   (cons 'distinct-states
         (tla-plus-model-receipt-distinct-states receipt))
   (cons 'states-left (tla-plus-model-receipt-states-left receipt))
   (cons 'graph-depth (tla-plus-model-receipt-graph-depth receipt))
   (cons 'admitted (tla-plus-model-receipt-admitted receipt))
   (cons 'output-digest
         (tla-plus-model-receipt-output-digest receipt))))
