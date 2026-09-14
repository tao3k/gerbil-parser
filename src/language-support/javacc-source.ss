;;; -*- Gerbil -*-
;;; JavaCC source admission for pinned native parser grammars.

(import :gerbil-parser/src/runtime/identity
        (only-in :std/srfi/13 string-trim-both)
        (only-in :gerbil-parser/src/utilities/strings
                 ascii-whitespace? string-prefix-at? string-index-from
                 text-lines))
(export +javacc-source-schema+
        javacc-production? javacc-production-name javacc-production-result
        javacc-production-line
        javacc-source? javacc-source-language javacc-source-version
        javacc-source-commit javacc-source-digest javacc-source-productions
        javacc-source-production
        javacc-source->datum javacc-source-from-datum
        parse-javacc-source parse-javacc-source/expected)

(def +javacc-source-schema+ "gerbil-parser.javacc-source.v1")

(defstruct javacc-production (name result line) transparent: #t)
(defstruct javacc-source
  (schema language version commit digest productions)
  transparent: #t)

(def (identifier-character? ch)
  (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))

(def (identifier? text)
  (and (positive? (string-length text))
       (or (char-alphabetic? (string-ref text 0))
           (char=? (string-ref text 0) #\_))
       (let loop ((offset 1))
         (or (= offset (string-length text))
             (and (identifier-character? (string-ref text offset))
                  (loop (+ offset 1)))))))

(def (last-word text)
  (let (length (string-length text))
    (let left ((offset (- length 1)))
      (cond
       ((negative? offset) "")
       ((ascii-whitespace? (string-ref text offset))
        (left (- offset 1)))
       (else
        (let start ((begin offset))
          (if (or (zero? begin)
                  (ascii-whitespace? (string-ref text (- begin 1))))
            (substring text begin (+ offset 1))
            (start (- begin 1)))))))))

(def (return-type text)
  (def (admitted type)
    (and (string-prefix-at? text type 0)
         (let (tail (string-trim-both
                     (substring text (string-length type)
                                (string-length text))))
           (and (or (zero? (string-length tail))
                    (string-prefix-at? tail "/*" 0)
                    (string-prefix-at? tail "//" 0)
                    (signature-name text))
                type))))
  (or (admitted "SyntaxTreeNode") (admitted "Token") (admitted "void")))

;;; A production header is the only JavaCC surface admitted here.  Java action
;;; bodies are deliberately opaque: they are neither evaluated nor translated
;;; into the parser runtime.
(def (signature-name text)
  (let* ((open (string-index-from text "("))
         (close (and open (string-index-from text ")" (+ open 1))))
         (colon (and close (string-index-from text ":" (+ close 1)))))
    (and open close colon
         (let (name (last-word (substring text 0 open)))
           (and (identifier? name) name)))))

(def (comment-line? text)
  (or (zero? (string-length text))
      (string-prefix-at? text "/*" 0)
      (string-prefix-at? text "*" 0)
      (string-prefix-at? text "*/" 0)))

(def (validate-productions productions)
  (let (catalog (make-table test: equal?))
    (for-each
     (lambda (production)
       (let (name (javacc-production-name production))
         (when (table-ref catalog name #f)
           (error "duplicate JavaCC production" name))
         (table-set! catalog name production)))
     productions)
    productions))

(def (scan-productions source)
  (let loop ((rest (text-lines source))
             (line 1) (pending-result #f) (found '()))
    (if (null? rest)
      (validate-productions (reverse found))
      (let* ((text (string-trim-both (car rest)))
             (inline-result (return-type text))
             (name (signature-name text))
             (result (or inline-result pending-result)))
        (cond
         ((and result name)
          (loop (cdr rest) (+ line 1) #f
                (cons (make-javacc-production name result line) found)))
         ((and inline-result (not name))
          (loop (cdr rest) (+ line 1) inline-result found))
         ((and pending-result (comment-line? text))
          (loop (cdr rest) (+ line 1) pending-result found))
         (else
          (loop (cdr rest) (+ line 1) #f found)))))))

(def (parse-javacc-source language version commit source)
  (unless (and (string? language) (string? version)
               (string? commit) (string? source))
    (error "JavaCC source identity and content must be strings"))
  (make-javacc-source
   +javacc-source-schema+ language version commit (sha256-text source)
   (scan-productions source)))

(def (parse-javacc-source/expected language version commit expected-digest source)
  (let (catalog (parse-javacc-source language version commit source))
    (unless (string=? (javacc-source-digest catalog) expected-digest)
      (error "JavaCC source digest mismatch"
             expected-digest (javacc-source-digest catalog)))
    catalog))

(def (javacc-source-production source name)
  (find (lambda (production)
          (string=? (javacc-production-name production) name))
        (javacc-source-productions source)))

(def (javacc-source->datum source)
  (list +javacc-source-schema+
        (javacc-source-language source)
        (javacc-source-version source)
        (javacc-source-commit source)
        (javacc-source-digest source)
        (map (lambda (production)
               (list (javacc-production-name production)
                     (javacc-production-result production)
                     (javacc-production-line production)))
             (javacc-source-productions source))))

(def (javacc-source-from-datum value)
  (unless (and (list? value) (= (length value) 6)
               (string=? (car value) +javacc-source-schema+))
    (error "invalid materialized JavaCC source v1" value))
  (make-javacc-source
   (car value) (cadr value) (caddr value) (cadddr value) (list-ref value 4)
   (map (lambda (row)
          (make-javacc-production (car row) (cadr row) (caddr row)))
        (list-ref value 5))))
