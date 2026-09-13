;;; -*- Gerbil -*-
;;; Runtime admission boundary for materialized language IR sidecars.

(import (only-in ./identity sha256-text)
        (only-in :std/sugar cut)
        (only-in :std/text/utf8 utf8->string)
        (only-in :std/text/zlib uncompress))
(export compiled-language-artifact-relative-path
        load-compiled-language-artifact
        load-compiled-language-artifact/roots)

(def +compiled-language-artifact-prefix+
  "gerbil-parser/compiled-language-artifacts/")

;; : (-> Datum Boolean)
(def (sha256-identity? value)
  (and (string? value)
       (= (string-length value) 71)
       (string=? (substring value 0 7) "sha256:")
       (let lp ((index 7))
         (or (= index 71)
             (and (memv (string-ref value index)
                        '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                          #\a #\b #\c #\d #\e #\f))
                  (lp (fx+ index 1)))))))

;; : (-> String String)
(def (compiled-language-artifact-relative-path digest)
  (unless (sha256-identity? digest)
    (error "invalid compiled language artifact digest" digest))
  (string-append +compiled-language-artifact-prefix+ digest ".gir.z"))

;;; Admission is fail-closed: storage location, content identity, complete
;;; datum framing, and schema must agree before generated LR state is exposed.
;; The roots variant makes the complete storage admission independently
;; testable without mutating Gerbil's process-global load path.
;; : (forall (a) (-> String [String] [String] [(Pair Symbol a)]))
;; : (-> String List List Alist)
(def (load-compiled-language-artifact/roots expected-schema locator roots)
  (def (resolve relative-path)
    (or (find file-exists?
              (map (cut path-expand relative-path <>)
                   roots))
        (error "compiled language artifact sidecar is unavailable"
               expected-schema relative-path)))
  (unless (and (string? expected-schema)
               (list? roots)
               (not (find (lambda (root) (not (string? root))) roots))
               (list? locator)
               (= (length locator) 2)
               (string? (car locator))
               (sha256-identity? (cadr locator))
               (equal? (car locator)
                       (compiled-language-artifact-relative-path
                        (cadr locator))))
    (error "invalid compiled language artifact locator"
           expected-schema locator))
  (let* ((relative-path (car locator))
         (expected-digest (cadr locator))
         (path (resolve relative-path))
         (serialized
          (utf8->string (call-with-input-file path uncompress)))
         (actual-digest (sha256-text serialized)))
    (unless (equal? actual-digest expected-digest)
      (error "compiled language artifact digest mismatch"
             expected-schema expected-digest actual-digest path))
    (call-with-input-string
     serialized
     (lambda (port)
       (let ((value (read port))
             (trailing (read port)))
         (unless (eof-object? trailing)
           (error "compiled language artifact contains trailing data"
                  expected-schema))
         (unless (and (list? value)
                      (alet (row (assq 'schema value))
                        (equal? (cdr row) expected-schema)))
           (error "compiled language artifact schema mismatch"
                  expected-schema value))
         value)))))

;; : (forall (a) (-> String [String] [(Pair Symbol a)]))
;; : (-> String List Alist)
(def (load-compiled-language-artifact expected-schema locator)
  (load-compiled-language-artifact/roots
   expected-schema locator
   (cons (path-expand "lib" (gerbil-path)) (load-path))))
