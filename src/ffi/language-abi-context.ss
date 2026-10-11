;;; Language ABI context and descriptor catalogs shared by ABI publication.
(import (only-in :gerbil-scheme-rust/scheme/utf8 gerbil-rs-encode-utf8-into)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in ../grammar/algebra grammar-expression-fields)
        (only-in ../language/descriptor language-grammar-grammar)
        (only-in ../language/source source-language? source-language-result-catalog))
(export #t)

(defstruct language-abi-context
  (id descriptor parser syntax-kind-index terminal-index field-symbols field-index plan source-admission)
  transparent: #t)

(def (descriptor-section descriptor name)
  (if (and (source-language? descriptor) (eq? name 'rules)) '()
    (let (row (assq name (if (source-language? descriptor)
                          (source-language-result-catalog descriptor)
                          (language-grammar-grammar descriptor))))
      (unless row (error "ABI descriptor lacks a required result section" name))
      (cdr row))))

;; Opaque rejected source uses the engine token without changing recognition.
(def (abi-terminal-rows descriptor)
  (let (rows (descriptor-section descriptor 'terminals))
    (if (assq 'unknown rows) rows
      (append rows '((unknown token))))))

(def (indexed-symbols symbols)
  (let (table (make-hash-table-eq size: (length symbols)))
    (for-each (lambda (symbol index) (hash-put! table symbol index))
              symbols
              (iota (length symbols)))
    table))

;; A shared field has one stable id. The grammar expression algebra is the
;; authority for fields emitted by the parser; declared syntax fields are
;; retained first for the descriptor's complete public surface.
(def (descriptor-field-symbols grammar)
  (delete-duplicates/hash
   (append
    (apply append (map caddr (descriptor-section grammar 'syntax-kinds)))
    (apply append
           (map (lambda (row) (grammar-expression-fields (cadr row)))
                (descriptor-section grammar 'rules))))
   from-end?: #t))

(def (prepare-language-abi-context id grammar parser)
  (let (field-symbols (descriptor-field-symbols grammar))
    (make-language-abi-context
     id grammar parser
     (indexed-symbols (map car (descriptor-section grammar 'syntax-kinds)))
     (indexed-symbols (map car (abi-terminal-rows grammar)))
     field-symbols
     (indexed-symbols field-symbols) #f abi-source-text)))


;;; Upstream caller storage is stable across Scheme polling and collections.
;;; The buffer is local to this admission; no Scheme body pointer is retained.
(extern allocate-admission-buffer release-admission-buffer admission-buffer-equal?)
(def (abi-source-text bytes)
  (let* ((source (utf8->string bytes)) (capacity (* 4 (string-length source))))
    (unless (fixnum? capacity) (error "native UTF-8 admission capacity exceeds fixnum range"))
    (if (zero? capacity)
      (begin
        (unless (zero? (u8vector-length bytes)) (error "native source is not canonical UTF-8"))
        source)
      (let (buffer (allocate-admission-buffer capacity))
        (unless buffer (error "native UTF-8 admission allocation failed"))
        (dynamic-wind void
          (lambda ()
            (let (written (gerbil-rs-encode-utf8-into source buffer capacity))
              (unless (admission-buffer-equal? buffer bytes written)
                (error "native source is not canonical UTF-8"))
              source))
          (lambda () (release-admission-buffer buffer)))))))
(begin-foreign
 (namespace ("gerbil-parser/src/ffi/language-abi-context#"
             allocate-admission-buffer release-admission-buffer admission-buffer-equal?))
 (c-declare #<<END-C
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
END-C
 )
 (define allocate-admission-buffer
   (c-lambda (unsigned-int64) (pointer void)
     "___return(malloc((size_t)___arg1));"))
 (define release-admission-buffer
   (c-lambda ((pointer void)) void
     "free(___arg1); ___return;"))
 (define admission-buffer-equal?
   (c-lambda ((pointer void) scheme-object unsigned-int64) bool
     "___return(___arg3 == ___HD_BYTES(___HEADER(___arg2)) && (!___arg3 || memcmp(___arg1, ___BODY_AS(___arg2, ___tSUBTYPED), (size_t)___arg3) == 0));")))
