;;; Language ABI context and descriptor catalogs shared by ABI publication.
(import (only-in :gerbil-scheme-rust/scheme/utf8 gerbil-rs-encode-utf8-into)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in ../grammar/algebra grammar-expression-fields)
        (only-in ../language/descriptor language-grammar-grammar)
        (only-in ../language/source source-language? source-language-result-catalog))
(export #t)

(defstruct language-abi-context
  (id descriptor parser syntax-kind-index terminal-index field-symbols field-index plan source-admission event-row-writer)
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
     (indexed-symbols field-symbols) #f abi-source-text write-abi-row!)))


;;; Pack a complete GPA1 row in one synchronous bounded leaf. Conversion of
;;; every integer completes before C touches the buffer; no allocation or
;;; callback can move the borrowed Scheme body during the write.
(extern pack-abi-row! allocate-admission-buffer release-admission-buffer admission-buffer-equal?)
(def (write-abi-row! payload offset tag index id start end)
  (unless (and (u8vector? payload) (exact-integer? offset) (>= offset 0))
    (error "invalid ABI row buffer or offset"))
  (unless (pack-abi-row! payload offset tag index id start end)
    (error "ABI row exceeds publication buffer" offset)))
;;; Upstream caller storage is stable across Scheme polling and collections.
;;; The buffer is local to this admission; no Scheme body pointer is retained.
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
             pack-abi-row! allocate-admission-buffer release-admission-buffer admission-buffer-equal?))
 (c-declare #<<END-C
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
static void gp_store_le(uint8_t *p, uint64_t value, unsigned width) {
  for (unsigned i = 0; i < width; ++i) { p[i] = (uint8_t)value; value >>= 8; }
}
END-C
 )
 (define pack-abi-row!
   (c-lambda (scheme-object unsigned-int64 unsigned-int32 unsigned-int32 unsigned-int64 unsigned-int32 unsigned-int32) bool
     "___SCMOBJ ___temp;
      if (!___U8VECTORP(___arg1)) { ___return(0); }
      size_t length = ___HD_BYTES(___HEADER(___arg1));
      if (___arg2 > length || length - (size_t)___arg2 < 24) { ___return(0); }
      uint8_t *p = (uint8_t*)___BODY_AS(___arg1, ___tSUBTYPED) + (size_t)___arg2;
      gp_store_le(p, ___arg3, 4); gp_store_le(p+4, ___arg4, 4);
      gp_store_le(p+8, ___arg5, 8); gp_store_le(p+16, ___arg6, 4);
      gp_store_le(p+20, ___arg7, 4); ___return(1);"))
 (define allocate-admission-buffer
   (c-lambda (unsigned-int64) (pointer void)
     "___return(malloc((size_t)___arg1));"))
 (define release-admission-buffer
   (c-lambda ((pointer void)) void
     "free(___arg1); ___return;"))
 (define admission-buffer-equal?
   (c-lambda ((pointer void) scheme-object unsigned-int64) bool
     "___return(___arg3 == ___HD_BYTES(___HEADER(___arg2)) && (!___arg3 || memcmp(___arg1, ___BODY_AS(___arg2, ___tSUBTYPED), (size_t)___arg3) == 0));")))
