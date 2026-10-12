;;; Language ABI context and descriptor catalogs shared by ABI publication.
;;; Contains bounded foreign byte leaves; package builds must use the
;;; explicit FFI module policy, including Darwin loadable-bundle linker flags.
(import (only-in :std/list/list delete-duplicates/hash)
        (only-in :std/string/misc string-every)
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
(extern copy-ascii-source-range! ascii-source-range? pack-abi-row!)
(def (write-abi-row! payload offset tag index id start end)
  (declare (not interrupts-enabled))
  ;; Match the upstream encoder's inline checked leaf: ordinary row scalars
  ;; remain tagged fixnums instead of entering seven generic FFI converters.
  ;; The general integer leaf retains the full unsigned 64-bit ABI domain.
  (unless (or (##c-code #<<C-END
___RESULT = gp_pack_fixnum_row(___ARG1, ___ARG2, ___ARG3, ___ARG4,
                              ___ARG5, ___ARG6, ___ARG7) ? ___TRU : ___FAL;
C-END
                payload offset tag index id start end)
              (pack-abi-row! payload offset tag index id start end))
    (error "ABI row exceeds publication buffer" offset)))
;;; Bounded ASCII leaves return to Scheme between chunks. Upstream caller
;;; storage is stable across polls; no Scheme body pointer survives a leaf.
(def (ascii-source-bytes? bytes)
  (and (u8vector? bytes)
    (let (length (u8vector-length bytes))
      (let loop ((start 0))
        (or (= start length)
          (let (end (min length (+ start 4096)))
            (and (ascii-source-range? bytes start end) (loop end))))))))
(def (copy-ascii-source! bytes source)
  (and (u8vector? bytes) (string? source)
       (= (u8vector-length bytes) (string-length source))
    (let (length (u8vector-length bytes))
      (let loop ((start 0))
        (or (= start length)
          (let (end (min length (+ start 4096)))
            (and (copy-ascii-source-range! bytes source start end) (loop end))))))))
(def (abi-source-text bytes)
  (unless (u8vector? bytes) (error "native source requires byte storage"))
  ;; Every 7-bit byte is its unique UTF-8 scalar encoding, including NUL.
  ;; Decode the proven subset directly into owned Scheme string storage.
  ;; Other input uses SDK decoding and canonical Unicode scalar admission.
  (if (ascii-source-bytes? bytes)
    (let (source (make-string (u8vector-length bytes) #\nul))
      (unless (copy-ascii-source! bytes source)
        (error "ASCII source changed during admission"))
      source)
    (let (source (utf8->string bytes))
      ;; Gambit characters may extend beyond Unicode scalars. Re-encoding alone
      ;; cannot reject surrogate encodings accepted by a permissive decoder.
      (unless (and (string-every unicode-scalar? source)
                   (equal? (string->utf8 source) bytes))
        (error "native source is not canonical UTF-8"))
      source)))
(def (unicode-scalar? character)
  (let (scalar (char->integer character))
    (and (<= 0 scalar #x10ffff)
         (not (<= #xd800 scalar #xdfff)))))
(begin-foreign
 (namespace ("gerbil-parser/src/ffi/language-abi-context#"
             copy-ascii-source-range! ascii-source-range? pack-abi-row!))
 (c-declare #<<END-C
#include <stdint.h>
static void gp_store_le(uint8_t *p, uint64_t value, unsigned width) {
  for (unsigned i = 0; i < width; ++i) { p[i] = (uint8_t)value; value >>= 8; }
}
/* All type, width and full-span checks precede the first borrowed write. */
static int gp_pack_fixnum_row(___SCMOBJ bytes, ___SCMOBJ position,
                             ___SCMOBJ tag, ___SCMOBJ index, ___SCMOBJ id,
                             ___SCMOBJ start, ___SCMOBJ end) {
  ___SCMOBJ ___temp;
  if (!___U8VECTORP(bytes) || !___FIXNUMP(position) || !___FIXNUMP(tag) ||
      !___FIXNUMP(index) || !___FIXNUMP(id) || !___FIXNUMP(start) || !___FIXNUMP(end))
    return 0;
  if (___INT(position) < 0 || ___INT(tag) < 0 || ___INT(index) < 0 ||
      ___INT(id) < 0 || ___INT(start) < 0 || ___INT(end) < 0)
    return 0;
  uint64_t offset = (uint64_t)___INT(position);
  uint64_t number = (uint64_t)___INT(tag), symbol = (uint64_t)___INT(index);
  uint64_t first = (uint64_t)___INT(start), last = (uint64_t)___INT(end);
  size_t length = ___HD_BYTES(___HEADER(bytes));
  if (number > UINT32_MAX || symbol > UINT32_MAX || first > UINT32_MAX ||
      last > UINT32_MAX || offset > length || length - (size_t)offset < 24)
    return 0;
  uint8_t *p = (uint8_t*)___BODY_AS(bytes, ___tSUBTYPED) + (size_t)offset;
  gp_store_le(p, number, 4); gp_store_le(p+4, symbol, 4);
  gp_store_le(p+8, (uint64_t)___INT(id), 8);
  gp_store_le(p+16, first, 4); gp_store_le(p+20, last, 4);
  return 1;
}
END-C
 )
 (define copy-ascii-source-range!
   (c-lambda (scheme-object scheme-object unsigned-int64 unsigned-int64) bool
     "___SCMOBJ ___temp;
      if (!___U8VECTORP(___arg1) || !___STRINGP(___arg2)) { ___return(0); }
      size_t length = ___HD_BYTES(___HEADER(___arg1));
      if ((size_t)___STRINGSIZE(___arg2) != length || ___arg3 > ___arg4 || ___arg4 > length || ___arg4 - ___arg3 > 4096) { ___return(0); }
      const uint8_t *p = (const uint8_t*)___BODY_AS(___arg1, ___tSUBTYPED);
      for (size_t i = (size_t)___arg3; i < (size_t)___arg4; ++i) {
        uint8_t scalar = p[i]; if (scalar & 0x80) { ___return(0); }
        ___STRINGSET(___arg2, ___FIX(i), ___CHR(scalar));
      }
      ___return(1);"))
 (define ascii-source-range?
   (c-lambda (scheme-object unsigned-int64 unsigned-int64) bool
     "___SCMOBJ ___temp;
      if (!___U8VECTORP(___arg1)) { ___return(0); }
      size_t length = ___HD_BYTES(___HEADER(___arg1));
      if (___arg2 > ___arg3 || ___arg3 > length || ___arg3 - ___arg2 > 4096) { ___return(0); }
      const uint8_t *p = (const uint8_t*)___BODY_AS(___arg1, ___tSUBTYPED);
      for (size_t i = (size_t)___arg2; i < (size_t)___arg3; ++i) { if (p[i] & 0x80) { ___return(0); } }
      ___return(1);"))
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
)
