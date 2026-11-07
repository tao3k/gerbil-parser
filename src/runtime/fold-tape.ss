;;; Request-owned direct OEV1 consumer; no intermediate event descriptors.
(import :std/ffi)
(export make-fold-tape fold-tape-start! fold-tape-token! fold-tape-finish!
        fold-tape-publish!)

;; Fixed OEV1 records are one synchronous foreign leaf operation. The owner
;; reserves the complete record first; no object/body pointer escapes the
;; leaf, which neither allocates Scheme storage nor calls back into Scheme.
;; Byte stores are portable at every alignment and on either host byte order.
(C-ffi-macrology)
(C-include "<stdint.h>")
(C-declare "static void fold_wire_le64(uint8_t *p, uint64_t value) {
  for (unsigned int i = 0; i < 8; ++i) p[i] = (uint8_t)(value >> (8 * i));
}")
(def-C-lambda write-start-record! (scheme-object unsigned-int64 unsigned-int16) void
  "uint8_t *p = U8_DATA(___arg1) + ___arg2;
   p[0] = 1; p[1] = (uint8_t)___arg3; p[2] = (uint8_t)(___arg3 >> 8);
   ___return;")
(def-C-lambda write-token-record!
  (scheme-object unsigned-int64 unsigned-int16 unsigned-int64 unsigned-int64) void
  "uint8_t *p = U8_DATA(___arg1) + ___arg2;
   p[0] = 2; p[1] = (uint8_t)___arg3; p[2] = (uint8_t)(___arg3 >> 8);
   fold_wire_le64(p + 3, ___arg4); fold_wire_le64(p + 11, ___arg5);
   ___return;")

(defstruct fold-tape-owner (bytes used sealed?))
(def (make-fold-tape header)
  (unless (u8vector? header) (error "invalid Fold tape header"))
  (let* ((size (u8vector-length header)) (bytes (make-u8vector (max 64 size) 0)))
    (u8vector-copy! bytes 0 header)
    (make-fold-tape-owner bytes size #f)))

(def (reserve! tape width)
  (when (fold-tape-owner-sealed? tape) (error "published Fold tape"))
  (let* ((offset (fold-tape-owner-used tape)) (end (+ offset width))
         (bytes (fold-tape-owner-bytes tape)) (size (u8vector-length bytes)))
    (when (> end size)
      (let (grown (make-u8vector (max end (* 2 size)) 0))
        (u8vector-copy! grown 0 bytes 0 offset)
        (set! (fold-tape-owner-bytes tape) grown)))
    (set! (fold-tape-owner-used tape) end)
    offset))
(def (unsigned? value width)
  (and (exact-integer? value) (<= 0 value)
       (< value (if (= width 2) 65536 18446744073709551616))))
(def (fold-tape-start! tape kind)
  (unless (unsigned? kind 2) (error "invalid Fold tape kind" kind))
  (let* ((offset (reserve! tape 3)) (bytes (fold-tape-owner-bytes tape)))
    (write-start-record! bytes offset kind))
  tape)
(def (fold-tape-token! tape kind from until)
  ;; Backward spans are part of the canonical Fold contract; retain them.
  (unless (and (unsigned? kind 2) (unsigned? from 8) (unsigned? until 8))
    (error "invalid Fold tape token" kind from until))
  (let* ((offset (reserve! tape 19)) (bytes (fold-tape-owner-bytes tape)))
    (write-token-record! bytes offset kind from until))
  tape)
(def (fold-tape-finish! tape)
  (let ((offset (reserve! tape 1)))
    (u8vector-set! (fold-tape-owner-bytes tape) offset 0))
  tape)
(def (fold-tape-publish! tape)
  (when (fold-tape-owner-sealed? tape) (error "published Fold tape"))
  (set! (fold-tape-owner-sealed? tape) #t)
  (let ((bytes (fold-tape-owner-bytes tape)) (used (fold-tape-owner-used tape)))
    ;; Same ownership transfer as std/io/bio get-memory-output-u8vector:
    ;; bytes were allocated privately; no alias escaped before publication.
    ;; Sealing and clearing the owner prevent future writers retaining them.
    (set! (fold-tape-owner-bytes tape) #f)
    (when (< used (u8vector-length bytes)) (u8vector-shrink! bytes used))
    bytes))
