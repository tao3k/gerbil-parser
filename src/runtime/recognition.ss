;;; -*- Gerbil -*-
;;; Private recognition values produced by generated parser machines.

(import (only-in ./token make-token token? token-lexeme token-end token-start)
        (only-in ./event-program event-program-value? event-program-value-start event-program-value-end))
(export prepare-recognition-source recognition-source? recognition-source-text
        recognition-source-offset recognition-source-token recognition-source-node
        relocate-recognition-value
        recognition-relocation? recognition-relocation-value recognition-relocation-delta
        make-recognition-node
        recognition-node?
        recognition-node-kind
        recognition-node-start
        recognition-node-end
        recognition-node-children
        make-recognition-fragment
        recognition-fragment?
        recognition-fragment-start
        recognition-fragment-end
        recognition-fragment-children
        make-recognition-child
        recognition-child?
        recognition-child-field
        recognition-child-field-set!
        recognition-child-value
        recognition-value-start
        recognition-value-end)

(defstruct recognition-node (kind start end children) transparent: #t)
(defstruct recognition-fragment (start end children) transparent: #t)
(defstruct recognition-child (field value) transparent: #t)


;;; Position views share the semantic subtree. Collapse repeated moves of the
;;; same view so successive edits do not retain a chain of source snapshots.
(defstruct recognition-relocation (value delta) transparent: #t)
(def (relocate-recognition-value value delta (force? #f))
  (cond
   ((recognition-relocation? value)
    (make-recognition-relocation (recognition-relocation-value value)
                                (+ delta (recognition-relocation-delta value))))
   ((and (zero? delta) (not force?)) value)
   (else (make-recognition-relocation value delta))))

;; : (-> RecognitionValue Nat)
(def (recognition-value-start value)
  (cond
   ((recognition-relocation? value)
    (+ (recognition-relocation-delta value)
       (recognition-value-start (recognition-relocation-value value))))
   ((token? value) (token-start value))
   ((recognition-node? value) (recognition-node-start value))
   ((recognition-fragment? value) (recognition-fragment-start value))
   ((event-program-value? value) (event-program-value-start value))
   (else (error "invalid recognition value" value))))

;; : (-> RecognitionValue Nat)
(def (recognition-value-end value)
  (cond
   ((recognition-relocation? value)
    (+ (recognition-relocation-delta value)
       (recognition-value-end (recognition-relocation-value value))))
   ((token? value) (token-end value))
   ((recognition-node? value) (recognition-node-end value))
   ((recognition-fragment? value) (recognition-fragment-end value))
   ((event-program-value? value) (event-program-value-end value))
   (else (error "invalid recognition value" value))))

;;; Source-backed results share one UTF-8 boundary index. Preparing a source is
;;; linear in characters; every node/token endpoint is then a constant-time
;;; lookup, including nonmonotonic recursive publication. Own the text so a
;;; caller cannot invalidate admitted boundaries by mutating the original token.
(defstruct recognition-source (owned-text origin offsets))

(def (prepare-recognition-source token)
  (unless (and (token? token) (exact-integer? (token-start token))
               (exact-integer? (token-end token)))
    (error "invalid recognition source token" token))
  (let* ((text (string-copy (token-lexeme token)))
         (length (string-length text)) (origin (token-start token)))
    ;; ASCII uses origin + character and needs no vector. Allocate a boundary
    ;; index only on the first multibyte scalar, then backfill the ASCII prefix.
    (let loop ((at 0) (byte origin) (offsets #f))
      (when offsets (vector-set! offsets at byte))
      (if (= at length)
        (begin
          (unless (= byte (token-end token))
            (error "recognition source byte span disagrees with text" token))
          (make-recognition-source text origin offsets))
        (let* ((code (char->integer (string-ref text at)))
               (index
                (if (or offsets (< code #x80)) offsets
                  (let (index (make-vector (+ length 1)))
                    (let prefix ((before 0))
                      (when (<= before at)
                        (vector-set! index before (+ origin before))
                        (prefix (+ before 1))))
                    index))))
          (unless (and (<= 0 code #x10ffff) (not (<= #xd800 code #xdfff)))
            (error "invalid Unicode scalar in recognition source" at))
          (loop (+ at 1)
                (+ byte (cond ((< code #x80) 1) ((< code #x800) 2)
                              ((< code #x10000) 3) (else 4))) index))))))

(def (recognition-source-text source)
  (string-copy (recognition-source-owned-text source)))

(def (recognition-source-offset source character)
  (unless (and (exact-integer? character) (<= 0 character)
               (<= character (string-length (recognition-source-owned-text source))))
    (error "recognition boundary outside admitted source" character))
  (let (index (recognition-source-offsets source))
    (if index (vector-ref index character)
      (+ (recognition-source-origin source) character))))

(def (recognition-source-token source kind start end)
  (unless (<= start end) (error "reversed recognition token bounds" start end))
  (let ((begin (recognition-source-offset source start))
        (finish (recognition-source-offset source end)))
    (make-token kind (substring (recognition-source-owned-text source) start end) begin finish)))

(def (recognition-source-node source kind start end children)
  (unless (<= start end) (error "reversed recognition node bounds" start end))
  (make-recognition-node kind (recognition-source-offset source start)
                         (recognition-source-offset source end) children))
