(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; The explicit native catalog admits the compiler without per-case processes.
(import (only-in ./event-strategy-fixture event-lines-language-grammar)
        (only-in ../src/compiler/event-fold-scheme event-fold-scheme-source)
        (only-in ../src/compiler/event-fold-scheme-context current-fold-scheme-diagnostics
                 current-fold-scheme-effect-only current-fold-scheme-expressions)
        (only-in ../src/compiler/event-fold-scheme-statements fold-scheme-statements)
        (only-in ../src/compiler/event-fold-scheme-expressions fold-scheme-offset)
        (only-in ../src/compiler/event-fold-future fold-future-marker-before-boundary?
                 fold-future-named-index fold-future-cache-for current-future-scan-cache)
        (only-in ../src/compiler/event-fold-prefix-family fold-scheme-prefix-family)
        (only-in ../src/compiler/event-fold-scheme-modules event-fold-scheme-module-sources)
        (only-in ../src/runtime/event-fold-lines fold-line-name-set-contains?)
        (only-in ../src/runtime/byte-spans byte-span-skip byte-span-all?)
        (only-in ../src/runtime/source-lines fold-source-lines fold-source-byte-lines
                 fold-source-byte-spans fold-source-frame-boundaries! fold-source-input-bytes))


(def (source initial line finish (helpers '()))
  (event-fold-scheme-source 'native-product event-lines-language-grammar 'Document
                            initial line finish helpers))

(def (saturated-step-reference end position count)
  (if (zero? count) position
    (saturated-step-reference end (min end (+ position 1)) (- count 1))))

;; Check the closed form, including inputs beyond the cap and fixnum range.
(defparser-check (check-saturated-steps)
  (each (count) ('(1 2 3 4 8 16 64))
    (sequential
        ((expression
          (let loop ((remaining count) (value '(state-offset position)))
            (if (zero? remaining) value
              (loop (- remaining 1) (list 'line-step value)))))
         (code (parameterize ((current-fold-scheme-expressions #f))
                 (fold-scheme-offset expression '((position . position)) '()))))
      ;; A lost rewrite can preserve results while restoring repeated work.
      (equal code (list 'min 'end (list '+ count 'position)))
      (scope ((run (eval (list 'lambda '(end position) code))))
        (each (end) ('(0 1 2 8 17 1000000000000000000000000000000000000))
          (each (position)
              ('(0 1 2 3 8 17 18 1000000000000000000000000000000000000))
            (equal (run end position)
                   (saturated-step-reference end position count))))))))

;; The fixture declares each row's class independently of the Fold scanner.
(def (future-suffix-reference rows)
  (and (pair? rows)
       (case (cdar rows)
         ((next) (future-suffix-reference (cdr rows)))
         ((accept) #t)
         (else #f))))

(defparser-check (check-future-suffix-rows rows demand?)
  (sequential
      ((bytes (string->utf8 (apply string-append (map car rows))))
       (cache (if demand? (fold-future-cache-for (gensym))
                (make-hash-table-eqv)))
       (queries
        (let loop ((rest rows) (offset 0) (out '()))
          (if (null? rest) (reverse (cons (cons offset rest) out))
            (loop (cdr rest) (+ offset (u8vector-length (string->utf8 (caar rest))))
                  (cons (cons offset rest) out))))))
    ;; Cold forward, backward and repeated starts exercise publication and hits.
    (each (query) ((append queries (reverse queries) queries))
      (equal (fold-future-marker-before-boundary?
              bytes (car query) "END" "STOP" 42 32 #t #f 58 cache)
             (future-suffix-reference (cdr query)))
      (unless demand?
        ;; Inspect answers published for traversed positions as well as starts.
        (each (position) (queries)
          (scope ((cached (hash-get cache (car position))))
            (when cached
              (equal cached (if (future-suffix-reference (cdr position)) 1 2)))))
        (equal (hash-get cache (u8vector-length bytes)) #f)))
    (unless demand?
      (equal (<= (hash-length cache) (length rows)) #t))))

(defparser-check (check-future-suffix-traces demand?)
  (iterate enumerate ((rows '()) (remaining 3))
    (verify check-future-suffix-rows rows demand?)
    (unless (zero? remaining)
      (each (row) ('((":K: value\n" . next) ("\t:中: 值\r\n" . next)
                     ("END\n" . accept) ("STOP\r\n" . stop) ("ordinary\n" . stop)))
        (continue enumerate (cons row rows) (- remaining 1))))))

;; Distinct in-line starts populate distinct cursor keys in one physical line.
(defparser-check (check-future-inline-starts)
  (scope ((bytes (string->utf8 "ordinary body\n")) (cache (make-hash-table-eqv)))
    (each (offset) ((iota (u8vector-length bytes)))
      (equal (fold-future-marker-before-boundary?
              bytes offset "END" #f 42 32 #f #f #f cache) #f))
    (equal (hash-length cache) (u8vector-length bytes))))

;; The first memo retains its exact start through the EOF probe and promotion.
(defparser-check (check-future-demand-promotion)
  (parameters ((current-future-scan-cache #f))
    (verify check-future-suffix-traces #t)
    (sequential
        ((bytes (string->utf8 "xEND\r\n"))
         (memo (fold-future-cache-for 'stable))
         (query (lambda (offset target)
                  (fold-future-marker-before-boundary?
                   bytes offset target #f 42 32 #f #f #f memo))))
      (equal (query 0 "END") #f)
      (equal (query (u8vector-length bytes) #f) #f)
      ;; An unusable marker detects an unexpected classification after a hit.
      (equal (query 0 #f) #f)
      (equal (query 1 "END") #t)
      (equal (query 0 #f) #f)
      (equal (query 1 #f) #t)))
  (parameters ((current-future-scan-cache #f))
    (equal (fold-future-marker-before-boundary?
            (string->utf8 "END") 0 "END" #f 42 32 #f #f #f
            (fold-future-cache-for 'stable)) #t)))

(deflanguage-parser-tests event-fold-scheme-test "Native EventFold source admission"
  (engine)
    (scenario "native byte frames preserve Unicode CR LF CRLF and terminal bounds"
      (verify check-saturated-steps)
      (raises (fold-source-input-bytes 42) true)
      (each (invalid) ((list (u8vector 255) (u8vector 192 128) (u8vector 237 160 128)
             (u8vector 244 144 128 128) (u8vector 226 130) (u8vector 128)))
        (raises (fold-source-input-bytes invalid) true)
        (raises (fold-source-input-bytes (u8vector-append #u8(65) invalid #u8(90))) true))
      (scope ((ascii (list->u8vector (iota 128))))
        (equal (eq? (fold-source-input-bytes ascii) ascii) #t)
        (equal (utf8->string (fold-source-input-bytes ascii)) (utf8->string ascii)))
      (each (size) ('(7 8 9 15 16 17))
        (sequential ((ascii (make-u8vector size 65))
                     (text (u8vector-append ascii (string->utf8 "é中🦀")))
                     (truncated (u8vector-append ascii #u8(226 130))))
          (equal (eq? (fold-source-input-bytes ascii) ascii) #t)
          (equal (eq? (fold-source-input-bytes text) text) #t)
          (raises (fold-source-input-bytes truncated) true)
          (raises (fold-source-input-bytes (u8vector-append ascii #u8(128 90))) true)))
      (raises (fold-source-byte-lines "not bytes" '() void) true)
      (scope ((bytes (u8vector 99 32 9 97 98)))
        (equal (byte-span-skip bytes 1 5 (lambda (byte) (memv byte '(9 32))))  3)
        (equal (byte-span-skip bytes 3 3 void)  3)
        (equal (byte-span-all? bytes 1 3 (lambda (byte) (memv byte '(9 32))))  #t)
        (equal (byte-span-all? bytes 1 5 (lambda (byte) (memv byte '(9 32))))  #f))
      (each (text) ('("" "abc" "\n" "\r" "\r\n" "é中🦀\r\nx\ry\nz" "a\n\n"))
         (sequential ((bytes (string->utf8 text))
                (before (u8vector->list bytes))
                (oracle (fold-source-lines text '()
                          (lambda (line start end rows)
                            (cons (list line start end) rows))))
                (actual (fold-source-byte-lines bytes '()
                          (parser-check-procedure (parser-check (line view start end rows)
                            (equal (string->utf8 line)  view)
                            (equal (u8vector-length view)  (- end start))
                            (act (cons (list line start end) rows)))))))
           (equal (eq? (fold-source-input-bytes bytes) bytes)  #t)
           (equal (fold-source-input-bytes text)  bytes)
           (equal actual  oracle)
           (equal (fold-source-byte-spans bytes '()
                    (lambda (start end rows)
                      (fold-source-frame-boundaries! bytes start end)
                      (cons (list (utf8->string (subu8vector bytes start end)) start end) rows)))
                   oracle)
           (equal (u8vector->list bytes)  before)))
      (scope ((bytes (string->utf8 "é中🦀")))
        (each (bounds) ('((-1 0) (0 10) (2 0) (1 2) (0 1) (2 3) (5 6)))
                    (raises
                     (fold-source-frame-boundaries! bytes (car bounds) (cadr bounds)) true))
        (each (bounds) ('((0 9) (0 2) (2 5) (5 9) (1 1) (9 9)))
                    (act (fold-source-frame-boundaries! bytes (car bounds) (cadr bounds))))))
    (scenario "runtime marker queries preserve suffix answers and boundary-specific cache bounds"
(locals ((query (lambda ( text target stop indent? heading?)
        (fold-future-marker-before-boundary?
         (string->utf8 text) 0 target stop 42 32 indent? heading? #f
         (make-hash-table-eqv)))))

      (verify check-future-suffix-traces #f)
      (verify check-future-demand-promotion)
      (verify check-future-inline-starts)
      (equal (query "x\n  EnD \r\n" "END" #f #t #f)  #t)
      (equal (query "x\n  EnD \r\n" "END" #f #f #f)  #f)
      (equal (query "stop\nEND\n" "END" "STOP" #f #f)  #f)
      (equal (query "END\n" "END" "END" #f #f)  #f)
      (equal (query "* Heading\nEND\n" "END" #f #f #t)  #f)
      (equal (query "x\n中\n" "中" #f #f #f)  #t)
      (equal (fold-future-marker-before-boundary?
              (string->utf8 "x\n中\n") 0 (string->utf8 "中") #f
              42 32 #f #f #f (make-hash-table-eqv))  #t)
      (equal (fold-future-marker-before-boundary?
              (string->utf8 "STOP\nEND\n") 0 (string->utf8 "END") (string->utf8 "STOP")
              42 32 #f #f #f (make-hash-table-eqv))  #f)
      (equal (query "ENDING\n" "END" #f #f #f)  #f)
      (sequential ((bytes (string->utf8 "  Stop\r\nEND n\nSTOP\n"))
             (index (fold-future-named-index bytes "END " "" "STOP"
                                            42 32 #t #f #t #f #t)))
        (equal (vector->list (vector-ref index 2))  '(14 0)))
      (equal (fold-future-marker-before-boundary?
              (string->utf8 ":KEY: value\nEND\n") 0 "END" #f
              42 32 #f #f 58 (make-hash-table-eqv))  #t)
      (equal (fold-future-marker-before-boundary?
              (string->utf8 "ordinary body\nEND\n") 0 "END" #f
              42 32 #f #f 58 (make-hash-table-eqv))  #f)
      (scope ((bytes (string->utf8 "END\n")) (cache (make-hash-table-eqv)))
        (act (hash-put! cache 0 1))
        ;; Invalid marker values deliberately prove that these paths never
        ;; invoke the encoder, matching the original cached search semantics.
        (equal (fold-future-marker-before-boundary?
                bytes 0 #f #f 42 32 #f #f #f cache)  #t)
        (act (hash-put! cache 0 2))
        (equal (fold-future-marker-before-boundary?
                bytes 0 #f #f 42 32 #f #f #f cache)  #f)
        (equal (fold-future-marker-before-boundary?
                bytes (u8vector-length bytes) #f #f 42 32 #f #f #f cache)  #f))))
    (scenario "generated source is deterministic and has no interpreted request path"
      (each (operator) ('(line-bytes-all-in? line-bytes-any-in?))
         (scope ((generated
               (source '()
                       `((if (,operator start (line-step start) (97 98))
                             ((token Line start end)) ())) '())))
           (equal (not (string-contains generated "(let loop ((cursor from))"))  #t)
           (equal (not (not (string-contains generated "native fold range outside source line")))  #t)
           (equal (not (not (string-contains generated "(u8vector-ref bytes from)")))  #t)))
      (sequential ((family '((line-starts-with-ascii-ci "#+TITLE:")
                       (line-starts-with-ascii-ci "#+AUTHOR:")
                       (line-starts-with-ascii-ci "#+DATE:")
                       (line-starts-with-ascii-ci "#+CAPTION:")))
             (code (fold-scheme-prefix-family family)))
        (equal (not (not code))  #t)
        (equal code  (fold-scheme-prefix-family (reverse family)))
        (equal (fold-scheme-prefix-family (cons '(state ready) family))  #f)
        (equal (fold-scheme-prefix-family (take family 3))  #f)
        (equal (fold-scheme-prefix-family (make-list 33 (car family)))  #f)
        (equal (fold-scheme-prefix-family (make-list 4 (list 'line-starts-with (make-string 129 #\a))))  #f))
      (sequential ((forms '((if (line-bytes-all-in? start end (10 97))
                         ((token Line start end)) ())))
             (first (source '() forms '())))
        (equal (equal? first (source '() forms '()))  #t)
        (equal (not (string-contains first "call-with-fold-cost"))  #t)
        (scope ((observed (parameterize ((current-fold-scheme-diagnostics #t))
                        (source '() forms '()))))
          (equal (not (not (string-contains observed "call-with-fold-cost")))  #t)
          (equal (not (not (string-contains observed "diagnostic Fold cannot bind")))  #t))
        (scope ((counted (parameterize ((current-fold-scheme-diagnostics 'counts))
                       (source '() forms '()))))
          (equal (not (not (string-contains counted "(fold-cost-count!")))  #t)
          (equal (not (string-contains counted "(call-with-fold-cost '"))  #t)
          (equal (not (not (string-contains counted "diagnostic Fold cannot bind")))  #t))
        (equal (source '() forms '())  first)
        (each (forbidden) ('("event-fold-runtime" "fold-statements" "fold-predicate" "run-event-fold" "eval "
                    "current-fold-line-view" "fold-line-bytes" "(string->utf8 line)"))
                    (equal (not (string-contains first forbidden))  #t))
        (equal (not (not (string-contains first "(def (bind-native-product expected-digest)")))  #t)
        (equal (not (not (string-contains first "(reverse! (cons '(finish) events))")))  #t)
        (scope ((units (event-fold-scheme-module-sources
                     'native-product event-lines-language-grammar 'Document
                     '() forms '() '() '() 1)))
          (equal units  (event-fold-scheme-module-sources
                          'native-product event-lines-language-grammar 'Document
                          '() forms '() '() '() 1))
          (each (unit) (units)
             ;; An empty import compiles, but runtime expander evaluation rejects it.
             (equal (not (string-contains (cdr unit) "(import)"))  #t)
             (scope ((definitions
                   (call-with-input-string (cdr unit)
                    (lambda (port)
                      (let loop ((count 0))
                        (let (form (read port))
                          (if (eof-object? form) count
                            (loop (+ count
                                     (if (and (pair? form) (eq? (car form) 'def)
                                              (pair? (cadr form))
                                              (string-prefix? "%event-fold-" (symbol->string (caadr form))))
                                       1 0))))))))))
               (equal (<= definitions 1)  #t))))
        (scope ((names (list->vector
                     (map (lambda (index) (string (integer->char (+ 32 index)))) (iota 95)))))
          (each (name) ((vector->list names))
             (scope ((bytes (string->utf8 (string-append "x" name "y"))))
               (equal (fold-line-name-set-contains? bytes 1 2 names)  #t)))
          (equal (fold-line-name-set-contains? (string->utf8 "") 0 0 names)  #f)
          (equal (fold-line-name-set-contains? (string->utf8 "é") 0 2 names)  #f)
          (equal (fold-line-name-set-contains? (string->utf8 "aa") 0 2 names)  #f))))
    (property "helpers are module-level source-parameterized native procedures"
      (bindings (generated
            (source '() '((call-source-helper slice start end)) '()
                    '((slice () ((token Line start end)))))))
      (equal (not (not (string-contains generated
                       "(def (%event-fold-native-product-helper-0 source-bytes")))  #t)
      (equal (not (string-contains generated "(utf8->string (subu8vector"))  #t)
      (equal (not (string-contains generated "subu8vector"))  #t)
      (equal (not (string-contains generated "utf8->string"))  #t)
      (equal (not (not (string-contains generated "fold-source-frame-boundaries!")))  #t))
    (scenario "in-place helpers preserve the owner while list helpers thread results"
      (each (effect-only?) ('(#f #t))
        (scope ((statements
                 (parameterize ((current-fold-scheme-effect-only effect-only?)
                                (current-fold-scheme-expressions #f))
                   (fold-scheme-statements '((call-source-helper child start end))
                                          '() '() '((child . visit-child))))))
          (equal
           (eval `(let ((source-bytes #u8()) (start 0) (end 0) (events (vector 0)))
                    (let (original events)
                      (let ((visit-child (lambda (_source _start _end owner)
                                           (vector-set! owner 0 1)
                                           'replacement)))
                        ,@statements
                        (list (vector-ref original 0)
                              ,(if effect-only? '(eq? events original) 'events))))))
           (if effect-only? '(1 #t) '(1 replacement))))))
    (scenario "untyped slots unknown instructions and callbacks fail before generation"
      (raises (source '((flag #f)) '((set-uint flag (uint 1))) '()) true)
      (raises (source '() '((foreign-operation)) '()) true)
      (raises (source '() (list void) '()) true)
      (raises (source '() '((call-source-helper missing start end)) '()) true))
    (scenario "cyclic input and dormant helper recursion are rejected"
      (scope ((cycle (list '(token Line start end))))
        (act (set-cdr! cycle cycle))
        (raises (source '() cycle '()) true))
      (raises
       (source '() '() '() '((cycle () ((call-source-helper cycle start end)))))
       true)))
