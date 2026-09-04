;;; -*- Gerbil -*-
;;; ISO WG3 BNF language-source adapter and validation boundary.

(import :gerbil-parser/src/runtime/identity)
(export +iso-bnf-source-schema-v1+
        iso-bnf-production?
        iso-bnf-production-name
        iso-bnf-production-expression
        iso-bnf-production-ast
        iso-bnf-production-line
        iso-bnf-production-references
        iso-bnf-source?
        iso-bnf-source-language
        iso-bnf-source-version
        iso-bnf-source-commit
        iso-bnf-source-digest
        iso-bnf-source-productions
        iso-bnf-source-production
        parse-iso-bnf-source
        parse-iso-bnf-source/expected)

(def +iso-bnf-source-schema-v1+ "gerbil-parser.iso-wg3-bnf-source.v1")

(defstruct iso-bnf-production (name expression ast line references)
  transparent: #t)
(defstruct iso-bnf-source
  (schema language version commit digest productions)
  transparent: #t)

(def (whitespace? ch)
  (or (char=? ch #\space) (char=? ch #\tab)
      (char=? ch #\newline) (char=? ch #\return)))

(def (trim text)
  (let (length (string-length text))
    (let left ((start 0))
      (if (and (< start length) (whitespace? (string-ref text start)))
        (left (+ start 1))
        (let right ((end length))
          (if (and (> end start)
                   (whitespace? (string-ref text (- end 1))))
            (right (- end 1))
            (substring text start end)))))))

(def (split-lines source)
  (let (length (string-length source))
    (let loop ((start 0) (offset 0) (lines '()))
      (cond
       ((= offset length)
        (reverse (cons (substring source start offset) lines)))
       ((char=? (string-ref source offset) #\newline)
        (loop (+ offset 1) (+ offset 1)
              (cons (substring source start offset) lines)))
       (else (loop start (+ offset 1) lines))))))

(def (substring-index text wanted (start 0))
  (let ((length (string-length text))
        (wanted-length (string-length wanted)))
    (let loop ((offset start))
      (cond
       ((> (+ offset wanted-length) length) #f)
       ((string=? (substring text offset (+ offset wanted-length)) wanted)
        offset)
       (else (loop (+ offset 1)))))))

(def (production-header line)
  (let* ((text (trim line))
         (separator (substring-index text "::=")))
    (and separator
         (> separator 2)
         (char=? (string-ref text 0) #\<)
         (let (close (substring-index text ">" 1))
           (and close
                (< close separator)
                (cons (substring text 1 close)
                      (trim (substring text (+ separator 3)
                                       (string-length text)))))))))

(def (append-expression current line)
  (let (next (trim line))
    (cond
     ((zero? (string-length next)) current)
     ((zero? (string-length current)) next)
     (else (string-append current " " next)))))

(def (reference-names expression)
  (let* ((annotation (substring-index expression "!!"))
         (source (if annotation (substring expression 0 annotation) expression))
         (length (string-length source)))
    (let loop ((offset 0) (found '()))
      (let (open (substring-index source "<" offset))
        (if (not open)
          (reverse found)
          (let (close (substring-index source ">" (+ open 1)))
            (if (not close)
              (reverse found)
              (let (name (substring source (+ open 1) close))
                (loop (+ close 1)
                      (if (or (zero? (string-length name))
                              (member name found))
                        found
                        (cons name found)))))))))))

(def (bnf-token-kind token)
  (if (pair? token) (car token) token))

(def (bnf-delimiter? ch)
  (or (whitespace? ch)
      (memv ch '(#\[ #\] #\{ #\} #\|))))

(def (tokenize-bnf-expression expression)
  (let (length (string-length expression))
    (let loop ((offset 0) (tokens '()))
      (cond
       ((= offset length) (reverse tokens))
       ((whitespace? (string-ref expression offset))
        (loop (+ offset 1) tokens))
       ((and (<= (+ offset 3) length)
             (string=? (substring expression offset (+ offset 3)) "..."))
        (loop (+ offset 3) (cons 'ellipsis tokens)))
       ((char=? (string-ref expression offset) #\<)
        (let (close (substring-index expression ">" (+ offset 1)))
          (if (and close (> close (+ offset 1)))
            (loop (+ close 1)
                  (cons (cons 'reference
                              (substring expression (+ offset 1) close))
                        tokens))
            (loop (+ offset 1) (cons (cons 'terminal "<") tokens)))))
       ((char=? (string-ref expression offset) #\[)
        (loop (+ offset 1) (cons 'open-square tokens)))
       ((char=? (string-ref expression offset) #\])
        (loop (+ offset 1) (cons 'close-square tokens)))
       ((char=? (string-ref expression offset) #\{)
        (loop (+ offset 1) (cons 'open-brace tokens)))
       ((char=? (string-ref expression offset) #\})
        (loop (+ offset 1) (cons 'close-brace tokens)))
       ((char=? (string-ref expression offset) #\|)
        (loop (+ offset 1) (cons 'bar tokens)))
       (else
        (let end ((next (+ offset 1)))
          (if (or (= next length)
                  (bnf-delimiter? (string-ref expression next))
                  (and (<= (+ next 3) length)
                       (string=? (substring expression next (+ next 3)) "...")))
            (loop next
                  (cons (cons 'terminal (substring expression offset next))
                        tokens))
            (end (+ next 1)))))))))

(def (finish-sequence reversed)
  (let (items (reverse reversed))
    (cond
     ((null? items) '(empty))
     ((null? (cdr items)) (car items))
     (else (cons 'sequence items)))))

(def (finish-choice reversed)
  (let (items (reverse reversed))
    (if (null? (cdr items)) (car items) (cons 'choice items))))

(def (parse-bnf-atom tokens)
  (unless (pair? tokens)
    (error "ISO BNF expression expected an atom"))
  (let (token (car tokens))
    (case (bnf-token-kind token)
      ((reference terminal)
       (values (list (bnf-token-kind token) (cdr token)) (cdr tokens)))
      ((open-square)
       (let-values (((expression rest)
                     (parse-bnf-choice (cdr tokens) 'close-square)))
         (unless (and (pair? rest) (eq? (car rest) 'close-square))
           (error "unclosed ISO BNF optional expression"))
         (values (list 'optional expression) (cdr rest))))
      ((open-brace)
       (let-values (((expression rest)
                     (parse-bnf-choice (cdr tokens) 'close-brace)))
         (unless (and (pair? rest) (eq? (car rest) 'close-brace))
           (error "unclosed ISO BNF grouped expression"))
         (values (list 'group expression) (cdr rest))))
      (else (error "unexpected ISO BNF expression token" token)))))

(def (parse-bnf-sequence tokens stop)
  (let loop ((rest tokens) (items '()))
    (if (or (null? rest)
            (eq? (car rest) 'bar)
            (and stop (eq? (car rest) stop)))
      (values (finish-sequence items) rest)
      (let-values (((atom next) (parse-bnf-atom rest)))
        (if (and (pair? next) (eq? (car next) 'ellipsis))
          (loop (cdr next) (cons (list 'repeat1 atom) items))
          (loop next (cons atom items)))))))

(def (parse-bnf-choice tokens stop)
  (let loop ((rest tokens) (alternatives '()))
    (let-values (((sequence next) (parse-bnf-sequence rest stop)))
      (if (and (pair? next) (eq? (car next) 'bar))
        (loop (cdr next) (cons sequence alternatives))
        (values (finish-choice (cons sequence alternatives)) next)))))

(def (parse-bnf-expression expression)
  (let (annotation (substring-index expression "!!"))
    (if annotation
      (let ((terminal (trim (substring expression 0 annotation)))
            (codepoints
             (trim (substring expression (+ annotation 2)
                              (string-length expression)))))
        (when (zero? (string-length terminal))
          (error "ISO BNF annotated terminal has no spelling" expression))
        (list 'annotated-terminal terminal codepoints))
      (let-values (((ast rest)
                    (parse-bnf-choice (tokenize-bnf-expression expression) #f)))
        (unless (null? rest)
          (error "unexpected trailing ISO BNF expression tokens" rest))
        ast))))

(def (parse-production-rows source)
  (let flush ((lines (split-lines source))
              (line-number 1)
              (name #f)
              (start-line #f)
              (expression "")
              (productions '()))
    (if (null? lines)
      (reverse
       (if name
         (cons (make-iso-bnf-production
                name expression (parse-bnf-expression expression)
                start-line (reference-names expression))
               productions)
         productions))
      (let* ((line (car lines))
             (header (production-header line)))
        (if header
          (flush
           (cdr lines) (+ line-number 1)
           (car header) line-number (cdr header)
           (if name
             (cons (make-iso-bnf-production
                    name expression (parse-bnf-expression expression)
                    start-line (reference-names expression))
                   productions)
             productions))
          (flush
           (cdr lines) (+ line-number 1) name start-line
           (if (and name
                    (or (zero? (string-length (trim line)))
                        (not (char=? (string-ref (trim line) 0) #\#))))
             (append-expression expression line)
             expression)
           productions))))))

(def (validate-productions productions)
  (let (names (map iso-bnf-production-name productions))
    (for-each
     (lambda (production)
       (let (name (iso-bnf-production-name production))
         (when (> (length (filter (cut string=? name <>) names)) 1)
           (error "duplicate ISO BNF production" name))
         (for-each
          (lambda (reference)
            (unless (member reference names)
              (error "unresolved ISO BNF production reference"
                     name reference)))
          (iso-bnf-production-references production))))
     productions)
    productions))

(def (parse-iso-bnf-source language version commit source)
  (unless (and (string? language) (string? version) (string? commit)
               (string? source))
    (error "ISO BNF source identity and content must be strings"))
  (let (productions (validate-productions (parse-production-rows source)))
    (when (null? productions)
      (error "ISO BNF source contains no productions"))
    (make-iso-bnf-source
     +iso-bnf-source-schema-v1+ language version commit
     (sha256-text source) productions)))

(def (parse-iso-bnf-source/expected language version commit expected-digest source)
  (let (catalog (parse-iso-bnf-source language version commit source))
    (unless (string=? (iso-bnf-source-digest catalog) expected-digest)
      (error "ISO BNF source digest mismatch"
             expected-digest (iso-bnf-source-digest catalog)))
    catalog))

(def (iso-bnf-source-production source name)
  (find (lambda (production)
          (string=? (iso-bnf-production-name production) name))
        (iso-bnf-source-productions source)))
