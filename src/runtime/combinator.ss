;;; -*- Gerbil -*-
;;; Language-neutral recognizer combinators instantiated by hygienic macros.

(import ./recognition
        ./token)
(export parser-empty
        parser-literal
        parser-token-kind
        parser-sequence
        parser-choice
        parser-optional
        parser-repeat
        parser-repeat1
        parser-field
        parser-alias
        parser-run)

(def (matched value rest)
  (cons value rest))

(def (parser-match-children result) (car result))
(def (parser-match-rest result) (cdr result))

(def (prepend-reversed! prefix suffix)
  (if (null? prefix)
    suffix
    (let loop ((rest prefix))
      (if (null? (cdr rest))
        (begin (set-cdr! rest suffix) prefix)
        (loop (cdr rest))))))

(def (reverse! values)
  (let loop ((rest values) (found '()))
    (if (null? rest)
      found
      (let (next (cdr rest))
        (set-cdr! rest found)
        (loop next rest)))))

(def (children-start children)
  (recognition-value-start (recognition-child-value (car children))))

(def (children-end children)
  (let loop ((rest children))
    (if (null? (cdr rest))
      (recognition-value-end (recognition-child-value (car rest)))
      (loop (cdr rest)))))

(def parser-empty
  (lambda (tokens) (matched '() tokens)))

(def (parser-literal literal)
  (lambda (tokens)
    (and (pair? tokens)
         (string=? (token-lexeme (car tokens)) literal)
         (matched (list (make-recognition-child #f (car tokens)))
                  (cdr tokens)))))

(def (parser-token-kind kind)
  (lambda (tokens)
    (and (pair? tokens)
         (eq? (token-kind (car tokens)) kind)
         (matched (list (make-recognition-child #f (car tokens)))
                  (cdr tokens)))))

(def (parser-sequence parsers)
  (lambda (tokens)
    (let loop ((rest-parsers parsers) (rest-tokens tokens) (children '()))
      (if (null? rest-parsers)
        (matched children rest-tokens)
        (let (result ((car rest-parsers) rest-tokens))
          (and result
               (loop (cdr rest-parsers)
                     (parser-match-rest result)
                     (prepend-reversed!
                      (parser-match-children result) children))))))))

(def (parser-choice parsers)
  (lambda (tokens)
    (let loop ((rest parsers))
      (and (pair? rest)
           (or ((car rest) tokens)
               (loop (cdr rest)))))))

(def (parser-optional parser)
  (lambda (tokens)
    (or (parser tokens) (matched '() tokens))))

(def (parser-repeat parser)
  (lambda (tokens)
    (let loop ((rest tokens) (children '()))
      (let (result (parser rest))
        (if (not result)
          (matched children rest)
          (let (next (parser-match-rest result))
            (when (eq? next rest)
              (error "repeated parser accepted empty input"))
            (loop next
                  (prepend-reversed!
                   (parser-match-children result) children))))))))

(def (parser-repeat1 parser)
  (parser-sequence (list parser (parser-repeat parser))))

(def (parser-field name parser)
  (lambda (tokens)
    (let (result (parser tokens))
      (and result
           (let (reversed-children (parser-match-children result))
             (if (null? reversed-children)
               result
               (if (and (null? (cdr reversed-children))
                        (not (recognition-child-field
                              (car reversed-children))))
                 (begin
                   (recognition-child-field-set!
                    (car reversed-children) name)
                   result)
                 (let* ((children (reverse! reversed-children))
                        (start (children-start children))
                        (end (children-end children)))
                   (matched
                    (list
                     (make-recognition-child
                      name
                      (make-recognition-fragment start end children)))
                    (parser-match-rest result))))))))))

(def (parser-alias kind parser)
  (lambda (tokens)
    (let (result (parser tokens))
      (and result
           (let (children (reverse! (parser-match-children result)))
             (and (pair? children)
                  (matched
                   (list
                    (make-recognition-child
                     #f
                     (make-recognition-node
                      kind
                      (children-start children)
                      (children-end children)
                      children)))
                   (parser-match-rest result))))))))

(def (parser-run parser tokens)
  (let (result (parser tokens))
    (unless result (error "input does not match parser entrypoint"))
    (let (children (parser-match-children result))
      (unless (and (pair? children)
                   (null? (cdr children))
                   (not (recognition-child-field (car children))))
        (error "parser entrypoint must produce exactly one unfielded root"))
      (values (recognition-child-value (car children))
              (parser-match-rest result)))))
