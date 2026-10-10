;;; -*- Gerbil -*-
;;; Stage homogeneous pure prefix alternatives into shared byte decisions.
;;; This is a private lowering of existing admitted IR, not a new author DSL.
(export fold-scheme-prefix-family)

(def (prefix-alternative? opcode form)
  (and (list? form) (= (length form) 2) (eq? (car form) opcode)
       (string? (cadr form)) (positive? (string-length (cadr form)))))

(def (lower-byte byte)
  (if (<= 65 byte 90) (+ byte 32) byte))

;; Build-local lists are pure. Shorter terminals absorb longer alternatives:
;; all members are total prefix predicates, with no state reads or effects.
(def (prefix-tree paths)
  (if (any null? paths)
    #t
    (let (groups
          (foldl (lambda (path groups)
                   (let (byte (car path))
                     (cons (cons byte (cons (cdr path) (cond ((assv byte groups) => cdr) (else '()))))
                           (filter (lambda (entry) (not (= byte (car entry)))) groups))))
                 '() paths))
      (map (lambda (entry) (cons (car entry) (prefix-tree (cdr entry))))
           (list-sort (lambda (left right) (< (car left) (car right))) groups)))))

(def (prefix-tree-code tree cursor ascii-ci?)
  (if (eq? tree #t)
    #t
    `(and (< ,cursor end)
          (let (byte (u8vector-ref source-bytes ,cursor))
            (case ,(if ascii-ci? '(ascii-lower-byte byte) 'byte)
              ,@(map (lambda (entry)
                       `((,(car entry))
                         (let (next (+ ,cursor 1))
                           ,(prefix-tree-code (cdr entry) 'next ascii-ci?)))) tree)
              (else #f))))))

(def (fold-scheme-prefix-family alternatives)
  (and (<= 4 (length alternatives) 32)
       (let* ((opcode (and (pair? (car alternatives)) (caar alternatives)))
              (ascii-ci? (eq? opcode 'line-starts-with-ascii-ci)))
         (and (memq opcode '(line-starts-with line-starts-with-ascii-ci))
              (every (cut prefix-alternative? opcode <>) alternatives)
              ;; Keep native expansion bounded. Other forms retain ordinary
              ;; Boolean lowering; this is not a second runtime implementation.
              (let (paths (map (lambda (form)
                                (let (bytes (u8vector->list (string->utf8 (cadr form))))
                                  (if ascii-ci? (map lower-byte bytes) bytes))) alternatives))
                (and (<= (foldl (lambda (path size) (+ size (length path))) 0 paths) 512)
                     (prefix-tree-code (prefix-tree paths) 'start ascii-ci?)))))))
