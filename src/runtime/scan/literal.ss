;;; -*- Gerbil -*-
;;; Scan literal owner: original pure lexical algorithms.
(import (only-in "./basic.ss" literal-at?))
(export literal-trie-terminal literal-trie-child keep-first-literal collect-literal make-literal-trie checked-lexer-literal make-bounded-literal-end-scanner make-literal-end-scanner ranked-literal-before? prepare-ranked-literal-terminals! ranked-literal-root-ascii prepare-ranked-literal-root make-ranked-literal-scanner/prepared make-ranked-literal-scanner ranked-core-literal ranked-core-winner scan-longest-literal)

;;; Compile a static literal catalog into a character trie once. The returned
;;; scanner follows at most the matching source prefix, independent of catalog
;;; size, and retains the longest terminal seen along that path.
;; : (-> (List String) (-> String Nat (Maybe Nat)))
(defrule (literal-trie-terminal node) (vector-ref node 0))

(defrule (literal-trie-child node-expr character-expr)
  (let* ((edges (vector-ref node-expr 1)) (character character-expr))
    (cond ((pair? edges) (and (eqv? character (car edges)) (cdr edges)))
          ((not edges) #f)
          (else (table-ref edges character #f)))))

(def (keep-first-literal previous payload) (or previous payload))

(def (collect-literal previous payload) (cons payload previous))

;;; Common prepared literal IR: accepting payload and character-indexed edges.
;;; Combining duplicate payloads is preparation work, never scanner dispatch.
(def (make-literal-trie entries (initial #f) (combine keep-first-literal))
  (def (make-node) (vector initial (make-table test: eqv?)))
  (let (root (make-node))
    (for-each
     (lambda (entry)
       (let (literal (car entry))
         (unless (string? literal) (error "literal trie requires strings" literal))
         (let insert ((node root) (index 0))
           (if (= index (string-length literal))
             (vector-set! node 0 (combine (literal-trie-terminal node) (cdr entry)))
             (let* ((children (vector-ref node 1))
                    (character (string-ref literal index))
                    (child (table-ref children character #f)))
               (unless child
                 (set! child (make-node))
                 (table-set! children character child))
               (insert child (+ index 1))))))) entries)
    ;; Preparation owns mutation. Freeze empty and singleton edge sets into
    ;; direct representations; branching nodes retain their character index.
    ;; An explicit work list avoids growing the stack with literal depth.
    (let freeze ((pending (list root)))
      (unless (null? pending)
        (let* ((node (car pending)) (children (vector-ref node 1))
               (edges (table->list children)) (rest (cdr pending)))
          (for-each (lambda (edge) (set! rest (cons (cdr edge) rest))) edges)
          (cond ((null? edges) (vector-set! node 1 #f))
                ((null? (cdr edges)) (vector-set! node 1 (car edges))))
          (freeze rest))))
    root))

(def (checked-lexer-literal literal)
  (unless (and (string? literal) (positive? (string-length literal)))
    (error "lexer literals must be non-empty strings" literal))
  literal)

(def (make-bounded-literal-end-scanner literals)
  (let (root (make-literal-trie
              (map (lambda (literal) (cons (checked-lexer-literal literal) #t)) literals)))
    (lambda (source start source-length)
      (and (exact-integer? start) (exact-integer? source-length)
           (<= 0 start source-length (string-length source))
        (let loop ((node root) (offset start) (selected #f))
          (if (= offset source-length)
            selected
            (let (child (literal-trie-child node (string-ref source offset)))
              (if child
                (let (next (+ offset 1))
                  (loop child next (if (literal-trie-terminal child) next selected)))
                selected))))))))

(def (make-literal-end-scanner literals)
  (let (scan (make-bounded-literal-end-scanner literals))
    (lambda (source start) (scan source start (string-length source)))))

;;; Merge literal-only lexical rules into one trie shared by every LR mode.
;;; Entries carry (literal name precedence declaration-index); the optional
;;; admitted vector filters complete rules without copying the trie per mode.
(def (ranked-literal-before? left right)
  (or (> (cadr left) (cadr right))
      (and (= (cadr left) (cadr right)) (< (caddr left) (caddr right)))))

;;; Order terminal catalogs once; mode filtering then stops at its first winner.
(def (prepare-ranked-literal-terminals! root)
  (let visit ((pending (list root)))
    (unless (null? pending)
      (let* ((node (car pending)) (edges (vector-ref node 1))
             (children (cond ((not edges) '())
                             ((pair? edges) (list (cdr edges)))
                             (else (map cdr (table->list edges))))))
        (vector-set! node 0 (list-sort ranked-literal-before? (vector-ref node 0)))
        (visit (append children (cdr pending))))))
  root)

;;; Dense ASCII fanout gets a request-independent root directory. Sparse and
;;; non-ASCII transitions retain the compact trie; the directory shares nodes.
(def (ranked-literal-root-ascii root)
  (let (edges (vector-ref root 1))
    (and edges (not (pair? edges))
         (let* ((entries (table->list edges))
                (ascii (filter (lambda (entry) (< (char->integer (car entry)) 128)) entries)))
           (and (>= (length ascii) 8)
                (let (directory (make-vector 128 #f))
                  (for-each (lambda (entry)
                              (vector-set! directory (char->integer (car entry)) (cdr entry))) ascii)
                  directory))))))

(def (prepare-ranked-literal-root entries)
  (prepare-ranked-literal-terminals!
              (make-literal-trie
               (map (lambda (entry)
                      (cons (checked-lexer-literal (car entry))
                            (list (cadr entry) (caddr entry) (cadddr entry)
                                  (string-length (car entry))))) entries)
               '() collect-literal)))

(def (make-ranked-literal-scanner/prepared root (ascii (ranked-literal-root-ascii root)))
  (def (best-admitted candidates admitted)
    (if admitted
      (let loop ((remaining candidates))
        (and (pair? remaining)
             (if (vector-ref admitted (caddr (car remaining)))
               (car remaining) (loop (cdr remaining)))))
      (and (pair? candidates) (car candidates))))
  (def (publish terminal start)
    (and terminal (list (car terminal) (+ start (cadddr terminal))
                        (cadr terminal) (caddr terminal))))
  (lambda (source start (admitted #f))
        (let (source-length (string-length source))
          (and (not (= start source-length))
               (let* ((ch (string-ref source start))
                      (child (if ascii
                               (let (code (char->integer ch))
                                 (if (< code 128) (vector-ref ascii code)
                                     (literal-trie-child root ch)))
                               (literal-trie-child root ch))))
                 (and child
                      (let scan ((node child) (offset (+ start 1))
                                 (selected (best-admitted (vector-ref child 0) admitted)))
                        (if (= offset source-length)
                          (publish selected start)
                          (let (child (literal-trie-child node (string-ref source offset)))
                            (if child
                              (let* ((candidates (vector-ref child 0))
                                     (terminal (and (pair? candidates) (best-admitted candidates admitted))))
                                (scan child (+ offset 1) (or terminal selected)))
                              (publish selected start)))))))))))

(def (make-ranked-literal-scanner entries)
  (make-ranked-literal-scanner/prepared (prepare-ranked-literal-root entries)))

;;; Product of the literal trie and the closed monotone run DFA. Rule masks
;;; are frozen by mode preparation; source scanning carries only automaton state
;;; and the last accepting winner. Numeric pending/rollback states retain their
;;; existing complete scanner rather than treating them as monotone runs.
(defrule (ranked-core-literal node admitted)
  (and node
       (let find ((candidates (vector-ref node 0)))
         (and (pair? candidates)
              (if (or (not admitted) (vector-ref admitted (caddr (car candidates))))
                (car candidates) (find (cdr candidates)))))))

(defrule (ranked-core-winner node active admitted winners)
  (let ((literal (ranked-core-literal node admitted)) (run (vector-ref winners active)))
    (if (and run
             (or (not literal) (> (cadr run) (cadr literal))
                 (and (= (cadr run) (cadr literal)) (< (caddr run) (caddr literal))))) run literal)))

;; scan-longest-literal
;;   : (-> String Fixnum List String)
;;   | doc m%
;;       `scan-longest-literal` selects the longest matching declared literal.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-longest-literal "<=" 0 '("<" "<="))
;;       ;; => "<="
;;       ```
;;     %
(def (scan-longest-literal source start literals)
  (foldl (lambda (candidate selected)
           (if (and (literal-at? source start candidate)
                    (or (not selected)
                        (> (string-length candidate)
                           (string-length selected))))
             candidate
             selected))
         #f
         literals))

