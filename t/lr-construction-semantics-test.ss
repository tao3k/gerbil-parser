;;; Whole-producer controls complement the universal construction proofs.
(import :gerbil-parser/language-test-support
        :gerbil-parser/t/fixtures/lr-construction-semantics
        (only-in "fixtures/construction-lifecycle.ss" construction-observation)
        (only-in "fixtures/construction-storage.ss" joint-construction-observation)
        (only-in "fixtures/construction-control.ss" growth-control-observation)
        (only-in "fixtures/construction-append.ss" append-control-observation)
        (only-in "fixtures/construction-fifo.ss" fifo-source-observation existing-zero-observation)
        (only-in "fixtures/bit-traversal.ss" traversal-observation traversal-reference traversal-masks))

(deflanguage-parser-tests lr-construction-semantics-test "LR construction protocols"
  (engine)
  (property "native prediction queues converge through cycles and retain duplicate rule IDs"
    (bindings (observed (native-predictions
         '((0 start ((nonterminal A)) concat #f)
           (1 A ((nonterminal B)) concat #f)
           (2 B ((nonterminal A)) concat #f)
           (3 B () concat #f)
           (4 B () concat #f)))))
    (equal (car observed) '(30 0 30 0 30 0 0 0))
      (equal (cadr observed) '(1 0 1)))
  (scenario "native state storage grows while preserving every published row"
    (each (width) ('(0 1 127 128 160))
      (equal (native-state-chain width)
        (list (+ width 1) (+ width 1) (if (< width 128) 128 256) #t #t))))
  (property "recursive target kernels are interned once"
    (bindings (observed (native-recursive-states)))
    (equal (car observed) 4)
    (equal (cadr observed) 4)
    (equal (caddr observed) '((0 2 5) (1) (2 3 5) (4)))
    (equal (cadddr observed)
      '((((nonterminal A) . 1) ((terminal token a) . 2)) ()
        (((terminal token a) . 2) ((nonterminal A) . 3)) ())))
  (scenario "complete queue publishes every ID once while borrowed scratch resets"
    (each (productions) (prediction-fixtures)
      (scope ((observed (construction-observation productions)))
        (equal (list-ref observed 1) (car observed))
        (equal (list-ref observed 2) (iota (car observed)))
        (equal (list-ref observed 3) (iota (car observed)))
        (equal (list-ref observed 4) (iota (car observed)))
        (equal (list-ref observed 5) (car observed))
        (equal (list-ref observed 6) #t)
        (equal (list-ref observed 10) (car observed))
        (equal (list-ref observed 11) #t)
        (equal (list-ref observed 12) #t))))
  (scenario "distinct kernels share aggregate templates within each fresh builder"
    (each (token) ('(alpha beta))
      (scope ((observed (construction-observation
        `((0 start ((nonterminal A)) concat #f)
          (1 A ((terminal token ,token) (nonterminal A)) concat #f)
          (2 A () concat #f)))))
        (equal (car observed) 4)
        (equal (list-ref observed 9) 2)
        (equal (list-ref observed 10) 4)
        (equal (list-ref observed 11) #t)
        (equal (list-ref observed 12) #t))))
  (scenario "physical plans retain shared templates while scratch carries between calls"
    (each (productions) (prediction-fixtures)
      (scope ((observed (construction-observation productions #f)))
        (equal (list-ref observed 6) #t)
        (equal (list-ref observed 11) #t)
        (equal (list-ref observed 12) #t))))
  (property "complete publication retains FIFO IDs across physical vector growth"
    (bindings
      (productions (list (list 0 'start
        (map (lambda (id) (list 'terminal 'token id)) (iota 129)) 'concat #f)))
      (observed (construction-observation productions)))
    (equal (car observed) 130)
    (equal (list-ref observed 1) 130)
    (equal (list-ref observed 2) (iota 130))
    (equal (list-ref observed 3) (iota 130))
    (equal (list-ref observed 4) (iota 130))
    (equal (list-ref observed 6) #t)
    (equal (list-ref observed 9) 1)
    (equal (list-ref observed 10) 130)
    (equal (list-ref observed 11) #t)
    (equal (list-ref observed 12) #t)
    (equal (list-ref observed 13) 128))
  (scenario "captured state IDs remain bounded across repeated physical vector expansion"
    (each (case)
      ('((127 128 0 ())
         (128 256 128 ((128 256)))
         (255 256 128 ((128 256)))
         (256 512 384 ((128 256) (256 512)))
         (511 512 384 ((128 256) (256 512)))
         (512 1024 896 ((128 256) (256 512) (512 1024)))))
      (scope ((width (car case)) (capacity (cadr case)) (copies (caddr case)) (growths (cadddr case)))
        (scope ((observed (construction-observation
          (list (list 0 'start (map (lambda (id) (list 'terminal 'token id)) (iota width)) 'concat #f)) #f)))
        (equal (car observed) (+ width 1))
        (equal (list-ref observed 12) #t)
        (equal (list-ref observed 13) copies)
        (equal (list-ref observed 14) capacity)
        (equal (list-ref observed 15) growths)
        (equal (list-ref observed 16) #t)))))
  (scenario "final physical queries reject a damaged shared index after publication"
    (each (damage expected) ('(#f #t) '(#t #f))
      (scope ((observed (construction-observation
        '((0 start ((nonterminal A)) concat #f)
          (1 A ((terminal token a) (nonterminal A)) concat #f)
          (2 A () concat #f)) #f damage)))
        (equal (list-ref observed 12) #t)
        (equal (list-ref observed 16) expected))))
  (scenario "one physical copy loop preserves three vectors for every supported positive layout"
    (each (initial) ('(1 2 3 5 128))
      (scope ((observed (joint-construction-observation
        (list (list 0 'start (map (lambda (id) (list 'terminal 'token id)) (iota 129)) 'concat #f)) initial)))
        (equal (car observed) 130)
        (equal (list-ref observed 2) #t)
        (equal (list-ref observed 3) #t)
        (equal (list-ref observed 4) #t)
        (equal (list-ref observed 5) #t))))
  (scenario "initial allocation without growth keeps aligned empty transition rows"
    (each (initial) ('(1 3 128))
      (scope ((observed (joint-construction-observation '((0 start () concat #f)) initial)))
        (equal observed (list 1 initial #t #t #t #t)))))
  (scenario "joint growth retains shared prediction objects and recursive target rows"
    (each (initial) ('(1 3))
      (scope ((observed (joint-construction-observation
        '((0 start ((nonterminal A)) concat #f)
          (1 A ((terminal token a) (nonterminal A)) concat #f)
          (2 A () concat #f)) initial)))
        (equal (car observed) 4)
        (equal (list-ref observed 2) #t)
        (equal (list-ref observed 3) #t)
        (equal (list-ref observed 4) #t)
        (equal (list-ref observed 5) #t))))
  (scenario "zero and negative initial capacities are rejected before source execution"
    (each (initial) ('(0 -1))
      (raises (joint-construction-observation '((0 start () concat #f)) initial) true)))
  (scenario "source growth switches owners only after all forward copies finish"
    (each (capacity) ('(1 2 3 5 128))
      (equal (growth-control-observation capacity capacity) '(#t #t #t #t #t))))
  (scenario "source growth guard leaves spare capacity and owners untouched"
    (each (capacity) ('(1 3 128))
      (equal (growth-control-observation capacity 0) '(#t #t #t))
      (equal (growth-control-observation capacity (- capacity 1)) '(#t #t #t))))
  (scenario "source append captures the old ID and registers before advancing the counter"
    (each (capacity) ('(1 2 3 5 128))
      (each (count) ((list 0 (- capacity 1) capacity))
        (equal (append-control-observation capacity count) '(#t #t #t #t #t)))))
  (scenario "source initialization precedes FIFO actions and only fresh targets enqueue"
    (each (capacity) ('(1 3 128))
      (equal (fifo-source-observation '((0 start () concat #f)) capacity) '(1 #t #t #t #t))
      (equal (fifo-source-observation
        '((0 start ((nonterminal A)) concat #f)
          (1 A ((terminal token a) (nonterminal A)) concat #f)
          (2 A () concat #f)) capacity) '(4 #t #t #t #t))))
  (scenario "source target callback treats ID zero as existing"
    (equal (existing-zero-observation) '((token . 0))))
  (scenario "source direct and field paths deliver the same ordered callbacks"
    (each (width) ((append '(1 2 5) (list (integer-length (##greatest-fixnum)))))
      (each (route) ('(native fixnum direct field))
        (each (mask) ((traversal-masks width))
          (scope ((observed (traversal-observation mask width route))
                  (expected (traversal-reference mask)))
            (equal (car observed) expected)
            (equal (caddr observed) (apply append (map (lambda (_) '(callback clear)) expected)))
            (equal (if (or (zero? mask) (memq route '(direct fixnum))) (cadr observed) '()) '())
            (equal (if (and (eq? route 'field) (not (zero? mask)))
                     (pair? (cadr observed)) #t) #t))))))
  (property "source traversal propagates callback failure before clearing"
    (bindings (calls 0) (sentinel (list 'callback-stop)))
    (equal (with-catch identity (lambda ()
      (traversal-observation (- (arithmetic-shift 1 128) 1) 5 'field
        (lambda (_) (set! calls (+ calls 1)) (raise sentinel))))) sentinel)
    (equal calls 1))
  (property "source traversal retains its frame across nested callbacks"
    (bindings (nested '())
      (observed (traversal-observation (- (arithmetic-shift 1 128) 1) 5 'field
        (lambda (_) (set! nested (cons (car (traversal-observation 5 2 'field)) nested))))))
    (equal (car observed) (iota 128))
    (equal nested (make-list 128 '(0 2))))
  (property "two-pass coordinates retain empty states and repeated core IDs"
    (bindings (observed (native-coordinates '(() (0 65 2048) () (0 130))
                          '((0 0) (1 0) (1 65) (1 2048) (1 66) (2 0) (3 0) (3 130)))))
    (equal observed '((0 0 3 3 5) (0 65 2048 0 130) 5 (#f 0 1 2 #f #f 3 4)))))
