(import :gerbil-parser/language-test-support
        (only-in :clan/poo/mop element?)
        (only-in :gerbil-parser/src/runtime/lr-data lr-terminal? lr-precedence?)
        (only-in :gerbil-parser/src/runtime/function-types LRActionEntry LRActionRows))

(deflanguage-parser-tests lr-data-contract-test "shared LR vocabulary with separate phase constraints"
  (engine)
    (scenario "terminal and precedence leaves share positive and negative controls"
      (sequential ()
        (each (v) ((list '(terminal eof) '(terminal token name)
                        '(terminal literal "") '(terminal layout-start "indent")))
          (equal (lr-terminal? v) #t))
        (each (v) ((list '(terminal eof extra) '(terminal token "wrong")
                        '(terminal literal symbol) '(nonterminal name)))
          (equal (lr-terminal? v) #f))
        (each (v) ((list #f '(none 0) '(left -1) '(right 2) '(dynamic 3)))
          (equal (lr-precedence? v) #t))
        (each (v) ((list '(left "wrong") '(left 1.0) '(unknown 1) '(left 1 extra)))
          (equal (lr-precedence? v) #f))))
    (scenario "runtime projection retains recursive forks and layout shifts"
      (sequential ()
        (equal (element? LRActionEntry '((terminal eof) fork (fork (reduce 0)))) #t)
        (equal (element? LRActionEntry '((terminal eof) layout-shift layout-next 0)) #t)
        (equal (element? LRActionEntry '((terminal eof) shift -1 #f)) #f)
        (equal (element? LRActionEntry '((terminal eof) shift 0 (left "bad"))) #f)
        (equal (element? LRActionRows
          '#((((terminal eof) layout-guard (shift 0 #f) (reduce 0))))) #t))))
