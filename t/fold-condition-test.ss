(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-scheme-expressions)

(def bindings '((left . left) (right . right)))
(deflanguage-parser-tests fold-condition-test "Conditional truth versus Boolean publication"
  (engine)
    (property "conditional nesting does not normalize values"
      (bindings)
      (equal (fold-scheme-condition '(and (state left) (or (state right) (bool #f))) bindings '())
         '(and left (or right #f))))
    (property "published Boolean state still normalizes"
      (bindings)
      (equal (fold-scheme-predicate '(and (state left) (state right)) bindings '())
         '(if (and left right) #t #f)))
    (scenario "a condition cannot leak its lowering context"
      (act (fold-scheme-condition '(or (state left) (state right)) bindings '()))
      (equal (fold-scheme-predicate '(or (state left) (state right)) bindings '())
         '(if (or left right) #t #f)))
    (property "short circuit source order remains explicit"
      (bindings)
      (equal (fold-scheme-condition '(and (bool #f) (line-starts-with "x")) bindings '())
         '(and #f (fold-byte-prefix? source-bytes start end '#u8(120) #f)))))
