(import :std/test ../src/compiler/event-fold-scheme-expressions)
(export fold-condition-test)
(def bindings '((left . left) (right . right)))
(def fold-condition-test
  (test-suite "Conditional truth versus Boolean publication"
    (test-case "conditional nesting does not normalize values"
      (check (fold-scheme-condition '(and (state left) (or (state right) (bool #f))) bindings '())
        => '(and left (or right #f))))
    (test-case "published Boolean state still normalizes"
      (check (fold-scheme-predicate '(and (state left) (state right)) bindings '())
        => '(if (and left right) #t #f)))
    (test-case "a condition cannot leak its lowering context"
      (fold-scheme-condition '(or (state left) (state right)) bindings '())
      (check (fold-scheme-predicate '(or (state left) (state right)) bindings '())
        => '(if (or left right) #t #f)))
    (test-case "short circuit source order remains explicit"
      (check (fold-scheme-condition '(and (bool #f) (line-starts-with "x")) bindings '())
        => '(and #f (fold-byte-prefix? source-bytes start end '#u8(120) #f))))))
