# Fast lexical algorithm loop; keep the POO Flow Case heap fence pre-import.
test-lexer:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/ranked-regular-scanner-test.ss

# LR mode admission is checked separately from the inner scanner transition.
test-lexical-mode:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/grammar-composition-test.ss

# Full qualification after the focused algorithm loop.
test-all:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/... languages/...

# Reports each complete language batch sample without a shell timing wrapper.
benchmark-versioned-matched label="current" samples="20":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/versioned-languages/matched-batch.ss {{label}} {{samples}}
