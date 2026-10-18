;;; Runtime capabilities must work without the GXI syntax expansion.
(import :gerbil-parser/language-test-support
        (only-in :gerbil/runtime/init __scheme-source)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language parse-arithmetic)
        (only-in :gerbil-parser/src/testing/language-product check-language-abi-entry)
        (only-in :gerbil-parser/src/ffi/language-artifact-codec bind-language-abi))

(def (check-host-product source accepted?)
  (let (artifact (parse-arithmetic source))
    (parameterize ((__scheme-source 'runtime-product-control))
      (check-language-abi-entry arithmetic-language source artifact accepted?))))

(deflanguage-parser-tests product-runtime-test "runtime product capability context"
  (engine (exclusive #t))
  (scenario "accepted native product does not require GXI syntax evaluation"
    (scope ((source-mode (__scheme-source)))
      (matches bind-language-abi procedure?)
      (act (check-host-product "1 + 2" #t))
      (equal (__scheme-source) source-mode)))
  (scenario "rejected native product retains its status in host Scheme mode"
    (scope ((source-mode (__scheme-source)))
      (act (check-host-product "1 +" #f))
      (equal (__scheme-source) source-mode)))
  (scenario "host Scheme mode rejects the retired Gerbil import path"
    (scope ((source-mode (__scheme-source)))
      (raises
        (parameterize ((__scheme-source 'runtime-product-control))
          (eval '(begin
                   (import (only-in :gerbil-parser/src/ffi/language-artifact-codec bind-language-abi))
                   bind-language-abi)))
        (lambda (_) #t))
      (equal (__scheme-source) source-mode)))
)
