(import :gerbil-parser/language-test-support)
;;; Independent profiles exercise POO inheritance and closed scanner admission.
(import :std/test
 (only-in :clan/poo/object .cc)
 (only-in :clan/poo/mop element?)
 (only-in :gerbil-parser/src/language/scanner-profile
  ScannerProfile. ScannerProfileContract defscanner-profile compile-scanner-profile)
 (only-in :gerbil-parser/src/runtime/source-scanner make-contextual-source-scanner
  source-scanner-tokens source-scanner-initial-state source-scanner-step
  source-scan-state-context source-scan-state-with-context)
 (only-in :gerbil-parser/src/runtime/token token-lexeme token-start token-end))

(defscanner-profile (letters :: self ScannerProfile.)
 (modes main) (initial-mode main) (tokens letter)
 (rules (letter main letter (literal "α") 0 keep)))
(defscanner-profile (digits :: self letters)
 (rules (digit main letter (literal "1") 0 keep)))
(def (worker profile source)
 (make-contextual-source-scanner (compile-scanner-profile profile) source 'source))
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(deflanguage-parser-tests scanner-profile-test "reusable POO scanner profiles"
  (engine)
  (scenario "independent declarations inherit slots and replace values"
   (equal (map token-lexeme (source-scanner-tokens (worker letters "αα") 'source))  '("α" "α"))
   (equal (map token-lexeme (source-scanner-tokens (worker digits "11") 'source))  '("1" "1"))
   (equal (rejects? (lambda () (source-scanner-tokens (worker digits "α") 'source)))  #t)
   (scope ((extension (.cc letters 'rules '((word main letter (literals ("α" "β")) 0 keep)))))
    (equal (map token-lexeme (source-scanner-tokens (worker extension "αβ") 'source))  '("α" "β"))))
  (scenario "contracts reject callbacks, missing modes and recursive guard data"
   (each (row) ((list (cons 'rules (lambda () '()))
          (cons 'initial-mode 'missing)
          (cons 'tokens '(other))
          (cons 'rules '((bad main letter (literal "") 0 keep)))
          (cons 'rules '((bad main letter (line-prefix "#\n" "\n") 0 keep)))
          (cons 'rules '((bad main letter (literal "α") 0 (expect-marker-in #f missing))))
          (cons 'rules '((bad main letter (unless-prefix ("#") () (unless-prefix ("#") () (literal "α"))) 0 keep)))))
    (equal (element? ScannerProfileContract (.cc letters (car row) (cdr row)))  #f)))
  (scenario "contextual workers reject foreign and displaced internal checkpoints"
   (sequential ((source "αα") (left (worker letters source)) (right (worker letters source))
          (initial (source-scanner-initial-state left)))
    (scope-values (((token next) (source-scanner-step left initial 'source)))
     (equal (list (token-start token) (token-end token))  '(0 2))
     (equal (rejects? (lambda () (source-scanner-step right initial 'source)))  #t)
     (equal (rejects? (lambda () (source-scanner-step left
       (source-scan-state-with-context initial (source-scan-state-context next)) 'source)))  #t)
     (scope-values (((again replay) (source-scanner-step left initial 'source)))
      (equal (list (token-start again) (token-end again))  '(0 2)))))))
