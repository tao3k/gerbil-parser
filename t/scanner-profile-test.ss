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
(export scanner-profile-test)
(defscanner-profile (letters :: self ScannerProfile.)
 (modes main) (initial-mode main) (tokens letter)
 (rules (letter main letter (literal "α") 0 keep)))
(defscanner-profile (digits :: self letters)
 (rules (digit main letter (literal "1") 0 keep)))
(def (worker profile source)
 (make-contextual-source-scanner (compile-scanner-profile profile) source 'source))
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def scanner-profile-test
 (test-suite "reusable POO scanner profiles"
  (test-case "independent declarations inherit slots and replace values"
   (check (map token-lexeme (source-scanner-tokens (worker letters "αα") 'source)) => '("α" "α"))
   (check (map token-lexeme (source-scanner-tokens (worker digits "11") 'source)) => '("1" "1"))
   (check (rejects? (lambda () (source-scanner-tokens (worker digits "α") 'source))) => #t)
   (let (extension (.cc letters 'rules '((word main letter (literals ("α" "β")) 0 keep))))
    (check (map token-lexeme (source-scanner-tokens (worker extension "αβ") 'source)) => '("α" "β"))))
  (test-case "contracts reject callbacks, missing modes and recursive guard data"
   (for-each (lambda (row)
    (check (element? ScannerProfileContract (.cc letters (car row) (cdr row))) => #f))
    (list (cons 'rules (lambda () '()))
          (cons 'initial-mode 'missing)
          (cons 'tokens '(other))
          (cons 'rules '((bad main letter (literal "") 0 keep)))
          (cons 'rules '((bad main letter (line-prefix "#\n" "\n") 0 keep)))
          (cons 'rules '((bad main letter (literal "α") 0 (expect-marker-in #f missing))))
          (cons 'rules '((bad main letter (unless-prefix ("#") () (unless-prefix ("#") () (literal "α"))) 0 keep))))))
  (test-case "contextual workers reject foreign and displaced internal checkpoints"
   (let* ((source "αα") (left (worker letters source)) (right (worker letters source))
          (initial (source-scanner-initial-state left)))
    (let-values (((token next) (source-scanner-step left initial 'source)))
     (check (list (token-start token) (token-end token)) => '(0 2))
     (check (rejects? (lambda () (source-scanner-step right initial 'source))) => #t)
     (check (rejects? (lambda () (source-scanner-step left
       (source-scan-state-with-context initial (source-scan-state-context next)) 'source))) => #t)
     (let-values (((again replay) (source-scanner-step left initial 'source)))
      (check (list (token-start again) (token-end again)) => '(0 2))))))))
