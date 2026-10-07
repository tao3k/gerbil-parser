(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; Closed header-derived scanning is shared by independent POO language Loaders.
(import :std/test
        (only-in :clan/poo/object .o .cc .ref)
        :gerbil-parser/language-support/projection
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-ref)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-support deflanguage deflanguage-parser-loader LanguageLoader. LanguageLoaderContract)
        (only-in :gerbil-parser/src/grammar/lexical-algebra lexical-expression?)
        (only-in :gerbil-parser/src/runtime/lexical-source scan-header-delimiter scan-header-data)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success? parse-artifact-valid? parse-artifact-roundtrip)
        (only-in :gerbil-parser/languages/hl7/parser hl7-language))
(export header-delimiter-test header-record-language-grammar header-record-language)

(begin
 (deflanguage header-record
  (syntax
   (lexical
    (root document)
    (lex (field Separator (header-delimiter "記" 2 0))
       (component ComponentSeparator (header-delimiter "記" 2 1))
       (data Data (header-data "記" 2 ";")))))
  (rules (document (node SourceFile
                    (literal "記") field component (field value data) field))))
 (bind-fixture-grammar-release header-record "header-record" "v1" "header-record.v1") )
(deflanguage-parser-loader (header-record-language :: self LanguageLoader.)
  (grammar header-record-language-grammar)
  (parse parse-header-record)
  (slots metadata: (.o dialect: 'header-record)))

(deflanguage-projection header-record-projection
 (grammar header-record-language-grammar)
 (strategy (.o (:: self RecordProjection.) prefix: "記" delimiter-count: 2
              constants: '((schema . "header-record.output.v1"))
              columns: '((value split (record "記" 1) 1 1)))))

(def header-delimiter-test
  (test-suite "closed header-derived delimiter rules"
    (test-case "independent record projection preserves its own source identity"
      (let* ((artifact (parse-header-record "記|§α|"))
             (projected (header-record-projection artifact)))
        (check (cdr (assq 'value projected)) => "α")
        (check (cdr (assq 'sourceDigest projected)) => (parse-artifact-ref artifact 'sourceDigest))))
    (test-case "engine rules derive separators and data without language callbacks"
      (check (scan-header-delimiter "MSH|^~\\&|" 3 "MSH" 5 0) => 4)
      (check (scan-header-data "MSH|^~\\&α x|" 8 "MSH" 5 "\r\n") => 11)
      (check (scan-header-data "MSH|^~\\&\r" 8 "MSH" 5 "\r\n") => #f)
      (check (scan-header-delimiter "記|§α§" 2 "記" 2 1) => 3)
      (check (scan-header-data "記|§α§" 3 "記" 2 ";") => 4)
      (for-each (lambda (source)
                  (check (scan-header-delimiter source 3 "MSH" 5 0) => #f))
                '("MSH|^~||" "MSH1^~\\&" "MSHα^~\\&" "MSH|^" "REC|^~\\&"))
      (for-each (lambda (offset)
                  (check (scan-header-data "記|§α§" offset "記" 2 ";") => #f))
                '(-1 1.5 5 100)))
    (test-case "closed recipe admission rejects malformed data"
      (for-each (lambda (expression) (check (lexical-expression? expression) => #f))
                '((header-delimiter "" 2 0) (header-delimiter "記" 0 0)
                  (header-delimiter "記" 33 0) (header-delimiter "記" 2 2)
                  (header-delimiter "記" 2 -1) (header-delimiter "記" 2.5 0)
                  (header-data "記" 2 callback) (header-data "記" 2)
                  (header-data "記" 0 ";") (header-data "記" 33 ";")))
      (check (lexical-expression? '(header-data "記" 2 "")) => #t))
    (test-case "independent POO Loaders use the same rules and remain lossless"
      (validate LanguageLoaderContract header-record-language)
      (let (extended (.cc header-record-language 'metadata
                          (.cc (.ref header-record-language 'metadata) 'consumer 'downstream)))
        (validate LanguageLoaderContract extended)
        (for-each
         (lambda (row)
           (let* ((source (car row)) (artifact ((.ref extended '.parse) source)))
             (check (parse-artifact-success? artifact) => (cdr row))
             (check (parse-artifact-valid? artifact) => #t)
             (check (parse-artifact-roundtrip artifact) => source)))
         '(("記|§α|" . #t) ("記*$α*" . #t) ("記|§a b|" . #t)
           ("記||α|" . #f) ("記a§αa" . #f) ("記|§|" . #f) ("記|§α;|" . #f))))
      (let* ((source "MSH*$%!?*LEGACY*AU*FHIR*AU*202609170900**ADT$A08*1*P*2.5.1\r")
             (artifact ((.ref hl7-language '.parse) source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))))
