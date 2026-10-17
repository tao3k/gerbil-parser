(import :gerbil-parser/language-test-support)
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
(export  header-record-language-grammar header-record-language)

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

(deflanguage-parser-tests header-delimiter-test "closed header-derived delimiter rules"
  (engine)
    (property "independent record projection preserves its own source identity"
      (bindings (artifact (parse-header-record "記|§α|")) (projected (header-record-projection artifact)))
      (equal (cdr (assq 'value projected))  "α")
      (equal (cdr (assq 'sourceDigest projected))  (parse-artifact-ref artifact 'sourceDigest)))
    (scenario "engine rules derive separators and data without language callbacks"
      (equal (scan-header-delimiter "MSH|^~\\&|" 3 "MSH" 5 0)  4)
      (equal (scan-header-data "MSH|^~\\&α x|" 8 "MSH" 5 "\r\n")  11)
      (equal (scan-header-data "MSH|^~\\&\r" 8 "MSH" 5 "\r\n")  #f)
      (equal (scan-header-delimiter "記|§α§" 2 "記" 2 1)  3)
      (equal (scan-header-data "記|§α§" 3 "記" 2 ";")  4)
      (each (source) ('("MSH|^~||" "MSH1^~\\&" "MSHα^~\\&" "MSH|^" "REC|^~\\&"))
                  (equal (scan-header-delimiter source 3 "MSH" 5 0)  #f))
      (each (offset) ('(-1 1.5 5 100))
                  (equal (scan-header-data "記|§α§" offset "記" 2 ";")  #f)))
    (scenario "closed recipe admission rejects malformed data"
      (each (expression) ('((header-delimiter "" 2 0) (header-delimiter "記" 0 0)
                  (header-delimiter "記" 33 0) (header-delimiter "記" 2 2)
                  (header-delimiter "記" 2 -1) (header-delimiter "記" 2.5 0)
                  (header-data "記" 2 callback) (header-data "記" 2)
                  (header-data "記" 0 ";") (header-data "記" 33 ";"))) (equal (lexical-expression? expression)  #f))
      (equal (lexical-expression? '(header-data "記" 2 ""))  #t))
    (scenario "independent POO Loaders use the same rules and remain lossless"
      (act (validate LanguageLoaderContract header-record-language))
      (scope ((extended (.cc header-record-language 'metadata
                          (.cc (.ref header-record-language 'metadata) 'consumer 'downstream))))
        (act (validate LanguageLoaderContract extended))
        (each (row) ('(("記|§α|" . #t) ("記*$α*" . #t) ("記|§a b|" . #t)
           ("記||α|" . #f) ("記a§αa" . #f) ("記|§|" . #f) ("記|§α;|" . #f)))
           (sequential ((source (car row)) (artifact ((.ref extended '.parse) source)))
             (equal (parse-artifact-success? artifact)  (cdr row))
             (equal (parse-artifact-valid? artifact)  #t)
             (equal (parse-artifact-roundtrip artifact)  source))))
      (sequential ((source "MSH*$%!?*LEGACY*AU*FHIR*AU*202609170900**ADT$A08*1*P*2.5.1\r")
             (artifact ((.ref hl7-language '.parse) source)))
        (equal (parse-artifact-success? artifact)  #t)
        (equal (parse-artifact-valid? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  source))))
