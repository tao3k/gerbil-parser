;;; Compiled nominal descriptors remain behind the existing language facades.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "../schema.ss" +grammar-schema+ +language-grammar-schema+) schema.))
(export Language.)
(.def (Language. @ (.get LanguageScopeContract proto))
  grammar-schema: schema.+grammar-schema+
  language-grammar-schema: schema.+language-grammar-schema+)
