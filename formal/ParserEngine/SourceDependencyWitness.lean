-- Generated reader data; proofs are maintained in separate Lean modules.
import ParserEngine.WalkSource
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def moduleSource : Datum :=
(l [(l [(s "import"), (s ":gerbil/expander"), (s ":gerbil/compiler/compile"), (s ":gerbil/compiler/method"), (s ":gerbil/compiler/base")]), (l [(s "export"), (s "prepare-source-procedure"), (s "source-expression-dependencies")]), (l [(s "defcompile-method"), (l [(s "collect-source-dependencies"), (.keyword "record"), (s "record")]), (l [(s "::source-dependency-collector"), (s "::collect-expression-refs")]), (l [(s "record")]), (.keyword "final"), (l [(s "%#ref"), (s "source-dependency-reference%")]), (l [(s "%#set!"), (s "source-dependency-assignment%")])]), (l [(s "def"), (l [(s "source-reference-resolved?"), (s "id")]), (l [(s "or"), (l [(s "resolve-identifier"), (s "id")]), (l [(s "let"), (l [(s "name"), (l [(s "stx-e"), (s "id")])]), (l [(s "and"), (l [(s "symbol?"), (s "name")]), (l [(s "string-prefix?"), (.text "##"), (l [(s "symbol->string"), (s "name")])]), (l [(s "procedure?"), (l [(s "##global-var-primitive-ref"), (l [(s "##make-global-var"), (s "name")])])])])])])]), (l [(s "def"), (l [(s "source-dependency-reference%"), (s "self"), (s "stx")]), (l [(s "ast-case"), (s "stx"), (l []), (l [(l [(s "_"), (s "id")]), (l [(s "unless"), (l [(s "source-reference-resolved?"), (l [(s "syntax"), (s "id")])]), (l [(l [(s "@"), (s "self"), (s "record")]), (l [(s "stx-e"), (l [(s "syntax"), (s "id")])])])])])])]), (l [(s "def"), (l [(s "source-dependency-assignment%"), (s "self"), (s "stx")]), (l [(s "ast-case"), (s "stx"), (l []), (l [(l [(s "_"), (s "id"), (s "expression")]), (l [(s "unless"), (l [(s "source-reference-resolved?"), (l [(s "syntax"), (s "id")])]), (l [(l [(s "@"), (s "self"), (s "record")]), (l [(s "stx-e"), (l [(s "syntax"), (s "id")])])])]), (l [(s "compile-e"), (s "self"), (l [(s "syntax"), (s "expression")])])])])]), (l [(s "def"), (l [(s "expanded-source-dependencies"), (s "core")]), (l [(s "let"), (l [(s "references"), (l [(s "quote"), (l [])])]), (l [(s "collect-source-dependencies"), (s "core"), (.keyword "record"), (l [(s "lambda"), (l [(s "name")]), (l [(s "set!"), (s "references"), (l [(s "cons"), (s "name"), (s "references")])])])]), (l [(s "reverse"), (s "references")])])]), (l [(s "def"), (l [(s "source-expression-dependencies"), (s "expression")]), (l [(s "expanded-source-dependencies"), (l [(s "core-expand"), (l [(s "datum->syntax"), (b false), (s "expression")])])])]), (l [(s "def"), (l [(s "prepare-source-procedure"), (s "expression")]), (l [(s "let*"), (l [(l [(s "core"), (l [(s "core-expand"), (l [(s "datum->syntax"), (b false), (s "expression")])])]), (l [(s "missing"), (l [(s "expanded-source-dependencies"), (s "core")])])]), (l [(s "unless"), (l [(s "and"), (l [(s "stx-pair?"), (s "core")]), (l [(s "memq"), (l [(s "stx-e"), (l [(s "stx-car"), (s "core")])]), (l [(s "quote"), (l [(s "%#lambda"), (s "%#case-lambda")])])])]), (l [(s "error"), (.text "source witness must expand to a procedure literal")])]), (l [(s "unless"), (l [(s "null?"), (s "missing")]), (l [(s "error"), (.text "source procedure has undeclared dependencies"), (s "missing")])]), (l [(s "let"), (l [(s "procedure"), (l [(s "eval-syntax*"), (s "core")])]), (l [(s "unless"), (l [(s "procedure?"), (s "procedure")]), (l [(s "error"), (.text "source witness is not a procedure")])]), (s "procedure")])])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def resolutionSource : Datum :=
(l [(s "def"), (l [(s "source-reference-resolved?"), (s "id")]), (l [(s "or"), (l [(s "resolve-identifier"), (s "id")]), (l [(s "let"), (l [(s "name"), (l [(s "stx-e"), (s "id")])]), (l [(s "and"), (l [(s "symbol?"), (s "name")]), (l [(s "string-prefix?"), (.text "##"), (l [(s "symbol->string"), (s "name")])]), (l [(s "procedure?"), (l [(s "##global-var-primitive-ref"), (l [(s "##make-global-var"), (s "name")])])])])])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def readerSource : Datum :=
(l [(s "def"), (l [(s "read-source-definition-port"), (s "port"), (s "name")]), (l [(s "let"), (l [(s "found"), (l [(s "let"), (s "loop"), (l [(l [(s "found"), (l [(s "quote"), (l [])])])]), (l [(s "let"), (l [(s "form"), (l [(s "read"), (s "port")])]), (l [(s "cond"), (l [(l [(s "eof-object?"), (s "form")]), (l [(s "reverse"), (s "found")])]), (l [(l [(s "source-declaration-matches?"), (s "form"), (s "name")]), (l [(s "loop"), (l [(s "cons"), (s "form"), (s "found")])])]), (l [(s "else"), (l [(s "loop"), (s "found")])])])])])]), (l [(s "unless"), (l [(s "="), (l [(s "length"), (s "found")]), (n 1)]), (l [(s "error"), (.text "expected one source declaration"), (s "name")])]), (l [(s "car"), (s "found")])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def matcherSource : Datum :=
(l [(s "def"), (l [(s "source-declaration-matches?"), (s "form"), (s "name")]), (l [(s "and"), (l [(s "list?"), (s "form")]), (l [(s ">="), (l [(s "length"), (s "form")]), (n 3)]), (l [(s "eq?"), (l [(s "car"), (s "form")]), (l [(s "quote"), (s "def")])]), (l [(s "or"), (l [(s "eq?"), (l [(s "cadr"), (s "form")]), (s "name")]), (l [(s "and"), (l [(s "pair?"), (l [(s "cadr"), (s "form")])]), (l [(s "eq?"), (l [(s "caadr"), (s "form")]), (s "name")])])])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def snapshotSource : Datum :=
(l [(s "def"), (l [(s "copy-source-data"), (s "value")]), (l [(s "u8vector->object"), (l [(s "object->u8vector"), (s "value")])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def transformSource : Datum :=
(l [(s "def"), (l [(s "transform-source-declaration"), (s "declaration"), (s "transform")]), (l [(s "unless"), (l [(s "and"), (l [(s "list?"), (s "declaration")]), (l [(s ">="), (l [(s "length"), (s "declaration")]), (n 3)]), (l [(s "eq?"), (l [(s "car"), (s "declaration")]), (l [(s "quote"), (s "def")])]), (l [(s "pair?"), (l [(s "cadr"), (s "declaration")])]), (l [(s "symbol?"), (l [(s "caadr"), (s "declaration")])]), (l [(s "list?"), (l [(s "cdadr"), (s "declaration")])])]), (l [(s "error"), (.text "source witness requires a procedure declaration")])]), (l [(s "let*"), (l [(l [(s "header"), (l [(s "copy-source-data"), (l [(s "cadr"), (s "declaration")])])]), (l [(s "declaration"), (l [(s "copy-source-data"), (s "declaration")])])]), (l [(s "let"), (l [(s "transformed"), (l [(s "transform"), (s "declaration")])]), (l [(s "unless"), (l [(s "and"), (l [(s "list?"), (s "transformed")]), (l [(s ">="), (l [(s "length"), (s "transformed")]), (n 3)]), (l [(s "eq?"), (l [(s "car"), (s "transformed")]), (l [(s "quote"), (s "def")])]), (l [(s "equal?"), (l [(s "cadr"), (s "transformed")]), (s "header")])]), (l [(s "error"), (.text "source transformation changed the declaration interface")])]), (s "transformed")])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def adapterSource : Datum :=
(l [(s "def"), (l [(s "source-procedure"), (s "path"), (s "name"), (s "dependencies"), (l [(s "transform"), (s "identity")])]), (l [(s "let"), (l [(s "declaration"), (l [(s "transform-source-declaration"), (l [(s "read-source-definition"), (s "path"), (s "name")]), (s "transform")])]), (l [(s "source-expression-procedure"), (l [(s "cons"), (l [(s "quote"), (s "lambda")]), (l [(s "cons"), (l [(s "append"), (l [(s "cdr"), (l [(s "cadr"), (s "declaration")])]), (s "dependencies")]), (l [(s "cddr"), (s "declaration")])])])])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def eventAdapterSource : Datum :=
(l [(s "def"), (l [(s "source-procedure"), (s "declaration")]), (l [(s "source-expression-procedure"), (l [(s "cons"), (l [(s "quote"), (s "lambda")]), (l [(s "cons"), (l [(s "cdr"), (l [(s "cadr"), (s "declaration")])]), (l [(s "cddr"), (s "declaration")])])])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def recordSource : Datum :=
(l [(s "def"), (l [(s "expanded-source-dependencies"), (s "core")]), (l [(s "let"), (l [(s "references"), (l [(s "quote"), (l [])])]), (l [(s "collect-source-dependencies"), (s "core"), (.keyword "record"), (l [(s "lambda"), (l [(s "name")]), (l [(s "set!"), (s "references"), (l [(s "cons"), (s "name"), (s "references")])])])]), (l [(s "reverse"), (s "references")])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def referenceSource : Datum :=
(l [(s "def"), (l [(s "source-dependency-reference%"), (s "self"), (s "stx")]), (l [(s "ast-case"), (s "stx"), (l []), (l [(l [(s "_"), (s "id")]), (l [(s "unless"), (l [(s "source-reference-resolved?"), (l [(s "syntax"), (s "id")])]), (l [(l [(s "@"), (s "self"), (s "record")]), (l [(s "stx-e"), (l [(s "syntax"), (s "id")])])])])])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def assignmentSource : Datum :=
(l [(s "def"), (l [(s "source-dependency-assignment%"), (s "self"), (s "stx")]), (l [(s "ast-case"), (s "stx"), (l []), (l [(l [(s "_"), (s "id"), (s "expression")]), (l [(s "unless"), (l [(s "source-reference-resolved?"), (l [(s "syntax"), (s "id")])]), (l [(l [(s "@"), (s "self"), (s "record")]), (l [(s "stx-e"), (l [(s "syntax"), (s "id")])])])]), (l [(s "compile-e"), (s "self"), (l [(s "syntax"), (s "expression")])])])])])
end ParserEngine.SourceDependencyWitness
namespace ParserEngine.SourceDependencyWitness
open WalkSource
def prepareSource : Datum :=
(l [(s "def"), (l [(s "prepare-source-procedure"), (s "expression")]), (l [(s "let*"), (l [(l [(s "core"), (l [(s "core-expand"), (l [(s "datum->syntax"), (b false), (s "expression")])])]), (l [(s "missing"), (l [(s "expanded-source-dependencies"), (s "core")])])]), (l [(s "unless"), (l [(s "and"), (l [(s "stx-pair?"), (s "core")]), (l [(s "memq"), (l [(s "stx-e"), (l [(s "stx-car"), (s "core")])]), (l [(s "quote"), (l [(s "%#lambda"), (s "%#case-lambda")])])])]), (l [(s "error"), (.text "source witness must expand to a procedure literal")])]), (l [(s "unless"), (l [(s "null?"), (s "missing")]), (l [(s "error"), (.text "source procedure has undeclared dependencies"), (s "missing")])]), (l [(s "let"), (l [(s "procedure"), (l [(s "eval-syntax*"), (s "core")])]), (l [(s "unless"), (l [(s "procedure?"), (s "procedure")]), (l [(s "error"), (.text "source witness is not a procedure")])]), (s "procedure")])])])
end ParserEngine.SourceDependencyWitness
