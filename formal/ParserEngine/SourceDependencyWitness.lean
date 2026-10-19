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
def adapterSource : Datum :=
(l [(s "def"), (l [(s "source-procedure"), (s "path"), (s "name"), (s "dependencies"), (l [(s "transform"), (s "identity")])]), (l [(s "let"), (l [(s "declaration"), (l [(s "transform"), (l [(s "read-source-definition"), (s "path"), (s "name")])])]), (l [(s "source-expression-procedure"), (l [(s "cons"), (l [(s "quote"), (s "lambda")]), (l [(s "cons"), (l [(s "append"), (l [(s "cdr"), (l [(s "cadr"), (s "declaration")])]), (s "dependencies")]), (l [(s "cddr"), (s "declaration")])])])])])])
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
