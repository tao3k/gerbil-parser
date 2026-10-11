;;; -*- Gerbil -*-

((schema . gerbil-parser.native-ffi-scenario.v1)
 (owner . gerbil-parser)
 (abiVersion . 2)
 (transport . caller-owned-result-struct)
 (forbiddenTransport . process-json-lines)
 (requiredSymbols
  "gerbil_parser_result_init"
  "gerbil_parser_result_release"
  "gerbil_parser_language_abi_version"
  "gerbil_parser_language_descriptor"
  "gerbil_parser_language_parse"))
