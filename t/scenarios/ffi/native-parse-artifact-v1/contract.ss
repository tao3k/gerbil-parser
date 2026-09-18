;;; -*- Gerbil -*-

((schema . gerbil-parser.native-ffi-scenario.v1)
 (owner . gerbil-parser)
 (abiVersion . 1)
 (transport . caller-owned-result-struct)
 (forbiddenTransport . process-json-lines)
 (requiredSymbols
  "gerbil_parser_result_v1_init"
  "gerbil_parser_result_v1_release"
  "gerbil_parser_native_abi_version"
  "gerbil_parser_native_descriptor"
  "gerbil_parser_native_parse"))
