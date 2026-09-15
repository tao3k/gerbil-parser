;;; -*- Gerbil -*-
;;; Parser-owned C ABI for the ISO GQL ParseArtifact v1 surface.

(import :std/foreign
        :std/text/json
        (only-in ../language/descriptor language-grammar-grammar)
        (only-in ../runtime/artifact parse-artifact-events parse-artifact-ref)
        (only-in ../../languages/gql/iso-39075-2024/grammar
                 gql-iso-language-grammar)
        (only-in ../../languages/gql/iso-39075-2024/parser
                 parse-gql-iso-39075-2024))
(export native-abi-version
        native-descriptor-payload
        native-parse-payload)

(def +gerbil-parser-native-abi-version+ 1)
(def +gerbil-parser-native-descriptor-schema+
  "gerbil-parser.native-descriptor.v1")

(def (grammar-section name)
  (cdr (assq name (language-grammar-grammar gql-iso-language-grammar))))

(def (syntax-kind->json row)
  (vector (symbol->string (car row))
          (symbol->string (cadr row))
          (list->vector (map symbol->string (caddr row)))))

(def (terminal->json row)
  (vector (symbol->string (car row))
          (symbol->string (cadr row))))

(def (parse-artifact->json-object artifact)
  (hash (schema (parse-artifact-ref artifact 'schema))
        (status (parse-artifact-ref artifact 'status))
        (grammarDigest (parse-artifact-ref artifact 'grammarDigest))
        (sourceDigest (parse-artifact-ref artifact 'sourceDigest))
        (events (list->vector (parse-artifact-events artifact)))))

(def (native-abi-version)
  +gerbil-parser-native-abi-version+)

(def (native-descriptor-payload)
  (json-object->string
   (hash (schema +gerbil-parser-native-descriptor-schema+)
         (grammarDigest
          (parse-artifact-ref (parse-gql-iso-39075-2024 "")
                              'grammarDigest))
         (syntaxKinds
          (list->vector (map syntax-kind->json
                             (grammar-section 'syntax-kinds))))
         (terminals
          (list->vector (map terminal->json
                             (grammar-section 'terminals)))))))

(def (native-parse-payload source)
  (json-object->string
   (parse-artifact->json-object
    (parse-gql-iso-39075-2024 source))))

(begin-ffi
  ((struct gerbil_parser_result_v1 status payload)
   gerbil-parser-native-abi-version
   gerbil-parser-native-descriptor
   gerbil-parser-native-parse)

  (c-declare #<<END-C
#include <stdint.h>
#include <stdlib.h>

typedef struct {
  int32_t status;
  char *payload;
} gerbil_parser_result_v1;

void gerbil_parser_result_v1_init(gerbil_parser_result_v1 *result) {
  result->status = 0;
  result->payload = NULL;
}

void gerbil_parser_result_v1_release(gerbil_parser_result_v1 *result) {
  if (result->payload != NULL) {
    free(result->payload);
  }
  result->status = 0;
  result->payload = NULL;
}
END-C
  )

  (define-c-struct gerbil_parser_result_v1
    ((status . int32)
     (payload . UTF-8-string))
    #f #f #t)

  (c-define (gerbil-parser-native-abi-version)
    () unsigned-int32 "gerbil_parser_native_abi_version" "extern"
    (gerbil-parser/src/ffi/parse-artifact-v1#native-abi-version))

  (c-define (gerbil-parser-native-descriptor result)
    (gerbil_parser_result_v1-borrowed-ptr*) int32
    "gerbil_parser_native_descriptor" "extern"
    (with-exception-catcher
     (lambda (exception)
       (gerbil_parser_result_v1-status-set! result -1)
       (gerbil_parser_result_v1-payload-set! result "")
       -1)
     (lambda ()
       (gerbil_parser_result_v1-payload-set!
        result
        (gerbil-parser/src/ffi/parse-artifact-v1#native-descriptor-payload))
       (gerbil_parser_result_v1-status-set! result 0)
       0)))

  (c-define (gerbil-parser-native-parse source result)
    (UTF-8-string gerbil_parser_result_v1-borrowed-ptr*) int32
    "gerbil_parser_native_parse" "extern"
    (with-exception-catcher
     (lambda (exception)
       (gerbil_parser_result_v1-status-set! result -1)
       (gerbil_parser_result_v1-payload-set! result "")
       -1)
     (lambda ()
       (gerbil_parser_result_v1-payload-set!
        result
        (gerbil-parser/src/ffi/parse-artifact-v1#native-parse-payload source))
       (gerbil_parser_result_v1-status-set! result 0)
       0))))
