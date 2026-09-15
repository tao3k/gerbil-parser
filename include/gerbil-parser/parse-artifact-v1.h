#ifndef GERBIL_PARSER_PARSE_ARTIFACT_V1_H
#define GERBIL_PARSER_PARSE_ARTIFACT_V1_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  int32_t status;
  char *payload;
} gerbil_parser_result_v1;

void gerbil_parser_result_v1_init(gerbil_parser_result_v1 *result);
void gerbil_parser_result_v1_release(gerbil_parser_result_v1 *result);
uint32_t gerbil_parser_native_abi_version(void);
int32_t gerbil_parser_native_descriptor(gerbil_parser_result_v1 *result);
int32_t gerbil_parser_native_parse(
    const char *source,
    gerbil_parser_result_v1 *result);

#ifdef __cplusplus
}
#endif

#endif
