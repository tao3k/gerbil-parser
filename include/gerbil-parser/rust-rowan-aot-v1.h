#ifndef GERBIL_PARSER_RUST_ROWAN_AOT_V1_H
#define GERBIL_PARSER_RUST_ROWAN_AOT_V1_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  int32_t status;
  uint8_t *payload;
  size_t length;
} gerbil_parser_rowan_result_v1;

void gerbil_parser_rowan_result_v1_init(
    gerbil_parser_rowan_result_v1 *result);
void gerbil_parser_rowan_result_v1_release(
    gerbil_parser_rowan_result_v1 *result);
uint32_t gerbil_parser_rowan_aot_abi_version(void);
int32_t gerbil_parser_rowan_compile(
    const char *grammar_path,
    gerbil_parser_rowan_result_v1 *result);

#ifdef __cplusplus
}
#endif

#endif
