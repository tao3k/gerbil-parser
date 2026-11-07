#ifndef GERBIL_PARSER_RUST_RUNTIME_AOT_H
#define GERBIL_PARSER_RUST_RUNTIME_AOT_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Initialize before first use. Calls replace the owned payload, including
 * on errors. Release after last use; repeated release and NULL are safe.
 * Calls require the initialized Gerbil runtime and its calling thread. */
typedef struct {
  int32_t status;
  uint8_t *payload;
  size_t length;
} gerbil_parser_runtime_result;

void gerbil_parser_runtime_result_init(
    gerbil_parser_runtime_result *result);
void gerbil_parser_runtime_result_release(
    gerbil_parser_runtime_result *result);
uint32_t gerbil_parser_runtime_aot_abi_version(void);
int32_t gerbil_parser_runtime_compile(
    const char *grammar_path,
    gerbil_parser_runtime_result *result);

#ifdef __cplusplus
}
#endif

#endif
