#ifndef GERBIL_PARSER_LANGUAGE_V2_H
#define GERBIL_PARSER_LANGUAGE_V2_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
/* Handles are registered by a compiled language pack, never a name registry.
 * All calls require the initialized Gerbil runtime and its owning thread.
 * Release invalidates a handle; its numeric identity is never reused.
 * The library/runtime must remain loaded through the last call and release.
 * This API does not initialize/shutdown the runtime or admit arbitrary threads. */
typedef uint64_t gerbil_parser_language_v2;
typedef struct { int32_t status; uint8_t *payload; size_t length; } gerbil_parser_result_v2;
/* Initialize once, release after use. Each call replaces the C-owned payload.
 * Result payload contains GPA1 bytes (parse) or descriptor/error UTF-8 JSON.
 * Transport success is separate from accepted/rejected syntax in GPA1.
 * Source is length-bearing: embedded NUL is retained; invalid UTF-8 fails.
 * Maximum source length is 64 MiB. NULL source is allowed only for length zero. */
/* GPA1 wire format (little endian, no C struct layout dependency):
 * 80-byte header: magic[4], version:u32=1, syntax_status:u32 (0 accepted,
 * 1 rejected), event_count:u32, grammar_sha256[32], source_sha256[32].
 * Exactly event_count 24-byte records follow: tag:u32, symbol:u32, id:u64,
 * start:u32, end:u32. Tags 1/2 are node start/finish, 3/4 field start/finish,
 * 5 token. Symbols index descriptor syntaxKinds/fields/terminals respectively.
 * Node/token IDs are separate sequential streams; field IDs are zero.
 * Start records have end=0; finish records have start=0. Token ranges are
 * nonempty UTF-8 byte slices covering the source once in order. Rejected
 * syntax retains lossless tokens and publishes no partial node/field tree.
 * Contextual handles publish their complete product digest in grammar_sha256;
 * bind it to the generated contextual spec, not merely its base grammar.
 * Consumers must bind both digests, validate nesting and reserved positions,
 * and keep the result alive until all borrowed payload views are dropped.
 * Descriptor/error JSON is not a GPA1 record; check transport status first. */
/* Non-owner OS threads fail before entering the VM; initialization still
 * belongs to the embedding host. This predicate is safe after module init. */
int32_t gerbil_parser_language_is_owner_thread(void);
uint32_t gerbil_parser_language_abi_version(void);
void gerbil_parser_result_v2_init(gerbil_parser_result_v2 *result);
void gerbil_parser_result_v2_release(gerbil_parser_result_v2 *result);
int32_t gerbil_parser_language_descriptor(gerbil_parser_language_v2 language, gerbil_parser_result_v2 *result);
int32_t gerbil_parser_language_parse(gerbil_parser_language_v2 language, const uint8_t *source, size_t length, gerbil_parser_result_v2 *result);
int32_t gerbil_parser_language_release(gerbil_parser_language_v2 language);
#ifdef __cplusplus
}
#endif
#endif
