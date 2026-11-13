#include <stdint.h>

/* A language's ABI version is its first field, and the only one read before
   the version is accepted. */
static const uint32_t old_language[64] = {12};
static const uint32_t future_language[64] = {16};

const void *tree_sitter_old(void) { return old_language; }
const void *tree_sitter_future(void) { return future_language; }
const void *tree_sitter_null(void) { return 0; }
