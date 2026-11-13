#define _GNU_SOURCE
#include <dlfcn.h>
#include <link.h>
#include <pthread.h>
#include <regex.h>
#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#include "tree_sitter/api.h"

#define EXPORT __attribute__((visibility("default")))

/* Nodes cross from Scheme as the bytes of a TSNode in a bytevector; memcpy
   keeps the access legal whatever the bytevector's alignment. */
static inline TSNode node_in(const void *p) {
  TSNode n;
  memcpy(&n, p, sizeof n);
  return n;
}

static inline int node_out(TSNode n, void *p) {
  if (ts_node_is_null(n)) return 0;
  memcpy(p, &n, sizeof n);
  return 1;
}

/* The collector cannot see memory tree-sitter allocates, so a program that
   drops large trees and allocates little Scheme data would never collect
   them. Counting what tree-sitter holds lets Scheme ask for a collection. */

static _Atomic uint64_t live_bytes;

typedef union {
  size_t size;
  max_align_t align;
} header;

static void out_of_memory(size_t size) {
  fprintf(stderr, "tree-sitter failed to allocate %zu bytes\n", size);
  abort();
}

static void *counted_malloc(size_t size) {
  if (size > SIZE_MAX - sizeof(header)) out_of_memory(size);
  header *h = malloc(sizeof(header) + size);
  if (!h) out_of_memory(size);
  h->size = size;
  atomic_fetch_add_explicit(&live_bytes, size, memory_order_relaxed);
  return h + 1;
}

static void *counted_calloc(size_t count, size_t size) {
  if (size != 0 && count > (SIZE_MAX - sizeof(header)) / size) out_of_memory(SIZE_MAX);
  size_t total = count * size;
  header *h = calloc(1, sizeof(header) + total);
  if (!h) out_of_memory(total);
  h->size = total;
  atomic_fetch_add_explicit(&live_bytes, total, memory_order_relaxed);
  return h + 1;
}

static void counted_free(void *p) {
  if (!p) return;
  header *h = (header *)p - 1;
  atomic_fetch_sub_explicit(&live_bytes, h->size, memory_order_relaxed);
  free(h);
}

static void *counted_realloc(void *p, size_t size) {
  if (!p) return counted_malloc(size);
  if (size > SIZE_MAX - sizeof(header)) out_of_memory(size);
  header *old = (header *)p - 1;
  size_t old_size = old->size;
  header *h = realloc(old, sizeof(header) + size);
  if (!h) out_of_memory(size);
  h->size = size;
  atomic_fetch_add_explicit(&live_bytes, size, memory_order_relaxed);
  atomic_fetch_sub_explicit(&live_bytes, old_size, memory_order_relaxed);
  return h + 1;
}

/* The allocator must be in place before tree-sitter allocates anything, so
   it is installed when the shared object is loaded. */
__attribute__((constructor)) static void install_allocator(void) {
  ts_set_allocator(counted_malloc, counted_calloc, counted_realloc, counted_free);
}

EXPORT uint64_t chez_ts_live_bytes(void) {
  return atomic_load_explicit(&live_bytes, memory_order_relaxed);
}

EXPORT void chez_ts_free(void *p) { counted_free(p); }

EXPORT void chez_ts_copy_bytes(const void *from, void *to, size_t size) { memcpy(to, from, size); }

EXPORT uint32_t chez_ts_layout(uint32_t which) {
  switch (which) {
    case 0: return sizeof(TSNode);
    case 1: return sizeof(TSPoint);
    case 2: return offsetof(TSPoint, row);
    case 3: return offsetof(TSPoint, column);
    case 4: return sizeof(TSRange);
    case 5: return offsetof(TSRange, start_point);
    case 6: return offsetof(TSRange, end_point);
    case 7: return offsetof(TSRange, start_byte);
    case 8: return offsetof(TSRange, end_byte);
    case 9: return sizeof(TSInputEdit);
    case 10: return offsetof(TSInputEdit, start_byte);
    case 11: return offsetof(TSInputEdit, old_end_byte);
    case 12: return offsetof(TSInputEdit, new_end_byte);
    case 13: return offsetof(TSInputEdit, start_point);
    case 14: return offsetof(TSInputEdit, old_end_point);
    case 15: return offsetof(TSInputEdit, new_end_point);
    case 16: return sizeof(TSQueryPredicateStep);
    case 17: return offsetof(TSQueryPredicateStep, type);
    case 18: return offsetof(TSQueryPredicateStep, value_id);
    case 19: return sizeof(TSLanguageMetadata);
    case 20: return offsetof(TSLanguageMetadata, major_version);
    case 21: return offsetof(TSLanguageMetadata, minor_version);
    case 22: return offsetof(TSLanguageMetadata, patch_version);
    case 23: return sizeof(TSSymbol);
    case 24: return sizeof(TSQueryPredicateStepType);
    case 25: return TREE_SITTER_LANGUAGE_VERSION;
    case 26: return TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION;
    case 27: return TSQueryPredicateStepTypeDone;
    case 28: return TSQueryPredicateStepTypeCapture;
    case 29: return TSQueryPredicateStepTypeString;
    case 30: return offsetof(TSNode, id);
    case 31: return offsetof(TSNode, tree);
    case 32: return sizeof(TSQueryError);
    default: return UINT32_MAX;
  }
}

/* glibc's dlopen hands back the object already loaded under a name even
   after the file at that name is rebuilt, which would silently give the old
   grammar; each loaded grammar's file identity tells the two apart. */
typedef struct loaded_grammar {
  void *handle;
  dev_t device;
  ino_t inode;
  struct timespec modified;
  struct loaded_grammar *next;
} loaded_grammar;

static loaded_grammar *loaded_grammars;
static pthread_mutex_t loaded_grammars_lock = PTHREAD_MUTEX_INITIALIZER;

static int grammar_file(void *handle, struct stat *file) {
  struct link_map *map;
  return dlinfo(handle, RTLD_DI_LINKMAP, &map) == 0 && stat(map->l_name, file) == 0;
}

static int same_file(const loaded_grammar *g, const struct stat *file) {
  return g->device == file->st_dev && g->inode == file->st_ino &&
         g->modified.tv_sec == file->st_mtim.tv_sec &&
         g->modified.tv_nsec == file->st_mtim.tv_nsec;
}

static loaded_grammar *find_grammar(void *handle) {
  loaded_grammar *g = loaded_grammars;
  while (g && g->handle != handle) g = g->next;
  return g;
}

static int grammar_changed(void *handle) {
  struct stat file;
  int known = grammar_file(handle, &file);
  pthread_mutex_lock(&loaded_grammars_lock);
  loaded_grammar *g = find_grammar(handle);
  int changed = g && !(known && same_file(g, &file));
  pthread_mutex_unlock(&loaded_grammars_lock);
  return changed;
}

/* Only once a language is returned: a handle closed after a failed load can
   be reused for another object. Plain malloc, since the record lives as long
   as the process and is not tree-sitter's memory. */
static void remember_grammar(void *handle) {
  struct stat file;
  if (!grammar_file(handle, &file)) return;
  pthread_mutex_lock(&loaded_grammars_lock);
  if (!find_grammar(handle)) {
    loaded_grammar *g = malloc(sizeof *g);
    if (g) {
      g->handle = handle;
      g->device = file.st_dev;
      g->inode = file.st_ino;
      g->modified = file.st_mtim;
      g->next = loaded_grammars;
      loaded_grammars = g;
    }
  }
  pthread_mutex_unlock(&loaded_grammars_lock);
}

/* RTLD_LOCAL and dlsym on the grammar's own handle, so that two grammars
   exporting the same name, or two builds of one grammar, never resolve to
   each other's symbols. The handle is never closed once a language is
   returned: a language's tables must outlive every tree made with it. */
EXPORT const TSLanguage *chez_ts_language_load(const char *path, const char *symbol,
                                               char *error, uint32_t error_size) {
  void *handle = dlopen(path, RTLD_NOW | RTLD_LOCAL);
  if (!handle) {
    snprintf(error, error_size, "%s", dlerror());
    return NULL;
  }
  if (grammar_changed(handle)) {
    snprintf(error, error_size,
             "the file has changed since it was loaded; load the rebuilt grammar from a new path");
    dlclose(handle);
    return NULL;
  }
  dlerror();
  const TSLanguage *(*language)(void) = (const TSLanguage *(*)(void))dlsym(handle, symbol);
  const char *message = dlerror();
  if (message || !language) {
    snprintf(error, error_size, "%s", message ? message : "the symbol is NULL");
    dlclose(handle);
    return NULL;
  }
  const TSLanguage *result = language();
  if (result) {
    remember_grammar(handle);
  } else {
    snprintf(error, error_size, "%s returned NULL", symbol);
  }
  return result;
}

EXPORT uint32_t chez_ts_language_lookahead(const TSLanguage *language, TSStateId state,
                                           TSSymbol *out, uint32_t capacity) {
  TSLookaheadIterator *it = ts_lookahead_iterator_new(language, state);
  if (!it) return 0;
  uint32_t count = 0;
  while (ts_lookahead_iterator_next(it)) {
    if (count < capacity) out[count] = ts_lookahead_iterator_current_symbol(it);
    count++;
  }
  ts_lookahead_iterator_delete(it);
  return count;
}

typedef struct {
  const char *bytes;
  uint32_t length;
} string_input;

static const char *read_string(void *payload, uint32_t byte, TSPoint point, uint32_t *bytes_read) {
  (void)point;
  string_input *input = payload;
  if (byte >= input->length) {
    *bytes_read = 0;
    return "";
  }
  *bytes_read = input->length - byte;
  return input->bytes + byte;
}

typedef const char *(*scheme_reader)(uint32_t byte, uint32_t row, uint32_t column,
                                     uint32_t *bytes_read);

static const char *read_scheme(void *payload, uint32_t byte, TSPoint point, uint32_t *bytes_read) {
  return ((scheme_reader)payload)(byte, point.row, point.column, bytes_read);
}

typedef int (*scheme_progress)(uint32_t byte, int has_error);

static bool report_progress(TSParseState *state) {
  return !((scheme_progress)state->payload)(state->current_byte_offset, state->has_error);
}

static TSTree *parse(TSParser *parser, const TSTree *old, TSInput input, scheme_progress progress) {
  if (!progress) return ts_parser_parse(parser, old, input);
  TSParseOptions options = {.payload = (void *)progress, .progress_callback = report_progress};
  return ts_parser_parse_with_options(parser, old, input, options);
}

EXPORT TSTree *chez_ts_parse_bytes(TSParser *parser, const TSTree *old, const char *bytes,
                                   uint32_t length, scheme_progress progress) {
  string_input payload = {bytes, length};
  TSInput input = {&payload, read_string, TSInputEncodingUTF8, NULL};
  return parse(parser, old, input, progress);
}

EXPORT TSTree *chez_ts_parse_reader(TSParser *parser, const TSTree *old, scheme_reader reader,
                                    scheme_progress progress) {
  TSInput input = {(void *)reader, read_scheme, TSInputEncodingUTF8, NULL};
  return parse(parser, old, input, progress);
}

typedef void (*scheme_logger)(int type, const char *message);

static void log_scheme(void *payload, TSLogType type, const char *message) {
  ((scheme_logger)payload)(type == TSLogTypeLex, message);
}

EXPORT void chez_ts_parser_set_logger(TSParser *parser, scheme_logger logger) {
  TSLogger value = {(void *)logger, logger ? log_scheme : NULL};
  ts_parser_set_logger(parser, value);
}

EXPORT void chez_ts_tree_root_node(const TSTree *tree, void *out) {
  node_out(ts_tree_root_node(tree), out);
}

EXPORT TSTree *chez_ts_tree_edited_copy(const TSTree *tree, const TSInputEdit *edit) {
  TSTree *copy = ts_tree_copy(tree);
  TSInputEdit e;
  memcpy(&e, edit, sizeof e);
  ts_tree_edit(copy, &e);
  return copy;
}

#define NODE_GETTER(type, name) \
  EXPORT type chez_ts_node_##name(const void *n) { return ts_node_##name(node_in(n)); }

NODE_GETTER(TSSymbol, symbol)
NODE_GETTER(TSSymbol, grammar_symbol)
NODE_GETTER(uint32_t, start_byte)
NODE_GETTER(uint32_t, end_byte)
NODE_GETTER(bool, is_named)
NODE_GETTER(bool, is_missing)
NODE_GETTER(bool, is_extra)
NODE_GETTER(bool, is_error)
NODE_GETTER(bool, has_changes)
NODE_GETTER(bool, has_error)
NODE_GETTER(TSStateId, parse_state)
NODE_GETTER(TSStateId, next_parse_state)
NODE_GETTER(uint32_t, child_count)
NODE_GETTER(uint32_t, named_child_count)
NODE_GETTER(uint32_t, descendant_count)
NODE_GETTER(const char *, type)

static inline uint64_t point_bits(TSPoint p) { return (uint64_t)p.row << 32 | p.column; }

EXPORT uint64_t chez_ts_node_start_point(const void *n) {
  return point_bits(ts_node_start_point(node_in(n)));
}

EXPORT uint64_t chez_ts_node_end_point(const void *n) {
  return point_bits(ts_node_end_point(node_in(n)));
}

#define NODE_STEP(name) \
  EXPORT int chez_ts_node_##name(const void *n, void *out) { \
    return node_out(ts_node_##name(node_in(n)), out); \
  }

NODE_STEP(parent)
NODE_STEP(next_sibling)
NODE_STEP(prev_sibling)
NODE_STEP(next_named_sibling)
NODE_STEP(prev_named_sibling)

#define NODE_STEP_U32(name) \
  EXPORT int chez_ts_node_##name(const void *n, uint32_t i, void *out) { \
    return node_out(ts_node_##name(node_in(n), i), out); \
  }

NODE_STEP_U32(child)
NODE_STEP_U32(named_child)
NODE_STEP_U32(first_child_for_byte)
NODE_STEP_U32(first_named_child_for_byte)

EXPORT int chez_ts_node_child_by_field_id(const void *n, TSFieldId id, void *out) {
  return node_out(ts_node_child_by_field_id(node_in(n), id), out);
}

EXPORT int chez_ts_node_child_with_descendant(const void *n, const void *d, void *out) {
  return node_out(ts_node_child_with_descendant(node_in(n), node_in(d)), out);
}

EXPORT int chez_ts_node_descendant_for_byte_range(const void *n, uint32_t start, uint32_t end,
                                                  int named, void *out) {
  TSNode self = node_in(n);
  return node_out(named ? ts_node_named_descendant_for_byte_range(self, start, end)
                        : ts_node_descendant_for_byte_range(self, start, end),
                  out);
}

EXPORT int chez_ts_node_descendant_for_point_range(const void *n, uint32_t start_row,
                                                   uint32_t start_column, uint32_t end_row,
                                                   uint32_t end_column, int named, void *out) {
  TSNode self = node_in(n);
  TSPoint start = {start_row, start_column}, end = {end_row, end_column};
  return node_out(named ? ts_node_named_descendant_for_point_range(self, start, end)
                        : ts_node_descendant_for_point_range(self, start, end),
                  out);
}

/* tree-sitter answers with a pointer into the language's field names, so
   the id is found by comparing pointers. */
static TSFieldId field_id_for_name(const TSLanguage *language, const char *name) {
  if (!name) return 0;
  uint32_t count = ts_language_field_count(language);
  for (uint32_t id = 1; id <= count; id++) {
    if (ts_language_field_name_for_id(language, id) == name) return id;
  }
  return 0;
}

EXPORT TSFieldId chez_ts_node_field_id_for_child(const void *n, int named, uint32_t i) {
  TSNode self = node_in(n);
  return field_id_for_name(ts_node_language(self),
                           named ? ts_node_field_name_for_named_child(self, i)
                                 : ts_node_field_name_for_child(self, i));
}

EXPORT int chez_ts_node_eq(const void *a, const void *b) {
  return ts_node_eq(node_in(a), node_in(b));
}

EXPORT char *chez_ts_node_string(const void *n, size_t *length) {
  char *s = ts_node_string(node_in(n));
  *length = strlen(s);
  return s;
}

/* A cursor, since ts_node_child is linear in the index and a loop over it
   quadratic. Field ids start at 1, so 0 selects every child. */
EXPORT uint32_t chez_ts_node_children(const void *n, int named, TSFieldId field, void *out,
                                      uint32_t capacity) {
  uint32_t count = 0;
  TSTreeCursor cursor = ts_tree_cursor_new(node_in(n));
  if (ts_tree_cursor_goto_first_child(&cursor)) {
    do {
      if (field && ts_tree_cursor_current_field_id(&cursor) != field) continue;
      TSNode child = ts_tree_cursor_current_node(&cursor);
      if (named && !ts_node_is_named(child)) continue;
      if (count < capacity) memcpy((char *)out + count * sizeof(TSNode), &child, sizeof child);
      count++;
    } while (ts_tree_cursor_goto_next_sibling(&cursor));
  }
  ts_tree_cursor_delete(&cursor);
  return count;
}

EXPORT TSTreeCursor *chez_ts_tree_cursor_new(const void *n) {
  TSTreeCursor *c = counted_malloc(sizeof *c);
  *c = ts_tree_cursor_new(node_in(n));
  return c;
}

EXPORT TSTreeCursor *chez_ts_tree_cursor_copy(const TSTreeCursor *c) {
  TSTreeCursor *copy = counted_malloc(sizeof *copy);
  *copy = ts_tree_cursor_copy(c);
  return copy;
}

EXPORT void chez_ts_tree_cursor_delete(TSTreeCursor *c) {
  ts_tree_cursor_delete(c);
  counted_free(c);
}

EXPORT void chez_ts_tree_cursor_reset(TSTreeCursor *c, const void *n) {
  ts_tree_cursor_reset(c, node_in(n));
}

EXPORT void chez_ts_tree_cursor_current_node(const TSTreeCursor *c, void *out) {
  node_out(ts_tree_cursor_current_node(c), out);
}

EXPORT int64_t chez_ts_tree_cursor_goto_first_child_for_point(TSTreeCursor *c, uint32_t row,
                                                              uint32_t column) {
  TSPoint p = {row, column};
  return ts_tree_cursor_goto_first_child_for_point(c, p);
}

EXPORT TSQuery *chez_ts_query_copy_without(const TSQuery *query, int pattern, uint32_t index,
                                           const char *capture, uint32_t length) {
  TSQuery *copy = ts_query_copy(query);
  if (pattern) {
    ts_query_disable_pattern(copy, index);
  } else {
    ts_query_disable_capture(copy, capture, length);
  }
  return copy;
}

typedef struct {
  TSQueryCursor *cursor;
  TSQueryMatch match;
} query_cursor;

EXPORT query_cursor *chez_ts_query_cursor_new(void) {
  query_cursor *c = counted_malloc(sizeof *c);
  c->cursor = ts_query_cursor_new();
  memset(&c->match, 0, sizeof c->match);
  return c;
}

EXPORT void chez_ts_query_cursor_delete(query_cursor *c) {
  ts_query_cursor_delete(c->cursor);
  counted_free(c);
}

EXPORT void chez_ts_query_cursor_exec(query_cursor *c, const TSQuery *query, const void *n) {
  memset(&c->match, 0, sizeof c->match);
  ts_query_cursor_exec(c->cursor, query, node_in(n));
}

EXPORT void chez_ts_query_cursor_set_match_limit(query_cursor *c, uint32_t limit) {
  ts_query_cursor_set_match_limit(c->cursor, limit);
}

EXPORT uint32_t chez_ts_query_cursor_match_limit(query_cursor *c) {
  return ts_query_cursor_match_limit(c->cursor);
}

EXPORT int chez_ts_query_cursor_did_exceed_match_limit(query_cursor *c) {
  return ts_query_cursor_did_exceed_match_limit(c->cursor);
}

EXPORT void chez_ts_query_cursor_set_max_start_depth(query_cursor *c, uint32_t depth) {
  ts_query_cursor_set_max_start_depth(c->cursor, depth);
}

EXPORT int chez_ts_query_cursor_set_byte_range(query_cursor *c, int containing, uint32_t start,
                                               uint32_t end) {
  return containing ? ts_query_cursor_set_containing_byte_range(c->cursor, start, end)
                    : ts_query_cursor_set_byte_range(c->cursor, start, end);
}

EXPORT int chez_ts_query_cursor_set_point_range(query_cursor *c, int containing, uint32_t start_row,
                                                uint32_t start_column, uint32_t end_row,
                                                uint32_t end_column) {
  TSPoint start = {start_row, start_column}, end = {end_row, end_column};
  return containing ? ts_query_cursor_set_containing_point_range(c->cursor, start, end)
                    : ts_query_cursor_set_point_range(c->cursor, start, end);
}

EXPORT int chez_ts_query_cursor_next(query_cursor *c, int captures, uint32_t *out) {
  uint32_t capture_index = 0;
  bool found = captures ? ts_query_cursor_next_capture(c->cursor, &c->match, &capture_index)
                        : ts_query_cursor_next_match(c->cursor, &c->match);
  if (!found) {
    memset(&c->match, 0, sizeof c->match);
    return 0;
  }
  out[0] = c->match.id;
  out[1] = c->match.pattern_index;
  out[2] = c->match.capture_count;
  out[3] = capture_index;
  return 1;
}

EXPORT uint32_t chez_ts_query_cursor_capture(query_cursor *c, uint32_t i, void *out) {
  memcpy(out, &c->match.captures[i].node, sizeof(TSNode));
  return c->match.captures[i].index;
}

EXPORT void chez_ts_query_cursor_remove_match(query_cursor *c, uint32_t id) {
  ts_query_cursor_remove_match(c->cursor, id);
}

EXPORT regex_t *chez_ts_regex_new(const char *pattern, char *error, uint32_t error_size) {
  regex_t *re = malloc(sizeof *re);
  if (!re) {
    snprintf(error, error_size, "out of memory");
    return NULL;
  }
  int status = regcomp(re, pattern, REG_EXTENDED | REG_NOSUB);
  if (status != 0) {
    regerror(status, re, error, error_size);
    free(re);
    return NULL;
  }
  return re;
}

EXPORT void chez_ts_regex_free(regex_t *re) {
  regfree(re);
  free(re);
}

/* The text starts at the node, so that ^ anchors there rather than at the
   start of the whole source. */
EXPORT int chez_ts_regex_search(const regex_t *re, const char *bytes, uint32_t start,
                                uint32_t end) {
  regmatch_t range = {.rm_so = 0, .rm_eo = end - start};
  return regexec(re, bytes + start, 1, &range, REG_STARTEND) == 0;
}
