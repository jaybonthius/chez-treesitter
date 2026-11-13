SCHEME ?= scheme
CC ?= cc
CAP ?=
OBJ ?= build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --program
TEST_ENV ?=
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 900
BENCH_TIMEOUT ?= 3600
TESTS := $(sort $(wildcard tests/*-test.sps))

TREE_SITTER := vendor/tree-sitter/lib
SHARED_OBJECT := $(OBJ)/src/tree-sitter/libchez-tree-sitter.so
C_SOURCES := src/tree-sitter/shim.c $(TREE_SITTER)/src/lib.c
GRAMMARS := $(OBJ)/tests/lib/grammars
TEST_GRAMMARS := $(GRAMMARS)/libtree-sitter-json.so $(GRAMMARS)/libtree-sitter-python.so \
  $(GRAMMARS)/libtree-sitter-fake.so
C_BENCH := $(OBJ)/bench/c-bench
CFLAGS ?= -O2
# Hidden symbols keep tree-sitter's internals bound inside the shared
# object; its API stays visible because api.h marks it so.
SHARED_CFLAGS := -std=gnu11 -fPIC -shared -fvisibility=hidden -Wall -Wextra \
  -I$(TREE_SITTER)/include -I$(TREE_SITTER)/src
GRAMMAR_CFLAGS := -std=c11 -fPIC -shared
# tree-sitter copies empty arrays with memcpy from NULL, which reads no byte
# but which UBSan reports.
SANITIZE_CFLAGS := -O1 -g -fno-omit-frame-pointer -fsanitize=address,undefined \
  -fno-sanitize=nonnull-attribute -fno-sanitize-recover=all
# Chez itself is not built with the sanitizers, so their runtimes are
# preloaded; Chez frees its heap only at exit, so leaks are not reported.
SANITIZE_ENV := ASAN_OPTIONS=detect_leaks=0 \
  LD_PRELOAD=$(shell $(CC) -print-file-name=libasan.so):$(shell $(CC) -print-file-name=libubsan.so)

.PHONY: build test sanitize bench grammar clean $(TESTS:%=run/%)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build: $(SHARED_OBJECT) $(TEST_GRAMMARS)
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss $(OBJ)

$(SHARED_OBJECT): $(C_SOURCES) $(wildcard $(TREE_SITTER)/src/*.c $(TREE_SITTER)/src/*.h) \
    $(TREE_SITTER)/include/tree_sitter/api.h
	@mkdir -p $(@D)
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) $(CFLAGS) $(SHARED_CFLAGS) -o $@ $(C_SOURCES)

$(GRAMMARS)/libtree-sitter-%.so: vendor/tree-sitter-%/src/parser.c
	@mkdir -p $(@D)
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) $(CFLAGS) $(GRAMMAR_CFLAGS) -Ivendor/tree-sitter-$*/src \
	  -o $@ $^ $(wildcard vendor/tree-sitter-$*/src/scanner.c)

$(GRAMMARS)/libtree-sitter-fake.so: tests/fixtures/fake-grammar.c
	@mkdir -p $(@D)
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) $(CFLAGS) $(GRAMMAR_CFLAGS) -o $@ $<

grammar:
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) $(CFLAGS) $(GRAMMAR_CFLAGS) -I$(GRAMMAR)/src -o $(OUTPUT) \
	  $(GRAMMAR)/src/parser.c $(wildcard $(GRAMMAR)/src/scanner.c)

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@$(CAP) timeout $(TEST_TIMEOUT) env $(TEST_ENV) $(RUN) ./$(OBJ)/$(*:.sps=.so)

# A build of its own, so that the ordinary one is never left sanitized.
sanitize:
	@$(MAKE) --no-print-directory test OBJ=build/sanitize CFLAGS="$(SANITIZE_CFLAGS)" \
	  TEST_ENV="$(SANITIZE_ENV)"

$(C_BENCH): bench/bench.c $(TREE_SITTER)/src/lib.c vendor/tree-sitter-json/src/parser.c \
    vendor/tree-sitter-python/src/parser.c vendor/tree-sitter-python/src/scanner.c
	@mkdir -p $(@D) $(@D)/json $(@D)/python
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) -O2 -std=gnu11 -I$(TREE_SITTER)/include \
	  -I$(TREE_SITTER)/src -c -o $(@D)/lib.o $(TREE_SITTER)/src/lib.c
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) -O2 -std=c11 -Ivendor/tree-sitter-json/src \
	  -c -o $(@D)/json/parser.o vendor/tree-sitter-json/src/parser.c
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) -O2 -std=c11 -Ivendor/tree-sitter-python/src \
	  -c -o $(@D)/python/parser.o vendor/tree-sitter-python/src/parser.c
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) -O2 -std=c11 -Ivendor/tree-sitter-python/src \
	  -c -o $(@D)/python/scanner.o vendor/tree-sitter-python/src/scanner.c
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) -O2 -std=gnu11 -I$(TREE_SITTER)/include -o $@ \
	  bench/bench.c $(@D)/lib.o $(@D)/json/parser.o $(@D)/python/parser.o $(@D)/python/scanner.o

bench: build $(C_BENCH)
	$(CAP) timeout $(BENCH_TIMEOUT) $(RUN) ./$(OBJ)/bench/bench.so $(C_BENCH)

clean:
	rm -rf build
