# chez-treesitter

tree-sitter for threaded Chez Scheme 10.4.1: grammars loaded from shared objects, parsers, immutable trees and their nodes, cursors, incremental reparsing, and queries with their predicates.

```sh
git submodule update --init
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme sanitize
make SCHEME=/path/to/chez-10.4.1/scheme bench
make grammar GRAMMAR=path/to/tree-sitter-x OUTPUT=libtree-sitter-x.so
```

tree-sitter (MIT) is the submodule `vendor/tree-sitter`, unmodified, pinned to v0.27.2, `3fb7be49145acfb07e33b61ed5689e2b6771b2e9`. The tests and benchmarks also use tree-sitter-json v0.24.8, `ee35a6ebefcef0c5c416c0d1ccec7370cfca5a24`, and tree-sitter-python v0.25.0, `293fdc02038ee2bf0e2e206711b69c90ac0d413f` (both MIT, unmodified submodules).
