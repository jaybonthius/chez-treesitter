(import (chezscheme) (tree-sitter) (check) (support))

(define layout (foreign-procedure "chez_ts_layout" (unsigned-32) unsigned-32))

(test "the C structs have the layouts the library reads"
  (check-equal? 32 (layout 0))
  (check-equal? 8 (layout 1))
  (check-equal? 24 (layout 4))
  (check-equal? 36 (layout 9))
  (check-equal? 8 (layout 16))
  (check-equal? 3 (layout 19)))

(test "the supported ABI versions are tree-sitter's"
  (check-equal? 13 (layout 26))
  (check-equal? 15 (layout 25)))

(test "ranges cross to C and back field by field"
  (let ([parser (make-parser (json))]
        [ranges (list (make-range 1 2 (make-point 3 4) (make-point 5 6))
                      (make-range 7 8 (make-point 9 10) (make-point 11 12)))])
    (parser-included-ranges-set! parser ranges)
    (check-equal? ranges (parser-included-ranges parser))))

(test "an edit crosses to C field by field"
  (let* ([tree (parser-parse (make-parser (json)) "[1,\n2]")]
         [edited (tree-edit tree (make-input-edit 0 1 4 (make-point 0 0) (make-point 0 1)
                                   (make-point 1 2)))]
         [root (tree-root-node edited)])
    (check-equal? 9 (node-end-byte root))
    (check-equal? (make-point 2 2) (node-end-point root))
    (check-equal? 7 (node-start-byte (node-named-child (node-child root 0) 1)))
    (check-equal? (make-point 2 0) (node-start-point (node-named-child (node-child root 0) 1)))))

(test "language metadata is the grammar's version"
  (let ([manifest (utf8->string (read-file "vendor/tree-sitter-python/tree-sitter.json"))])
    (check (string-contains? manifest (format "\"version\": ~s" (language-version (python)))))))

(test "node types and fields agree with tree-sitter's own S-expressions"
  (for-each
    (lambda (name)
      (let ([root (tree-root-node (parser-parse (make-parser (python)) (python-example name)))])
        (check-equal? (node-sexp root) (sexp-of root))))
    '("python3-grammar.py" "python3.8_grammar.py" "python2-grammar.py")))

(test "the grammars' ABI versions are inside the supported range"
  (check (<= 13 (language-abi-version (json)) 15))
  (check (<= 13 (language-abi-version (python)) 15)))

(test-exit)
