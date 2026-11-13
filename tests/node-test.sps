(import (chezscheme) (tree-sitter) (check) (support))

(define (parse language source)
  (parser-parse (make-parser language) source))

(define (root language source)
  (tree-root-node (parse language source)))

(define (sexp node) (and node (node-sexp node)))

(define json-root (root (json) "[1,2,{\"name\": \"Bob\"},3]"))

(test "a node knows its type, flags and tree"
  (check-equal? "document" (node-type json-root))
  (check (node-named? json-root))
  (check (not (node-missing? json-root)))
  (check (not (node-extra? json-root)))
  (check (not (node-error? json-root)))
  (check (not (node-has-error? json-root)))
  (check (not (node-has-changes? json-root)))
  (check (tree? (node-tree json-root)))
  (check (eq? (json) (node-language json-root))))

(test "children, named children and their counts"
  (let ([array (node-child json-root 0)])
    (check-equal? "array" (node-type array))
    (check-equal? "(array (number) (number) (object (pair key: (string (string_content)) value: (string (string_content)))) (number))"
      (node-sexp array))
    (check-equal? 9 (node-child-count array))
    (check-equal? 4 (node-named-child-count array))
    (check-equal? '("[" "number" "," "number" "," "object" "," "number" "]")
      (map node-type (node-children array)))
    (check-equal? '("number" "number" "object" "number") (map node-type (node-named-children array)))
    (check-equal? #f (node-child array 9))
    (check-equal? #f (node-named-child array 4))
    (check-equal? "object" (node-type (node-named-child array 2)))
    (check-equal? '() (node-children (node-child array 0)))))

(test "fields name children"
  (let* ([object (node-named-child (node-child json-root 0) 2)]
         [pair (node-named-child object 0)])
    (check-equal? "pair" (node-type pair))
    (check-equal? "key" (node-field-name-for-child pair 0))
    (check-equal? #f (node-field-name-for-child pair 1))
    (check-equal? "value" (node-field-name-for-child pair 2))
    (check-equal? "value" (node-field-name-for-named-child pair 1))
    (check-equal? #f (node-field-name-for-child pair 7))
    (check-equal? "(string (string_content))" (sexp (node-child-by-field-name pair "key")))
    (check-equal? "\"Bob\"" (node-text (node-child-by-field-name pair "value")))
    (check-equal? (node-child-by-field-name pair "key")
      (node-child-by-field-id pair (language-field-id (json) "key")))
    (check-equal? #f (node-child-by-field-name object "key"))
    (check-raises assertion-violation? (node-child-by-field-name pair "nope"))
    (check-equal? '("\"Bob\"") (map node-text (node-children-by-field-name pair "value")))))

(test "a field can hold several children"
  (let* ([module (root (python) "from a import b, c\n")]
         [statement (node-child module 0)])
    (check-equal? "import_from_statement" (node-type statement))
    (check-equal? '("b" "c") (map node-text (node-children-by-field-name statement "name")))
    (check-equal? '("a") (map node-text (node-children-by-field-name statement "module_name")))
    (check-equal? '("b" "c")
      (map node-text
        (node-children-by-field-id statement (language-field-id (python) "name"))))))

(test "siblings, named or not"
  (let ([object (node-named-child (node-child json-root 0) 2)])
    (check-equal? "(number)" (sexp (node-next-named-sibling object)))
    (check-equal? "(\",\")" (sexp (node-next-sibling object)))
    (check-equal? "(number)" (sexp (node-previous-named-sibling object)))
    (check-equal? "(\",\")" (sexp (node-previous-sibling object)))
    (check-equal? #f (node-next-sibling json-root))
    (check-equal? #f (node-previous-sibling (node-child (node-child json-root 0) 0)))))

(test "the first child at or after a byte"
  (let ([array (node-child json-root 0)])
    (check-equal? "(number)" (sexp (node-first-named-child-for-byte array 3)))
    (check-equal? "(\",\")" (sexp (node-first-child-for-byte array 4)))
    (check-equal? #f (node-first-child-for-byte array 100))))

(test "the smallest node spanning a byte or point range"
  (check-equal? "(\"{\")" (sexp (node-descendant-for-byte-range json-root 5 6)))
  (check-equal? "(object (pair key: (string (string_content)) value: (string (string_content))))"
    (sexp (node-named-descendant-for-byte-range json-root 5 6)))
  (check-equal? "(\"{\")"
    (sexp (node-descendant-for-point-range json-root (make-point 0 5) (make-point 0 6))))
  (check-equal? "object"
    (node-type (node-named-descendant-for-point-range json-root (make-point 0 5) (make-point 0 6))))
  (check-raises assertion-violation? (node-descendant-for-byte-range json-root 6 5))
  (check-raises assertion-violation?
    (node-descendant-for-point-range json-root (make-point 0 6) 5)))

(test "parents lead to the root, and the child containing a descendant"
  (let* ([bob (node-named-descendant-for-byte-range json-root 15 18)]
         [chain (let loop ([n bob] [acc '()]) (if n (loop (node-parent n) (cons (node-type n) acc)) acc))])
    (check-equal? '("document" "array" "object" "pair" "string" "string_content") chain)
    (check-equal? #f (node-parent json-root))
    (check-equal? "array" (node-type (node-child-with-descendant json-root bob)))
    (check-equal? "object"
      (node-type (node-child-with-descendant (node-child json-root 0) bob)))))

(test "positions are bytes, and points rows and byte columns"
  (let* ([source "{\"é\": [1,\n  2]}"]
         [root (root (json) source)]
         [two (node-named-descendant-for-byte-range root 13 14)])
    (check-equal? "2" (node-text two))
    (check-equal? 13 (node-start-byte two))
    (check-equal? 14 (node-end-byte two))
    (check-equal? (make-point 1 2) (node-start-point two))
    (check-equal? (make-point 1 3) (node-end-point two))
    (check-equal? (make-point 1 5) (node-end-point root))
    (check-equal? (bytevector-length (string->utf8 source)) (node-end-byte root))
    (check-equal? "\"é\"" (node-text (node-child-by-field-name (node-named-child (node-child root 0) 0) "key")))))

(test "missing, extra and error nodes"
  (let* ([root (root (json) "[1, 2")] [array (node-child root 0)])
    (check (node-has-error? root))
    (let ([missing (node-child array (- (node-child-count array) 1))])
      (check (node-missing? missing))
      (check-equal? "]" (node-type missing))
      (check-equal? (node-end-byte missing) (node-start-byte missing))))
  (let* ([root (root (python) "x = 1 # note\n")] [comment (node-named-descendant-for-byte-range root 6 12)])
    (check-equal? "comment" (node-type comment))
    (check (node-extra? comment)))
  (let* ([root (root (json) "[1, @]")] [bad (find node-error? (preorder-nodes root))])
    (check bad)
    (check-equal? "ERROR" (node-type bad))
    (check (node-has-error? root))))

(test "symbols, grammar types and descendant counts"
  (let ([array (node-child json-root 0)])
    (check-equal? "array" (language-symbol-name (json) (node-symbol array)))
    (check-equal? (node-type array) (node-grammar-type array))
    (check-equal? (node-symbol array) (node-grammar-symbol array))
    (check-equal? (length (preorder-nodes array)) (node-descendant-count array))
    (check (fixnum? (node-parse-state array)))))

(test "an aliased node has its alias as its type and its rule as its grammar type"
  (let ([aliased (find (lambda (n) (not (string=? (node-type n) (node-grammar-type n))))
                   (preorder-nodes
                     (root (python) "print(f'{x!r:>10}')\nmatch y:\n    case [a, *b]: pass\n")))])
    (check aliased)
    (check (not (= (node-symbol aliased) (node-grammar-symbol aliased))))))

(test "nodes compare by what they denote"
  (let ([a (node-child json-root 0)] [b (node-child json-root 0)])
    (check (not (eq? a b)))
    (check (node=? a b))
    (check (equal? a b))
    (check-equal? (equal-hash a) (equal-hash b))
    (check (not (node=? a json-root)))
    (let ([table (make-hashtable equal-hash equal?)])
      (hashtable-set! table a 'array)
      (check-equal? 'array (hashtable-ref table b #f))))
  (let ([other (root (json) "[1,2,{\"name\": \"Bob\"},3]")])
    (check (not (node=? json-root other)))))

(test "a node prints its type and bytes"
  (check-equal? "#<node document 0-23>" (format "~s" json-root)))

(test "the S-expression from tree-sitter matches the one built from this library"
  (for-each
    (lambda (name)
      (let ([root (root (python) (python-example name))])
        (check (not (node-has-error? root)))
        (check-equal? (node-sexp root) (sexp-of root))))
    '("python3-grammar.py" "python3.8_grammar.py" "tabs.py" "multiple-newlines.py"))
  (check-equal? (node-sexp json-root) (sexp-of json-root)))

(test "points, ranges and edits are values"
  (check (equal? (make-point 1 2) (make-point 1 2)))
  (check (not (equal? (make-point 1 2) (make-point 2 1))))
  (check-equal? (equal-hash (make-point 3 4)) (equal-hash (make-point 3 4)))
  (check (equal? (make-range 0 5 (make-point 0 0) (make-point 0 5))
                 (make-range 0 5 (make-point 0 0) (make-point 0 5))))
  (check-raises assertion-violation? (make-point -1 0))
  (check-raises assertion-violation? (make-point 0 #x100000000))
  (check-raises assertion-violation? (make-range 5 0 (make-point 0 0) (make-point 0 0)))
  (check-raises assertion-violation? (make-range 0 5 '(0 . 0) (make-point 0 0)))
  (check-raises assertion-violation?
    (make-input-edit 5 4 6 (make-point 0 5) (make-point 0 4) (make-point 0 6)))
  (check-raises assertion-violation? (make-input-edit 0 1 1 #f #f #f)))

(test "node procedures check their arguments"
  (check-raises assertion-violation? (node-type 'node))
  (check-raises assertion-violation? (node-child json-root -1))
  (check-raises assertion-violation? (node-child json-root 'first))
  (check-raises assertion-violation? (node-child-by-field-name json-root 'key))
  (check-raises assertion-violation? (node=? json-root 'node))
  (check-raises assertion-violation? (node-tree 'node))
  (check-equal? 'node-language
    (guard (c [#t (condition-who c)]) (node-language 'node)))
  (check-raises assertion-violation?
    (node-child-with-descendant json-root (node-child (root (json) "[1,2,{\"name\": \"Bob\"},3]") 0))))

(test-exit)
