(import (chezscheme) (tree-sitter) (check) (support))

(define (raised thunk)
  (guard (c [#t c]) (thunk) #f))

(test "a grammar loads with its name, path and versions"
  (check-equal? "json" (language-name (json)))
  (check-equal? (grammar-path "json") (language-path (json)))
  (check-equal? 14 (language-abi-version (json)))
  (check-equal? #f (language-version (json)))
  (check-equal? "python" (language-name (python)))
  (check-equal? 15 (language-abi-version (python)))
  (check-equal? "0.25.0" (language-version (python))))

(test "a grammar loaded twice parses alike"
  (let ([again (load-language (grammar-path "json") "json")])
    (check (not (eq? again (json))))
    (check-equal? (node-sexp (tree-root-node (parser-parse (make-parser (json)) "[1]")))
      (node-sexp (tree-root-node (parser-parse (make-parser again) "[1]"))))))

(test "a missing file raises a load error naming the path and symbol"
  (let ([c (raised (lambda () (load-language "/nonexistent/libtree-sitter-x.so" "x")))])
    (check (language-load-error? c))
    (check (not (language-version-error? c)))
    (check-equal? "/nonexistent/libtree-sitter-x.so" (language-load-error-path c))
    (check-equal? "tree_sitter_x" (language-load-error-symbol c))
    (check (string-contains? (condition-message c) "/nonexistent/libtree-sitter-x.so"))
    (check-equal? 'load-language (condition-who c))))

(test "a grammar without the symbol raises a load error"
  (let ([c (raised (lambda () (load-language (grammar-path "json") "python")))])
    (check (language-load-error? c))
    (check-equal? "tree_sitter_python" (language-load-error-symbol c))
    (check (string-contains? (condition-message c) "tree_sitter_python"))))

(test "a language function returning NULL raises a load error"
  (check-raises language-load-error? (load-language (grammar-path "fake") "null")))

(test "an unsupported ABI version raises a version error"
  (for-each
    (lambda (name version)
      (let ([c (raised (lambda () (load-language (grammar-path "fake") name)))])
        (check (language-version-error? c))
        (check (language-load-error? c))
        (check-equal? version (language-version-error-version c))
        (check-equal? (string-append "tree_sitter_" name) (language-load-error-symbol c))))
    '("old" "future")
    '(12 16)))

(define (write-file path bytes)
  (call-with-port (open-file-output-port path (file-options no-fail))
    (lambda (port) (put-bytevector port bytes))))

(test "a grammar rebuilt at a path it was loaded from raises a load error"
  (let* ([directory (path-parent (grammar-path "json"))]
         [path (path-build directory "rebuilt.so")]
         [staged (path-build directory "rebuilt-staged.so")]
         [moved (path-build directory "rebuilt-moved.so")])
    (write-file path (read-file (grammar-path "json")))
    (check-equal? "json" (language-name (load-language path "json")))
    (check-equal? "json" (language-name (load-language path "json")))
    (write-file staged (read-file (grammar-path "python")))
    (rename-file staged path)
    (for-each
      (lambda (name)
        (let ([c (raised (lambda () (load-language path name)))])
          (check (language-load-error? c))
          (check-equal? path (language-load-error-path c))
          (check (string-contains? (condition-message c) "changed since it was loaded"))))
      '("json" "python"))
    (rename-file path moved)
    (check-equal? "python" (language-name (load-language moved "python")))
    (delete-file moved)))

(test "load-language checks its arguments"
  (check-raises assertion-violation? (load-language 'json "json"))
  (check-raises assertion-violation? (load-language (grammar-path "json") 'json))
  (check-raises assertion-violation? (load-language (grammar-path "json") "c-sharp"))
  (check-raises assertion-violation? (load-language (grammar-path "json") "")))

(test "symbols map to names and types both ways"
  (let* ([python (python)]
         [identifier (language-named-symbol python "identifier")]
         [def (language-anonymous-symbol python "def")])
    (check (fixnum? identifier))
    (check-equal? "identifier" (language-symbol-name python identifier))
    (check-equal? 'regular (language-symbol-type python identifier))
    (check-equal? "def" (language-symbol-name python def))
    (check-equal? 'anonymous (language-symbol-type python def))
    (check-equal? #f (language-named-symbol python "no_such_symbol"))
    (check-equal? #f (language-anonymous-symbol python "identifier"))
    (check-equal? "ERROR" (language-symbol-name python #xffff))
    (check-equal? "_ERROR" (language-symbol-name python #xfffe))
    (check-raises assertion-violation? (language-symbol-name python (language-symbol-count python)))
    (check-raises assertion-violation? (language-symbol-type python 60000))
    (check (> (language-symbol-count python) 100))
    (check (> (language-state-count python) 1000))
    (check (exists (lambda (s) (eq? (language-symbol-type python s) 'auxiliary))
             (iota (language-symbol-count python))))))

(test "symbol names are shared and immutable"
  (let ([name (language-symbol-name (json) 1)])
    (check (eq? name (language-symbol-name (json) 1)))
    (check-raises assertion-violation? (string-set! name 0 #\x))))

(test "supertypes list their subtypes"
  (let* ([python (python)] [supertypes (language-supertypes python)])
    (check (pair? supertypes))
    (check (for-all (lambda (s) (eq? 'supertype (language-symbol-type python s))) supertypes))
    (let ([expression (find (lambda (s) (string=? "expression" (language-symbol-name python s)))
                        supertypes)])
      (check expression)
      (check (member "primary_expression"
               (map (lambda (s) (language-symbol-name python s))
                 (language-subtypes python expression)))))
    (check-equal? '() (language-supertypes (json)))
    (check-equal? '() (language-subtypes (json) 1))))

(test "fields map to names both ways"
  (let ([json (json)])
    (check-equal? 2 (language-field-count json))
    (check-equal? #f (language-field-name json 0))
    (check-equal? '("key" "value")
      (list-sort string<? (map (lambda (id) (language-field-name json id)) '(1 2))))
    (check-equal? "key" (language-field-name json (language-field-id json "key")))
    (check-equal? #f (language-field-id json "nope"))
    (check-equal? #f (language-field-name json 3))))

(test "parse states give the next state and the valid lookahead"
  (let* ([json (json)]
         [root (tree-root-node (parser-parse (make-parser json) "[1]"))]
         [open (node-child (node-child root 0) 0)]
         [state (node-parse-state open)]
         [names (map (lambda (s) (language-symbol-name json s))
                  (language-lookahead-symbols json state))])
    (check (member "[" names))
    (check (member "{" names))
    (check (member "number" names))
    (check-equal? (node-next-parse-state open)
      (language-next-state json state (node-grammar-symbol open)))))

(test "language procedures check their arguments"
  (check-raises assertion-violation? (language-symbol-name 'json 1))
  (check-raises assertion-violation? (language-symbol-name (json) -1))
  (check-raises assertion-violation? (language-symbol-name (json) #x10000))
  (check-raises assertion-violation? (language-field-id (json) 'key))
  (check-raises assertion-violation? (language-lookahead-symbols (json) 'state)))

(test-exit)
