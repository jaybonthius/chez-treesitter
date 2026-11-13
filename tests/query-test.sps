(import (chezscheme) (tree-sitter) (check) (support))

(define (raised thunk)
  (guard (c [#t c]) (thunk) #f))

(define (root language source)
  (tree-root-node (parser-parse (make-parser language) source)))

(define (match-texts query node)
  (map (lambda (m) (capture-texts (query-match-captures m))) (query-matches query node)))

(define (match-texts* query node start end)
  (map (lambda (m) (capture-texts (query-match-captures m))) (query-matches query node start end)))

(define json-root (root (json) "[1,2,{\"name\": \"Bob\"},3]"))

(test "captures come in document order, filtered by predicates"
  (let ([query (make-query (json)
                 "(string) @string
(pair key: (_) @keyword)
((_) @bob (#match? @bob \"^B.b$\"))
(number) @number
((number) @n3 (#eq? @n3 \"3\"))
((number) @n3p (#last-sibling? @n3p))"
                 (lambda (name arguments)
                   (and (string=? name "last-sibling?")
                        (not (node-next-named-sibling (car (car arguments)))))))])
    (check-equal?
      '(("number" . "1") ("number" . "2") ("string" . "\"name\"") ("keyword" . "\"name\"")
        ("string" . "\"Bob\"") ("bob" . "Bob") ("number" . "3") ("n3" . "3") ("n3p" . "3"))
      (named-captures query json-root))))

(test "matches group captures by pattern"
  (let* ([query (make-query (json) "(pair key: (_) @key value: (_) @value) (number) @number")]
         [matches (query-matches query json-root)])
    (check-equal? '(1 1 0 1) (map query-match-pattern matches))
    (check-equal? '((("key" . "\"name\"") ("value" . "\"Bob\""))) (match-texts (make-query (json) "(pair key: (_) @key value: (_) @value)") json-root))
    (let ([capture (car (query-match-captures (caddr matches)))])
      (check (query-capture? capture))
      (check-equal? "key" (query-capture-name capture))
      (check-equal? 0 (query-capture-index capture))
      (check-equal? 0 (query-capture-pattern capture))
      (check (node? (query-capture-node capture))))))

(test "a byte range limits the captures"
  (let ([query (make-query (json) "(number) @n")])
    (check-equal? '(("n" . "2") ("n" . "3"))
      (capture-texts (query-captures query json-root 3 22)))
    (check-equal? '((("n" . "1")) (("n" . "2"))) (match-texts* query json-root 0 4))))

(test "an invalid query raises a query error with its kind and position"
  (for-each
    (lambda (source kind offset row column)
      (let ([c (raised (lambda () (make-query (json) source)))])
        (check (query-error? c))
        (check-equal? kind (query-error-kind c))
        (check-equal? offset (query-error-offset c))
        (check-equal? row (query-error-row c))
        (check-equal? column (query-error-column c))
        (check-equal? 'make-query (condition-who c))))
    '("(array" "(foo)" "(pair nope: (_))" "((number) @n (#eq? @m \"1\"))" "(object (number))"
      "(number) @n\n(array\n  (foo))")
    '(syntax node-type field capture structure node-type)
    '(6 1 6 20 8 22)
    '(0 0 0 0 0 2)
    '(6 1 6 20 8 3)))

(test "a malformed or unknown predicate is a predicate error at its pattern"
  (for-each
    (lambda (source)
      (let ([c (raised (lambda () (make-query (json) source)))])
        (check (query-error? c))
        (check-equal? 'predicate (query-error-kind c))
        (check-equal? 12 (query-error-offset c))))
    '("(number) @a ((number) @n (#eq? @n))"
      "(number) @a ((number) @n (#eq? \"1\" @n))"
      "(number) @a ((number) @n (#match? @n @n))"
      "(number) @a ((number) @n (#any-of? \"1\"))"
      "(number) @a ((number) @n (#set!))"
      "(number) @a ((number) @n (#set! a b c))"
      "(number) @a ((number) @n (#is? @n @n x))"
      "(number) @a ((number) @n (#frob? @n))"
      "(number) @a ((number) @n (#match? @n \"[\"))"
      "(number) @a ((number) @n (#match? @n \"(?i)x\"))"
      "(number) @a ((number) @n (#match? @n \"\\\\p{L}\"))")))

(define comments (root (python) "# a\n# a\nx\n# a\n# b\ny\n"))

(define (comment-groups predicate)
  (map (lambda (m) (map cdr m))
    (match-texts (make-query (python) (string-append "((comment)+ @c " predicate ")")) comments)))

(test "text predicates test every captured node, or with any- one of them"
  (check-equal? '(("# a" "# a") ("# a" "# b")) (comment-groups ""))
  (check-equal? '(("# a" "# a")) (comment-groups "(#eq? @c \"# a\")"))
  (check-equal? '(("# a" "# b")) (comment-groups "(#any-eq? @c \"# b\")"))
  (check-equal? '() (comment-groups "(#not-eq? @c \"# a\")"))
  (check-equal? '(("# a" "# b")) (comment-groups "(#any-not-eq? @c \"# a\")"))
  (check-equal? '(("# a" "# a")) (comment-groups "(#match? @c \"^# a$\")"))
  (check-equal? '(("# a" "# b")) (comment-groups "(#any-match? @c \"b\")"))
  (check-equal? '(("# a" "# a")) (comment-groups "(#not-match? @c \"b\")"))
  (check-equal? '(("# a" "# a") ("# a" "# b")) (comment-groups "(#any-not-match? @c \"b\")"))
  (check-equal? '(("# a" "# a")) (comment-groups "(#any-of? @c \"# a\" \"# c\")"))
  (check-equal? '(("# a" "# a")) (comment-groups "(#not-any-of? @c \"# b\")")))

(test "#eq? compares two captures"
  (let ([module (root (python) "x = x\ny = z\n")])
    (check-equal? '((("a" . "x") ("b" . "x")))
      (match-texts (make-query (python) "((assignment left: (identifier) @a right: (identifier) @b) (#eq? @a @b))") module))
    (check-equal? '((("a" . "y") ("b" . "z")))
      (match-texts (make-query (python) "((assignment left: (identifier) @a right: (identifier) @b) (#not-eq? @a @b))") module))))

(test "#match? anchors at the node and takes Rust's common regular expression forms"
  (let* ([module (root (python) "abc = 1\nA_B2 = 2\nfoo_bar = 3\nx9 = 4\n")]
         [names (lambda (regex)
                  (map cdr (named-captures
                             (make-query (python)
                               (format "((identifier) @n (#match? @n ~s))" regex))
                             module)))])
    (check-equal? '("abc") (names "^a"))
    (check-equal? '("A_B2") (names "^[A-Z][A-Z_\\d]*$"))
    (check-equal? '("A_B2" "x9") (names "\\d"))
    (check-equal? '("A_B2" "foo_bar") (names "^\\w+_\\w+$"))
    (check-equal? '("abc" "foo_bar") (names "^[^\\d_A-Z]+$|_b"))
    (check-equal? '("abc" "foo_bar") (names "^(?:abc|foo.*?)$"))
    (check-equal? '("A_B2" "foo_bar") (names "[\\]_-]"))
    (check-equal? '("x9") (names "[\\]-]|9"))
    (check-equal? '() (names "[\\^]"))
    (check-equal? '("abc" "A_B2" "foo_bar" "x9") (names "[^^]"))))

(test "#set!, #is? and #is-not? become pattern properties"
  (let ([query (make-query (python)
                 "((identifier) @x (#set! priority \"10\") (#set! @x role) (#is? local) (#is-not? @x definition \"yes\"))
(string) @s")])
    (let ([settings (query-pattern-settings query 0)])
      (check-equal? 2 (length settings))
      (check (query-property? (car settings)))
      (check-equal? "priority" (query-property-key (car settings)))
      (check-equal? "10" (query-property-value (car settings)))
      (check-equal? #f (query-property-capture (car settings)))
      (check-equal? "role" (query-property-key (cadr settings)))
      (check-equal? #f (query-property-value (cadr settings)))
      (check-equal? "x" (query-property-capture (cadr settings))))
    (check-equal? '("local") (map query-property-key (query-pattern-assertions query 0)))
    (let ([negated (car (query-pattern-negated-assertions query 0))])
      (check-equal? "definition" (query-property-key negated))
      (check-equal? "yes" (query-property-value negated))
      (check-equal? "x" (query-property-capture negated)))
    (check-equal? '() (query-pattern-settings query 1))
    (check-equal? '(("x" . "a") ("s" . "'b'")) (named-captures query (root (python) "a = 'b'\n")))))

(test "a handler decides predicates tree-sitter leaves to the host"
  (let* ([seen '()]
         [query (make-query (json) "((number) @n (#odd? @n \"x\" @n))"
                  (lambda (name arguments)
                    (set! seen (cons (cons name arguments) seen))
                    (odd? (string->number (node-text (car (car arguments)))))))])
    (check-equal? '(("n" . "1") ("n" . "3")) (named-captures query json-root))
    (check (for-all (lambda (s) (and (equal? "odd?" (car s)) (equal? "x" (caddr s))
                                     (node? (car (cadr s))) (node=? (car (cadr s)) (car (cadddr s)))))
             seen))))

(test "directives are pattern data for the host, and filter no match"
  (let* ([source (string-append
                   "((comment)* @doc . (function_definition name: (identifier) @name) @definition"
                   " (#strip! @doc \"^#\\\\s*\") (#select-adjacent! @doc @definition))\n"
                   "(identifier) @i")]
         [module (root (python) "# about f\ndef f(): pass\n")]
         [query (make-query (python) source)]
         [first-pattern (lambda (matches)
                          (map (lambda (m) (capture-texts (query-match-captures m)))
                            (filter (lambda (m) (= 0 (query-match-pattern m))) matches)))])
    (check-equal? '(("strip!" 0 "^#\\s*") ("select-adjacent!" 0 2))
      (query-pattern-directives query 0))
    (check-equal? '() (query-pattern-directives query 1))
    (check-equal? '("doc" "name" "definition") (list-head (query-capture-names query) 3))
    (check-equal? '((("doc" . "# about f") ("definition" . "def f(): pass") ("name" . "f")))
      (first-pattern (query-matches query module)))
    (check-equal? (first-pattern (query-matches query module))
      (first-pattern
        (query-matches (make-query (python) source (lambda (name arguments) (raise 'called)))
          module)))))

(test "a condition raised by a handler reaches the caller"
  (let ([query (make-query (json) "((number) @n (#fail? @n))" (lambda (name arguments) (raise 'handler)))])
    (check-equal? 'handler (raised (lambda () (query-captures query json-root))))))

(test "a query describes its patterns and captures"
  (let ([query (make-query (python)
                 "(function_definition name: (identifier) @name) @def
((identifier)+ @ids)
(call function: (_)? @fn) @call")])
    (check (query? query))
    (check (eq? (python) (query-language query)))
    (check-equal? 3 (query-pattern-count query))
    (check-equal? 5 (query-capture-count query))
    (check-equal? '("name" "def" "ids" "fn" "call") (query-capture-names query))
    (check-equal? 0 (query-pattern-start-byte query 0))
    (check-equal? (+ 1 (string-length "(function_definition name: (identifier) @name) @def"))
      (query-pattern-end-byte query 0))
    (check-equal? (query-pattern-end-byte query 0) (query-pattern-start-byte query 1))
    (check (query-pattern-rooted? query 0))
    (check (not (query-pattern-non-local? query 0)))
    (check (query-pattern-non-local? query 1))
    (check (not (query-step-guaranteed? query 1)))
    (check (query-step-guaranteed? query 27))
    (check-equal? 'one (query-capture-quantifier query 0 "name"))
    (check-equal? 'zero (query-capture-quantifier query 0 "ids"))
    (check-equal? 'one-or-more (query-capture-quantifier query 1 "ids"))
    (check-equal? 'zero-or-one (query-capture-quantifier query 2 "fn"))
    (check-raises assertion-violation? (query-capture-quantifier query 0 "nope"))
    (check-raises assertion-violation? (query-pattern-start-byte query 3))))

(test "patterns and captures can be left out of a copy"
  (let* ([query (make-query (json) "(number) @n ((string) @s (#eq? @s \"\\\"Bob\\\"\")) (pair) @p")]
         [no-numbers (query-without-pattern query 0)]
         [no-pairs (query-without-capture query "p")])
    (check-equal? '(("p" . "\"name\": \"Bob\"") ("s" . "\"Bob\""))
      (named-captures no-numbers json-root))
    (check-equal? '("1" "2" "\"Bob\"" "3") (map cdr (named-captures no-pairs json-root)))
    (check-equal? 5 (length (named-captures query json-root)))))

(test "a query cursor steps through matches and captures"
  (let ([cursor (make-query-cursor)] [query (make-query (json) "(array (number) @n (number) @m)")])
    (check (query-cursor? cursor))
    (check-equal? #f (query-cursor-next-match! cursor))
    (query-cursor-exec! cursor query json-root)
    (let ([m (query-cursor-next-match! cursor)])
      (check (query-match? m))
      (check-equal? '(("n" . "1") ("m" . "2")) (capture-texts (query-match-captures m))))
    (query-cursor-exec! cursor query json-root)
    (let loop ([acc '()])
      (let ([c (query-cursor-next-capture! cursor)])
        (if c
            (loop (cons (node-text (query-capture-node c)) acc))
            (check-equal? '("1" "1" "2" "2" "3" "3") (reverse acc)))))))

(test "a query cursor limits by range, containing range, depth and match count"
  (let ([query (make-query (json) "(number) @n")] [cursor (make-query-cursor)])
    (define (texts)
      (query-cursor-exec! cursor query json-root)
      (let loop ([acc '()])
        (let ([c (query-cursor-next-capture! cursor)])
          (if c (loop (cons (node-text (query-capture-node c)) acc)) (reverse acc)))))
    (query-cursor-byte-range-set! cursor 2 22)
    (check-equal? '("2" "3") (texts))
    (query-cursor-byte-range-set! cursor 0 #xffffffff)
    (query-cursor-point-range-set! cursor (make-point 0 0) (make-point 0 2))
    (check-equal? '("1") (texts))
    (query-cursor-point-range-set! cursor (make-point 0 0) (make-point #xffffffff #xffffffff))
    (query-cursor-containing-byte-range-set! cursor 3 4)
    (check-equal? '("2") (texts))
    (query-cursor-containing-byte-range-set! cursor 0 #xffffffff)
    (query-cursor-containing-point-range-set! cursor (make-point 0 21) (make-point 0 22))
    (check-equal? '("3") (texts))
    (query-cursor-containing-point-range-set! cursor (make-point 0 0)
      (make-point #xffffffff #xffffffff))
    (query-cursor-max-start-depth-set! cursor 1)
    (check-equal? '() (texts))
    (query-cursor-max-start-depth-set! cursor 2)
    (check-equal? '("1" "2" "3") (texts))
    (query-cursor-max-start-depth-set! cursor #f)
    (check-equal? #f (query-cursor-match-limit cursor))
    (query-cursor-match-limit-set! cursor 1)
    (check-equal? 1 (query-cursor-match-limit cursor))
    (texts)
    (check (not (query-cursor-match-limit-exceeded? cursor)))
    (let* ([deep (root (python) "a(b(c(d(e(f(g(h(i))))))))\n")]
           [query (make-query (python) "(call (argument_list (call) @inner)) @outer")])
      (query-cursor-exec! cursor query deep)
      (let loop () (when (query-cursor-next-capture! cursor) (loop)))
      (check (query-cursor-match-limit-exceeded? cursor)))
    (query-cursor-match-limit-set! cursor #f)
    (check-raises assertion-violation? (query-cursor-byte-range-set! cursor 5 4))
    (check-raises assertion-violation?
      (query-cursor-point-range-set! cursor (make-point 1 0) (make-point 0 5)))))

(test "a tree without source text needs the source for text predicates"
  (let* ([bytes (string->utf8 "[1, 2]")]
         [tree (parser-parse (make-parser (json)) (lambda (byte point) (and (= byte 0) bytes)))]
         [cursor (make-query-cursor)]
         [query (make-query (json) "((number) @n (#eq? @n \"2\"))")])
    (check-raises assertion-violation? (query-captures query (tree-root-node tree)))
    (query-cursor-exec! cursor query (tree-root-node tree) bytes)
    (check-equal? "(number)" (node-sexp (query-capture-node (query-cursor-next-capture! cursor))))
    (check-equal? 1 (length (query-captures (make-query (json) "(array (number) @n .)") (tree-root-node tree))))
    (check-equal? 1 (length (query-captures query (tree-root-node tree) bytes)))
    (check-equal? 1 (length (query-matches query (tree-root-node tree) 0 6 bytes)))
    (check-raises assertion-violation? (query-matches query (tree-root-node tree) "[1, 2]"))))

(test "a query runs only on nodes of its language"
  (check-raises assertion-violation?
    (query-captures (make-query (python) "(identifier) @i") json-root)))

(test "the grammar's own highlight and tag queries run"
  (let* ([read (lambda (path) (utf8->string (read-file path)))]
         [module (root (python) (python-example "python3-grammar.py"))]
         [highlights (make-query (python) (read "vendor/tree-sitter-python/queries/highlights.scm"))]
         [captures (query-captures highlights module)])
    (check (> (length captures) 1000))
    (check (let loop ([cs captures])
             (or (null? (cdr cs))
                 (and (<= (node-start-byte (query-capture-node (car cs)))
                          (node-start-byte (query-capture-node (cadr cs))))
                      (loop (cdr cs))))))
    (check (exists (lambda (c) (string=? "constructor" (query-capture-name c))) captures))
    (let ([tags (read "vendor/tree-sitter-python/queries/tags.scm")])
      (check (exists (lambda (c) (string=? "definition.function" (query-capture-name c)))
               (query-captures (make-query (python) tags) module))))))

(test "query procedures check their arguments"
  (check-raises assertion-violation? (make-query 'json "(number) @n"))
  (check-raises assertion-violation? (make-query (json) '(number)))
  (check-raises assertion-violation? (make-query (json) "(number) @n" 'handler))
  (check-raises assertion-violation? (query-captures 'query json-root))
  (check-raises assertion-violation? (query-captures (make-query (json) "(number) @n") 'node))
  (check-raises assertion-violation? (query-cursor-exec! (make-query-cursor) 'query json-root))
  (check-raises assertion-violation? (query-cursor-match-limit-set! (make-query-cursor) 0))
  (check-raises assertion-violation? (query-without-capture (make-query (json) "(number) @n") 'n)))

(test-exit)
