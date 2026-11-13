(import (chezscheme) (tree-sitter) (check) (support))

(define (parse language source . old)
  (apply parser-parse (make-parser language) source old))

(test "editing a tree leaves the original and its nodes as they were"
  (let* ([source "def foo():\n  bar()\n"]
         [tree (parse (python) source)]
         [root (tree-root-node tree)]
         [call (node-named-descendant-for-byte-range root 13 18)])
    (let-values ([(edit new) (replace-source source 0 0 "\n\n")])
      (let ([edited (tree-edit tree edit)])
        (check (not (eq? edited tree)))
        (check-equal? "call" (node-type call))
        (check-equal? 13 (node-start-byte call))
        (check-equal? "bar()" (node-text call))
        (check (not (node-has-changes? root)))
        (check (node-has-changes? (tree-root-node edited)))
        (check-equal? (+ 2 (node-end-byte root)) (node-end-byte (tree-root-node edited)))
        (let ([moved (node-named-descendant-for-byte-range (tree-root-node edited) 15 20)])
          (check-equal? "call" (node-type moved))
          (check-equal? (make-point 3 2) (node-start-point moved)))
        (check-raises assertion-violation? (node-text (tree-root-node edited)))))))

(test "an edited tree reparses incrementally, and reports what changed"
  (let*-values ([(source) "def foo():\n  bar()"]
                [(parser) (make-parser (python))]
                [(tree) (parser-parse parser source)]
                [(edit new) (replace-source source 15 15 "(1, 2)")]
                [(edited) (tree-edit tree edit)]
                [(reparsed) (parser-parse parser new edited)])
    (check-equal? "def foo():\n  ba(1, 2)r()" new)
    (check-equal? (node-sexp (tree-root-node (parse (python) new)))
      (node-sexp (tree-root-node reparsed)))
    (let ([changed (tree-changed-ranges edited reparsed)])
      (check (pair? changed))
      (check (for-all range? changed))
      (check (exists (lambda (r) (<= (range-start-byte r) 15 (range-end-byte r))) changed)))
    (check-equal? '() (tree-changed-ranges reparsed reparsed))))

(test "an unedited tree can be the old tree of a parse"
  (let* ([parser (make-parser (json))] [tree (parser-parse parser "[1, 2]")])
    (check-equal? (node-sexp (tree-root-node tree))
      (node-sexp (tree-root-node (parser-parse parser "[1, 2]" tree))))))

;; Python's scanner keeps indentation state that must survive reuse.
(define (random-edits language source pieces count seed)
  (let ([parser (make-parser language)] [fresh (make-parser language)])
    (random-seed seed)
    (let loop ([i 0] [source (string->utf8 source)] [tree (parser-parse parser source)]
               [failures 0])
      (if (= i count)
          failures
          (let* ([size (bytevector-length source)]
                 [start (random (+ size 1))]
                 [old-end (min size (+ start (random 6)))]
                 [piece (list-ref pieces (random (length pieces)))])
            (let*-values ([(edit new) (edit-source source start old-end piece)]
                          [(reparsed) (parser-parse parser new (tree-edit tree edit))])
              (loop (+ i 1) new reparsed
                (if (string=? (node-sexp (tree-root-node reparsed))
                      (node-sexp (tree-root-node (parser-parse fresh new))))
                    failures
                    (+ failures 1)))))))))

(test "incremental reparses agree with fresh parses after random edits"
  (check-equal? 0
    (random-edits (json) "{\"a\": [1, 2, {\"b\": null}], \"c\": \"d\"}"
      '("1" "," "[" "]" "{" "}" "\"x\"" ":" " " "\n" "true" "" "\"" "null")
      400 7))
  (check-equal? 0
    (random-edits (python) (utf8->string (python-example "python3-grammar.py"))
      '("x" " " "\n" "    " "\t" "def f():\n    pass\n" "(" ")" ":" "if y:\n" "\"\"\"" "'" "#"
        "" "é" "    return 1\n")
      150 11)))

(test "changed ranges cover the edited region"
  (let*-values ([(source) "[1, 2, 3]"]
                [(parser) (make-parser (json))]
                [(tree) (parser-parse parser source)]
                [(edit new) (replace-source source 4 5 "{}")]
                [(edited) (tree-edit tree edit)]
                [(reparsed) (parser-parse parser new edited)])
    (let ([changed (tree-changed-ranges edited reparsed)])
      (check-equal? 1 (length changed))
      (check (<= (range-start-byte (car changed)) 4))
      (check (>= (range-end-byte (car changed)) 6)))))

(test "tree-edit checks its arguments"
  (let ([tree (parse (json) "[]")])
    (check-raises assertion-violation? (tree-edit 'tree (make-input-edit 0 0 0 (make-point 0 0) (make-point 0 0) (make-point 0 0))))
    (check-raises assertion-violation? (tree-edit tree '(0 0 0)))
    (check-raises assertion-violation? (tree-changed-ranges tree 'tree))
    (let ([other (parse (python) "x\n")])
      (check-raises assertion-violation? (tree-changed-ranges tree other))
      (check-raises assertion-violation? (parser-parse (make-parser (json)) "[]" other)))))

(test-exit)
