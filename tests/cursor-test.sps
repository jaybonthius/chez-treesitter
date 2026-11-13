(import (chezscheme) (tree-sitter) (check) (support))

(define root
  (tree-root-node (parser-parse (make-parser (python)) (python-example "python3-grammar.py"))))

(test "a cursor visits the nodes the node procedures do, in order"
  (let ([walked (cursor-preorder root)] [expected (preorder-nodes root)])
    (check-equal? (length expected) (length walked))
    (check (for-all node=? expected walked))
    (check-equal? (node-descendant-count root) (length walked))))

(test "a cursor knows the field, depth and preorder index of its node"
  (let ([c (make-tree-cursor root)] [mismatches 0] [index 0])
    (check (tree-cursor? c))
    (check-equal? 0 (tree-cursor-depth c))
    (check-equal? #f (tree-cursor-field-name c))
    (check-equal? #f (tree-cursor-field-id c))
    (let walk ([depth 0])
      (unless (= index (tree-cursor-descendant-index c)) (set! mismatches (+ mismatches 1)))
      (unless (= depth (tree-cursor-depth c)) (set! mismatches (+ mismatches 1)))
      (set! index (+ index 1))
      (when (tree-cursor-goto-first-child! c)
        (let loop ([i 0])
          (let* ([node (tree-cursor-node c)] [parent (node-parent node)])
            (unless (equal? (tree-cursor-field-name c) (node-field-name-for-child parent i))
              (set! mismatches (+ mismatches 1)))
            (unless (equal? (tree-cursor-field-id c)
                      (let ([name (tree-cursor-field-name c)])
                        (and name (language-field-id (python) name))))
              (set! mismatches (+ mismatches 1))))
          (walk (+ depth 1))
          (when (tree-cursor-goto-next-sibling! c) (loop (+ i 1))))
        (tree-cursor-goto-parent! c)))
    (check-equal? 0 mismatches)
    (check-equal? (node-descendant-count root) index)))

(test "a cursor jumps to a preorder index"
  (let ([c (make-tree-cursor root)] [nodes (list->vector (preorder-nodes root))])
    (for-each
      (lambda (i)
        (tree-cursor-goto-descendant! c i)
        (check (node=? (vector-ref nodes i) (tree-cursor-node c)))
        (check-equal? i (tree-cursor-descendant-index c)))
      (list 0 1 17 500 (- (vector-length nodes) 1) 3))))

(test "a cursor moves to the last child and back"
  (let ([c (make-tree-cursor root)])
    (check (tree-cursor-goto-last-child! c))
    (check (node=? (node-child root (- (node-child-count root) 1)) (tree-cursor-node c)))
    (check (tree-cursor-goto-previous-sibling! c))
    (check (node=? (node-child root (- (node-child-count root) 2)) (tree-cursor-node c)))
    (check (tree-cursor-goto-parent! c))
    (check (not (tree-cursor-goto-parent! c)))
    (check (not (tree-cursor-goto-next-sibling! c)))))

(test "a cursor finds the first child at a byte or point"
  (let* ([tree (parser-parse (make-parser (json)) "[1,\n 22, 3]")]
         [array (node-child (tree-root-node tree) 0)]
         [c (make-tree-cursor array)])
    (check-equal? 3 (tree-cursor-goto-first-child-for-byte! c 5))
    (check-equal? "22" (node-text (tree-cursor-node c)))
    (tree-cursor-reset! c array)
    (check-equal? 3 (tree-cursor-goto-first-child-for-point! c (make-point 1 2)))
    (check-equal? "22" (node-text (tree-cursor-node c)))
    (tree-cursor-reset! c array)
    (check-equal? #f (tree-cursor-goto-first-child-for-byte! c 100))))

(test "a cursor stays inside the node it started from"
  (let* ([statement (node-named-child root 3)] [c (make-tree-cursor statement)])
    (check (not (tree-cursor-goto-parent! c)))
    (check (not (tree-cursor-goto-next-sibling! c)))
    (check (node=? statement (tree-cursor-node c)))))

(test "a reset cursor walks another tree"
  (let* ([other (tree-root-node (parser-parse (make-parser (json)) "[1]"))]
         [c (make-tree-cursor root)])
    (tree-cursor-goto-first-child! c)
    (tree-cursor-reset! c other)
    (check (node=? other (tree-cursor-node c)))
    (check (tree-cursor-goto-first-child! c))
    (check-equal? "array" (node-type (tree-cursor-node c)))
    (check (eq? (json) (node-language (tree-cursor-node c))))))

(test "copies and reset-to share a position but move apart"
  (let ([c (make-tree-cursor root)])
    (tree-cursor-goto-first-child! c)
    (tree-cursor-goto-next-sibling! c)
    (let ([copy (tree-cursor-copy c)] [other (make-tree-cursor root)])
      (check (node=? (tree-cursor-node c) (tree-cursor-node copy)))
      (tree-cursor-goto-next-sibling! copy)
      (check (not (node=? (tree-cursor-node c) (tree-cursor-node copy))))
      (tree-cursor-reset-to! other c)
      (check (node=? (tree-cursor-node c) (tree-cursor-node other)))
      (check-equal? 1 (tree-cursor-depth other))
      (check (tree-cursor-goto-parent! other))
      (check (node=? root (tree-cursor-node other)))
      (tree-cursor-reset-to! c c)
      (check-equal? 1 (tree-cursor-depth c)))))

(test "cursor procedures check their arguments"
  (let ([c (make-tree-cursor root)])
    (check-raises assertion-violation? (make-tree-cursor 'node))
    (check-raises assertion-violation? (tree-cursor-node 'cursor))
    (check-raises assertion-violation? (tree-cursor-goto-descendant! c -1))
    (check-raises assertion-violation? (tree-cursor-goto-first-child-for-point! c 3))
    (check-raises assertion-violation? (tree-cursor-reset! c 'node))
    (check-raises assertion-violation? (tree-cursor-reset-to! c 'cursor))
    (check-equal? 'tree-cursor-field-name
      (guard (c [#t (condition-who c)]) (tree-cursor-field-name 'cursor)))))

(test-exit)
