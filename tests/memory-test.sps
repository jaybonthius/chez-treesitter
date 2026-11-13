(import (chezscheme) (tree-sitter) (check) (support))

(define example (python-example "python3-grammar.py"))

;; Dropped objects are freed when the next one is made, so settling
;; collects, makes and drops a small object, and collects again; the small
;; object it leaves is the same each time.
(define (settle!)
  (collect-all!)
  (make-query-cursor)
  (collect-all!)
  (make-query-cursor)
  (live-bytes))


(test "settling is stable"
  (let ([a (settle!)] [b (settle!)])
    (check-equal? a b)))

(test "dropped trees are freed, each once"
  (let ([baseline (settle!)])
    (let* ([parser (make-parser (python))]
           [trees (map (lambda (i) (parser-parse parser example)) (iota 20))])
      (check (> (live-bytes) (+ baseline (* 20 (bytevector-length example)))))
      (check-equal? 20 (length trees)))
    (check-equal? baseline (settle!))))

(define (deep-node)
  (let loop ([n (tree-root-node (parser-parse (make-parser (python)) example))])
    (if (= 0 (node-named-child-count n))
        n
        (loop (node-named-child n (- (node-named-child-count n) 1))))))

(test "a node keeps its tree alive"
  (let ([baseline (settle!)])
    (let ([node (deep-node)])
      (settle!)
      (parser-parse (make-parser (python)) "x = 1\n")
      (settle!)
      (check (string? (node-text node)))
      (check-equal? "module"
        (let up ([n node]) (if (node-parent n) (up (node-parent n)) (node-type n))))
      (check (pair? (query-captures (make-query (python) "(identifier) @i")
                      (node-parent (node-parent node))))))
    (check-equal? baseline (settle!))))

(test "nodes from many dropped trees stay usable while others are freed"
  (let ([nodes (let loop ([i 0] [acc '()])
                 (if (= i 30)
                     acc
                     (begin
                       (when (= 0 (mod i 5)) (collect-all!))
                       (loop (+ i 1) (cons (deep-node) acc)))))])
    (settle!)
    (check (for-all (lambda (n) (string=? (node-text n) (node-text (car nodes)))) nodes))
    (check (for-all (lambda (n) (node-parent n)) nodes))))

(test "edited copies, cursors, queries and parsers are freed"
  (let ([baseline (settle!)])
    (let* ([tree (parser-parse (make-parser (python)) example)]
           [edit (make-input-edit 0 0 1 (make-point 0 0) (make-point 0 0) (make-point 0 1))]
           [edited (map (lambda (i) (tree-edit tree edit)) (iota 10))]
           [cursors (map (lambda (i) (make-tree-cursor (tree-root-node tree))) (iota 10))]
           [copies (map tree-cursor-copy cursors)]
           [queries (map (lambda (i) (make-query (python) "((identifier) @i (#match? @i \"^x\"))"))
                      (iota 10))]
           [query-cursors (map (lambda (q) (let ([c (make-query-cursor)])
                                             (query-cursor-exec! c q (tree-root-node tree))
                                             (query-cursor-next-capture! c)
                                             c))
                            queries)]
           [parsers (map (lambda (i) (make-parser (json))) (iota 10))])
      (for-each tree-cursor-goto-first-child! cursors)
      (check-equal? 40 (length (append edited copies query-cursors parsers))))
    (check-equal? baseline (settle!))))

(test "a halted or failed parse leaves nothing behind"
  (let ([baseline (settle!)])
    (let ([p (make-parser (python))])
      (parser-parse p example #f (lambda (byte has-error?) #f))
      (guard (c [#t #f]) (parser-parse p (lambda (byte point) (raise 'stop))))
      (guard (c [#t #f]) (parser-parse p example #f (lambda (byte has-error?) (raise 'stop)))))
    (check-equal? baseline (settle!))))

;; Without help the collector would never run here: each tree is built in
;; tree-sitter's memory, from bytes Scheme does not copy.
(test "dropping big trees without allocating in Scheme keeps memory bounded"
  (let* ([big (bytevector->immutable-bytevector
                (call-with-bytevector-output-port
                  (lambda (out) (do ([i 0 (+ i 1)]) ((= i 40)) (put-bytevector out example)))))]
         [parser (make-parser (python))]
         [one (let ([before (settle!)])
                (let ([tree (parser-parse parser big)])
                  (- (live-bytes) before)))]
         [most 0])
    (settle!)
    (do ([i 0 (+ i 1)]) ((= i 150))
      (parser-parse parser big)
      (set! most (max most (live-bytes))))
    (check (< (* 4 most) (* 150 one)))
    (check (< most (* 1024 1024 1024)))))

(test-exit)
