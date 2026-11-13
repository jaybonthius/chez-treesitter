(import (chezscheme) (tree-sitter) (check) (support))

(define example (python-example "python3-grammar.py"))
(define expected (node-sexp (tree-root-node (parser-parse (make-parser (python)) example))))

(define (spawn thunk)
  (let* ([result (box #f)]
         [thread (fork-thread (lambda () (set-box! result (guard (c [#t c]) (thunk)))))])
    (lambda () (thread-join thread) (unbox result))))

(define (run-threads count thunk)
  (map (lambda (join) (join))
    (map (lambda (i) (spawn (lambda () (thunk i)))) (iota count))))

(test "threads parse at once, each with its own parser"
  (let ([results (run-threads 4
                   (lambda (i)
                     (let ([parser (make-parser (python))])
                       (let loop ([n 0] [ok? #t])
                         (if (= n 10)
                             ok?
                             (loop (+ n 1)
                               (and ok? (string=? expected
                                          (node-sexp (tree-root-node (parser-parse parser example)))))))))))])
    (check-equal? '(#t #t #t #t) results)))

(test "threads share a tree, walking, querying and editing it"
  (let* ([tree (parser-parse (make-parser (python)) example)]
         [root (tree-root-node tree)]
         [count (length (cursor-preorder root))]
         [query (make-query (python) "((identifier) @constant (#match? @constant \"^[A-Z][A-Z_]*$\"))")]
         [constants (length (query-captures query root))]
         [edit (make-input-edit 0 0 1 (make-point 0 0) (make-point 0 0) (make-point 0 1))]
         [results (run-threads 4
                    (lambda (i)
                      (let loop ([n 0] [ok? #t])
                        (if (= n 5)
                            ok?
                            (loop (+ n 1)
                              (and ok?
                                   (= count (length (cursor-preorder root)))
                                   (= constants (length (query-captures query root)))
                                   (node-has-changes? (tree-root-node (tree-edit tree edit)))))))))])
    (check-equal? '(#t #t #t #t) results)
    (check (not (node-has-changes? root)))))

(test "a parser in use by one thread refuses another"
  (let* ([parser (make-parser (json))]
         [m (make-mutex)] [started (make-condition)] [proceed (make-condition)]
         [state 'waiting]
         [worker (spawn
                   (lambda ()
                     (parser-parse parser
                       (lambda (byte point)
                         (with-mutex m
                           (when (eq? state 'waiting)
                             (set! state 'started)
                             (condition-broadcast started)
                             (let wait () (unless (eq? state 'go) (condition-wait proceed m) (wait)))))
                         (and (= byte 0) "[1]")))))])
    (with-mutex m (let wait () (unless (eq? state 'started) (condition-wait started m) (wait))))
    (let ([c (guard (c [#t c]) (parser-parse parser "[2]"))])
      (check (assertion-violation? c))
      (check (string-contains? (condition-message c) "in use")))
    (with-mutex m (set! state 'go) (condition-broadcast proceed))
    (check-equal? "(document (array (number)))" (node-sexp (tree-root-node (worker))))
    (check (tree? (parser-parse parser "[2]")))))

(test "a cursor used by two threads at once refuses one of them, and stays sound"
  (let* ([root (tree-root-node (parser-parse (make-parser (python)) example))]
         [cursor (make-tree-cursor root)]
         [results (run-threads 4
                    (lambda (i)
                      (let loop ([n 0] [refused 0])
                        (if (= n 20000)
                            refused
                            (loop (+ n 1)
                              (guard (c [(and (assertion-violation? c)
                                              (string-contains? (condition-message c) "in use"))
                                         (+ refused 1)])
                                (unless (tree-cursor-goto-first-child! cursor)
                                  (unless (tree-cursor-goto-next-sibling! cursor)
                                    (tree-cursor-reset! cursor root)))
                                (node-type (tree-cursor-node cursor))
                                refused))))))])
    (check (for-all fixnum? results))
    (check (node? (tree-cursor-node cursor)))))

(test "other threads collect while a long parse runs"
  (let* ([big (call-with-bytevector-output-port
                (lambda (out) (do ([i 0 (+ i 1)]) ((= i 30)) (put-bytevector out example))))]
         [done? #f]
         [worker (spawn
                   (lambda ()
                     (let ([tree (parser-parse (make-parser (python)) big)])
                       (set! done? #t)
                       tree)))]
         [collections
          (let loop ([n 0])
            (if done?
                n
                (begin
                  (make-vector 100000 n)
                  (collect-rendezvous)
                  (loop (+ n 1)))))])
    (let ([tree (worker)])
      (check (> collections 0))
      (check (not (node-has-error? (tree-root-node tree))))
      (check-equal? (bytevector-length big) (node-end-byte (tree-root-node tree))))))

(test "trees made in threads are freed after the threads end"
  (collect-all!)
  (make-query-cursor)
  (collect-all!)
  (make-query-cursor)
  (let ([baseline (live-bytes)])
    (run-threads 4 (lambda (i) (parser-parse (make-parser (python)) example) #t))
    (collect-all!)
    (make-query-cursor)
    (collect-all!)
    (make-query-cursor)
    (check-equal? baseline (live-bytes))))

(test-exit)
