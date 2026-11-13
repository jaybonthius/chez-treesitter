(library (support)
  (export grammar-path json python edit-source replace-source preorder-nodes cursor-preorder
    sexp-of capture-texts named-captures string-contains? live-bytes collect-all!
    read-file python-example)
  (import (chezscheme) (tree-sitter))

  (define (grammar-path name)
    (let ([file (format "grammars/libtree-sitter-~a.so" name)])
      (let loop ([directories (library-directories)])
        (cond
          [(null? directories) (errorf 'grammar-path "~a is in none of the library directories" file)]
          [(file-exists? (path-build (cdar directories) file)) (path-build (cdar directories) file)]
          [else (loop (cdr directories))]))))

  (define json
    (let ([language #f])
      (lambda ()
        (unless language (set! language (load-language (grammar-path "json") "json")))
        language)))

  (define python
    (let ([language #f])
      (lambda ()
        (unless language (set! language (load-language (grammar-path "python") "python")))
        language)))

  (define (point-at bytes offset)
    (let loop ([i 0] [row 0] [line-start 0])
      (cond
        [(fx= i offset) (make-point row (fx- offset line-start))]
        [(fx= (bytevector-u8-ref bytes i) 10) (loop (fx+ i 1) (fx+ row 1) (fx+ i 1))]
        [else (loop (fx+ i 1) row line-start)])))

  (define (edit-source source start old-end replacement)
    (let* ([new (string->utf8 replacement)]
           [result (make-bytevector (fx+ (fx- (bytevector-length source) (fx- old-end start))
                                         (bytevector-length new)))]
           [new-end (fx+ start (bytevector-length new))])
      (bytevector-copy! source 0 result 0 start)
      (bytevector-copy! new 0 result start (bytevector-length new))
      (bytevector-copy! source old-end result new-end (fx- (bytevector-length source) old-end))
      (values
        (make-input-edit start old-end new-end (point-at source start) (point-at source old-end)
          (point-at result new-end))
        result)))

  (define (replace-source source start old-end replacement)
    (let-values ([(edit new) (edit-source (string->utf8 source) start old-end replacement)])
      (values edit (utf8->string new))))

  (define (preorder-nodes node)
    (cons node (apply append (map preorder-nodes (node-children node)))))

  (define (cursor-preorder node)
    (let ([c (make-tree-cursor node)])
      (let loop ([acc (list (tree-cursor-node c))])
        (if (tree-cursor-goto-first-child! c)
            (loop (cons (tree-cursor-node c) acc))
            (let up ([acc acc])
              (cond
                [(tree-cursor-goto-next-sibling! c) (loop (cons (tree-cursor-node c) acc))]
                [(tree-cursor-goto-parent! c) (up acc)]
                [else (reverse acc)]))))))

  (define (sexp-of node)
    (call-with-string-output-port
      (lambda (out)
        (let write-node ([node node] [field #f] [root? #t])
          (let ([visible? (or root? (node-named? node))])
            (when visible?
              (unless root?
                (put-char out #\space)
                (when field (put-string out field) (put-string out ": ")))
              (put-char out #\()
              (put-string out (node-type node)))
            (do ([i 0 (+ i 1)]) ((= i (node-child-count node)))
              (write-node (node-child node i) (node-field-name-for-child node i) #f))
            (when visible? (put-char out #\))))))))

  (define (capture-texts captures)
    (map (lambda (c) (cons (query-capture-name c) (node-text (query-capture-node c)))) captures))

  (define (named-captures query node)
    (capture-texts (query-captures query node)))

  (define (string-contains? s part)
    (let ([n (string-length part)])
      (let loop ([i 0])
        (and (<= (+ i n) (string-length s))
             (or (string=? (substring s i (+ i n)) part) (loop (+ i 1)))))))

  (define live-bytes (foreign-procedure "chez_ts_live_bytes" () unsigned-64))

  (define (collect-all!)
    (collect (collect-maximum-generation))
    (collect (collect-maximum-generation)))

  (define (read-file path)
    (call-with-port (open-file-input-port path) get-bytevector-all))

  (define (python-example name)
    (read-file (string-append "vendor/tree-sitter-python/examples/" name))))
