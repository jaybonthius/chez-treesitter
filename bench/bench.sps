(import (chezscheme) (tree-sitter) (support))

(define c-bench (car (command-line-arguments)))
(define data-directory (path-parent c-bench))

(define (seconds-per thunk)
  (thunk)
  (let ([start (current-time 'time-monotonic)])
    (let loop ([n 1])
      (thunk)
      (let* ([d (time-difference (current-time 'time-monotonic) start)]
             [elapsed (+ (time-second d) (/ (time-nanosecond d) 1e9))])
        (if (< elapsed 1.0) (loop (+ n 1)) (/ elapsed n))))))

(define (json-document count)
  (call-with-string-output-port
    (lambda (out)
      (put-string out "[\n")
      (do ([i 0 (+ i 1)]) ((= i count))
        (fprintf out "  {\"id\": ~a, \"name\": \"item ~a\", \"tags\": [\"a\", \"b\", \"c\"], \"price\": ~a.5, \"active\": ~a, \"nested\": {\"x\": [1, 2, 3], \"y\": null}}~a\n"
          i i (mod (* i 7) 1000) (if (even? i) "true" "false") (if (= i (- count 1)) "" ",")))
      (put-string out "]\n"))))

(define (repeat bytes n)
  (call-with-bytevector-output-port
    (lambda (out) (do ([i 0 (+ i 1)]) ((= i n)) (put-bytevector out bytes)))))

;; Toggling one digit keeps the tree's shape, so every reparse does the same
;; work.
(define (toggle-offset bytes)
  (define (digit? i) (<= 48 (bytevector-u8-ref bytes i) 57))
  (let loop ([i (quotient (bytevector-length bytes) 2)])
    (if (and (digit? i) (not (digit? (- i 1))) (not (digit? (+ i 1)))) i (loop (+ i 1)))))

(define (with-byte bytes offset value)
  (let ([copy (bytevector-copy bytes)])
    (bytevector-u8-set! copy offset value)
    (bytevector->immutable-bytevector copy)))

(define (write-file path bytes)
  (when (file-exists? path) (delete-file path))
  (call-with-port (open-file-output-port path) (lambda (out) (put-bytevector out bytes))))

(define (c-results name grammar source-path offset query-path)
  (let ([out (path-build data-directory (string-append name ".out"))])
    (unless (= 0 (system (format "~a ~a ~a ~a ~a 1 2 ~a > ~a" c-bench name grammar source-path offset
                           query-path out)))
      (errorf 'bench "~a failed" c-bench))
    (let ([port (open-input-file out)])
      (let loop ([acc '()])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (begin (close-port port) acc)
              (let ([fields (read (open-input-string (string-append "(" line ")")))])
                (loop (cons (cons (cadr fields) (cddr fields)) acc)))))))))

(define (cursor-count root)
  (let ([c (make-tree-cursor root)])
    (let loop ([count 1])
      (if (tree-cursor-goto-first-child! c)
          (loop (+ count 1))
          (let up ()
            (cond
              [(tree-cursor-goto-next-sibling! c) (loop (+ count 1))]
              [(tree-cursor-goto-parent! c) (up)]
              [else count]))))))

(define (children-count node)
  (fold-left (lambda (count child) (+ count (children-count child))) 1 (node-children node)))

(define (row label c scheme unit)
  (printf "  ~32a ~12,2f ~12,2f ~8,2f  ~a\n" label c scheme (/ scheme c) unit))

(define (run name language source query-path)
  (define source-path (path-build data-directory (string-append name ".source")))
  (define offset (toggle-offset source))
  (write-file source-path source)
  (let* ([c (c-results name (language-name language) source-path offset query-path)]
         [c-time (lambda (key) (car (cdr (assq key c))))]
         [megabytes (/ (bytevector-length source) 1e6)]
         [a (with-byte source offset 49)]
         [b (with-byte source offset 50)]
         [string-source (utf8->string source)]
         [parser (make-parser language)]
         [query (make-query language (utf8->string (read-file query-path)))]
         [toggle #f])
    (printf "~a: ~,2f MB\n" name megabytes)
    (printf "  ~32a ~12a ~12a ~8a\n" "" "C API" "Scheme" "ratio")
    (row "parse, MB/s" (/ megabytes (c-time 'parse))
      (/ megabytes (seconds-per (lambda () (parser-parse parser a)))) "")
    (row "parse a string, MB/s" (/ megabytes (c-time 'parse))
      (/ megabytes (seconds-per (lambda () (parser-parse parser string-source)))) "")
    (let* ([point (lambda (bytes) (let loop ([i 0] [row 0] [column 0])
                                    (if (= i offset)
                                        (make-point row column)
                                        (if (= 10 (bytevector-u8-ref bytes i))
                                            (loop (+ i 1) (+ row 1) 0)
                                            (loop (+ i 1) row (+ column 1))))))]
           [start (point source)]
           [end (make-point (point-row start) (+ 1 (point-column start)))]
           [edit (make-input-edit offset (+ offset 1) (+ offset 1) start end end)]
           [tree (parser-parse parser a)])
      (row "edit and reparse one byte, ms" (* 1e3 (c-time 'reparse))
        (* 1e3 (seconds-per
                 (lambda ()
                   (set! toggle (not toggle))
                   (set! tree (parser-parse parser (if toggle b a) (tree-edit tree edit))))))
        "")
      (printf "  ~32a ~12,2f ~12a ~8,2f  ~a\n" "  C editing a copy instead, ms" (* 1e3 (c-time 'reparse-copy))
        "" (/ (c-time 'reparse-copy) (c-time 'reparse)) "copy / in place")
      (let ([root (tree-root-node tree)] [count (caddr (assq 'walk c))])
        (unless (= count (cursor-count root)) (errorf 'bench "the walks disagree"))
        (row "walk with a tree cursor, ns/node" (/ (* 1e9 (c-time 'walk)) count)
          (/ (* 1e9 (seconds-per (lambda () (cursor-count root)))) count) "")
        (row "walk with node-children, ns/node" (/ (* 1e9 (c-time 'walk)) count)
          (/ (* 1e9 (seconds-per (lambda () (children-count root)))) count)
          (format "~a nodes" count)))
      (let* ([root (tree-root-node tree)]
             [count (caddr (assq 'captures c))]
             [kept (length (query-captures query root))])
        (row "highlight query, ns/capture" (/ (* 1e9 (c-time 'captures)) count)
          (/ (* 1e9 (seconds-per (lambda () (query-captures query root)))) kept)
          (format "~a captures in C, ~a kept by predicates" count kept))))
    (newline)))

(printf "chez-treesitter benchmark: seconds per run averaged over at least 1 s\n\n")
(run "json" (json) (string->utf8 (json-document 40000))
  (path-build (current-directory) "vendor/tree-sitter-json/queries/highlights.scm"))
(run "python" (python) (repeat (python-example "python3-grammar.py") 40)
  (path-build (current-directory) "vendor/tree-sitter-python/queries/highlights.scm"))
