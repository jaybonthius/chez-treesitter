(import (chezscheme) (tree-sitter) (check) (support))

(define (raised thunk)
  (guard (c [#t c]) (thunk) #f))

(define (chunk-reader bytes size)
  (lambda (byte point)
    (and (< byte (bytevector-length bytes))
         (let* ([n (min size (- (bytevector-length bytes) byte))] [chunk (make-bytevector n)])
           (bytevector-copy! bytes byte chunk 0 n)
           chunk))))

(define example (python-example "python3-grammar.py"))

(define (repeat-bytes bytes n)
  (let ([out (make-bytevector (* n (bytevector-length bytes)))])
    (do ([i 0 (+ i 1)]) ((= i n) out)
      (bytevector-copy! bytes 0 out (* i (bytevector-length bytes)) (bytevector-length bytes)))))

(test "a parser parses strings and bytevectors alike"
  (let ([parser (make-parser (json))])
    (check (parser? parser))
    (check (eq? (json) (parser-language parser)))
    (let ([a (parser-parse parser "{\"a\": [true, null]}")]
          [b (parser-parse parser (string->utf8 "{\"a\": [true, null]}"))])
      (check (tree? a))
      (check (eq? (json) (tree-language a)))
      (check-equal? (node-sexp (tree-root-node a)) (node-sexp (tree-root-node b))))))

(test "empty sources give an empty root"
  (check-equal? "(document)" (node-sexp (tree-root-node (parser-parse (make-parser (json)) ""))))
  (check-equal? "(module)" (node-sexp (tree-root-node (parser-parse (make-parser (python)) "")))))

(test "a tree keeps its own copy of mutable source bytes"
  (let* ([bytes (string->utf8 "[123]")]
         [root (tree-root-node (parser-parse (make-parser (json)) bytes))])
    (bytevector-u8-set! bytes 1 (char->integer #\9))
    (check-equal? "123" (node-text (node-named-child (node-child root 0) 0)))))

(test "a reader procedure gives the same tree as the whole text"
  (let ([parser (make-parser (python))]
        [expected (node-sexp (tree-root-node (parser-parse (make-parser (python)) example)))])
    (for-each
      (lambda (size)
        (check-equal? expected
          (node-sexp (tree-root-node (parser-parse parser (chunk-reader example size))))))
      '(1 7 4096 1000000))
    (let ([text (utf8->string example)])
      (check-equal? expected
        (node-sexp
          (tree-root-node
            (parser-parse parser
              (lambda (byte point)
                (and (< byte (bytevector-length example))
                     (utf8->string
                       (let ([rest (make-bytevector (- (bytevector-length example) byte))])
                         (bytevector-copy! example byte rest 0 (bytevector-length rest))
                         rest)))))))))))

(test "a reader is told the point of each byte it is asked for"
  (let* ([bytes (string->utf8 "[1,\n 2]")] [seen '()])
    (parser-parse (make-parser (json))
      (lambda (byte point)
        (set! seen (cons (cons byte point) seen))
        ((chunk-reader bytes 1) byte point)))
    (check (member (cons 5 (make-point 1 1)) seen))
    (check (member (cons 0 (make-point 0 0)) seen))))

(test "a tree parsed from a reader has no source text"
  (let ([root (tree-root-node (parser-parse (make-parser (json)) (chunk-reader (string->utf8 "[1]") 2)))])
    (check-equal? "(document (array (number)))" (node-sexp root))
    (check-raises assertion-violation? (node-text root))))

(test "a condition raised by the reader is raised by parser-parse"
  (let* ([parser (make-parser (json))]
         [failure (make-message-condition "reader failed")]
         [c (raised (lambda () (parser-parse parser (lambda (byte point) (raise failure)))))])
    (check (eq? failure c))
    (check-equal? "(document (number))" (node-sexp (tree-root-node (parser-parse parser "1"))))))

(test "a reader returning something else is refused"
  (let ([parser (make-parser (json))])
    (check-raises assertion-violation? (parser-parse parser (lambda (byte point) 'text)))
    (check-equal? "(document (number))" (node-sexp (tree-root-node (parser-parse parser "1"))))))

(test "a parser cannot be used again from inside its own parse"
  (let* ([parser (make-parser (json))]
         [c (raised (lambda ()
                      (parser-parse parser
                        (lambda (byte point) (parser-parse parser "[]") #f))))])
    (check (assertion-violation? c))
    (check (string-contains? (condition-message c) "in use"))
    (check (tree? (parser-parse parser "[]")))))

(test "another parser can be used from inside a parse"
  (let* ([inner (make-parser (json))]
         [outer (make-parser (json))]
         [texts '()]
         [tree (parser-parse outer
                 (lambda (byte point)
                   (set! texts (cons (node-sexp (tree-root-node (parser-parse inner "[]"))) texts))
                   (and (= byte 0) "[1]")))])
    (check-equal? "(document (array (number)))" (node-sexp (tree-root-node tree)))
    (check (member "(document (array))" texts))))

(test "a progress procedure returning #f halts the parse, and the next parse resumes"
  (let* ([parser (make-parser (python))]
         [big (repeat-bytes example 20)]
         [calls 0])
    (check-equal? #f
      (parser-parse parser big #f
        (lambda (byte has-error?)
          (set! calls (+ calls 1))
          (check (boolean? has-error?))
          (< byte 10000))))
    (check (> calls 1))
    (let ([resumed (parser-parse parser big)])
      (check-equal? (node-sexp (tree-root-node (parser-parse (make-parser (python)) big)))
        (node-sexp (tree-root-node resumed))))))

(test "a progress procedure that always continues gives the whole tree"
  (let ([tree (parser-parse (make-parser (python)) example #f (lambda (byte has-error?) #t))])
    (check-equal? (node-sexp (tree-root-node (parser-parse (make-parser (python)) example)))
      (node-sexp (tree-root-node tree)))))

(test "after a halted parse, a reset parser starts again"
  (let* ([parser (make-parser (python))])
    (check-equal? #f (parser-parse parser example #f (lambda (byte has-error?) #f)))
    (parser-reset! parser)
    (check-equal? "(module (expression_statement (integer)))"
      (node-sexp (tree-root-node (parser-parse parser "1\n"))))))

(test "a condition raised by the progress procedure is raised by parser-parse"
  (let* ([parser (make-parser (python))]
         [c (raised (lambda ()
                      (parser-parse parser example #f
                        (lambda (byte has-error?) (raise 'stop)))))])
    (check-equal? 'stop c)
    (check-equal? "(module (expression_statement (integer)))"
      (node-sexp (tree-root-node (parser-parse parser "1\n"))))))

(test "a logger hears parse and lex messages"
  (let ([parser (make-parser (json))] [messages '()])
    (check-equal? #f (parser-logger parser))
    (parser-logger-set! parser (lambda (type message) (set! messages (cons (cons type message) messages))))
    (check (procedure? (parser-logger parser)))
    (parser-parse parser "[1]")
    (check (assq 'parse messages))
    (check (assq 'lex messages))
    (check (for-all (lambda (m) (string? (cdr m))) messages))
    (parser-logger-set! parser #f)
    (set! messages '())
    (parser-parse parser "[1]")
    (check-equal? '() messages)))

(test "a condition raised by the logger is raised by parser-parse"
  (let ([parser (make-parser (json))])
    (parser-logger-set! parser (lambda (type message) (raise 'logged)))
    (check-equal? 'logged (raised (lambda () (parser-parse parser "[1]"))))
    (parser-logger-set! parser #f)
    (check (tree? (parser-parse parser "[1]")))))

(define (interrupt-disable-depth)
  (let ([depth (disable-interrupts)]) (enable-interrupts) depth))

(test "a continuation leaving a callback leaves interrupts and the parser usable"
  (let ([parser (make-parser (python))] [depth (interrupt-disable-depth)])
    (check-equal? 'left
      (call/cc (lambda (k) (parser-parse parser (lambda (byte point) (k 'left))))))
    (check-equal? 'left
      (call/cc (lambda (k) (parser-parse parser example #f (lambda (byte has-error?) (k 'left))))))
    (check-equal? 'left
      (call/cc
        (lambda (k)
          (parser-logger-set! parser (lambda (type message) (k 'left)))
          (parser-parse parser "1\n"))))
    (parser-logger-set! parser #f)
    (check-equal? depth (interrupt-disable-depth))
    (check-equal? 'expired
      ((make-engine (lambda () (let loop ([i 0]) (when (< i 100000000) (loop (+ i 1)))))) 1000
        (lambda (ticks value) 'completed)
        (lambda (engine) 'expired)))
    (check-equal? "(module (expression_statement (integer)))"
      (node-sexp (tree-root-node (parser-parse parser "1\n"))))))

(test "a parser changes language"
  (let ([parser (make-parser (json))])
    (parser-language-set! parser (python))
    (check (eq? (python) (parser-language parser)))
    (check-equal? "(module (expression_statement (integer)))"
      (node-sexp (tree-root-node (parser-parse parser "1\n"))))))

(test "included ranges limit what is parsed"
  (let* ([text "aa [1] bb\ncc [2] dd"]
         [ranges (list (make-range 3 6 (make-point 0 3) (make-point 0 6))
                       (make-range 13 16 (make-point 1 3) (make-point 1 6)))]
         [parser (make-parser (json))])
    (parser-included-ranges-set! parser ranges)
    (check-equal? ranges (parser-included-ranges parser))
    (let ([tree (parser-parse parser text)])
      (check-equal? "(document (array (number)) (array (number)))" (node-sexp (tree-root-node tree)))
      (check-equal? ranges (tree-included-ranges tree))
      (check-equal? "[2]" (node-text (node-named-child (tree-root-node tree) 1))))
    (parser-included-ranges-set! parser '())
    (check-equal?
      (list (make-range 0 #xffffffff (make-point 0 0) (make-point #xffffffff #xffffffff)))
      (parser-included-ranges parser))
    (check-raises assertion-violation? (parser-included-ranges-set! parser (reverse ranges)))
    (check-raises assertion-violation? (parser-included-ranges-set! parser '(1 2)))))

(test "parser procedures check their arguments"
  (let ([parser (make-parser (json))])
    (check-raises assertion-violation? (make-parser 'json))
    (check-raises assertion-violation? (parser-parse 'parser "[]"))
    (check-raises assertion-violation? (parser-parse parser 42))
    (check-raises assertion-violation? (parser-parse parser "[]" 'tree))
    (check-raises assertion-violation? (parser-parse parser "[]" #f 'continue))
    (check-raises assertion-violation? (parser-language-set! parser 'python))
    (check-raises assertion-violation? (parser-logger-set! parser 'logger))))

(test-exit)
