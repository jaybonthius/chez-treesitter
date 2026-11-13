(library (tree-sitter)
  (export
    &language-load-error language-load-error? language-load-error-path
    language-load-error-symbol
    &language-version-error language-version-error? language-version-error-version
    &query-error query-error? query-error-kind query-error-offset query-error-row
    query-error-column

    make-point point? point-row point-column
    make-range range? range-start-byte range-end-byte range-start-point range-end-point
    make-input-edit input-edit? input-edit-start-byte input-edit-old-end-byte
    input-edit-new-end-byte input-edit-start-point input-edit-old-end-point
    input-edit-new-end-point

    load-language language? language-name language-path language-abi-version
    language-version language-symbol-count language-state-count language-field-count
    language-symbol-name language-named-symbol language-anonymous-symbol language-symbol-type
    language-field-name language-field-id language-supertypes language-subtypes
    language-next-state language-lookahead-symbols

    make-parser parser? parser-language parser-language-set! parser-included-ranges
    parser-included-ranges-set! parser-logger parser-logger-set! parser-parse
    parser-reset!

    tree? tree-language tree-root-node tree-edit tree-changed-ranges
    tree-included-ranges

    node? node=? node-tree node-language node-type node-symbol node-grammar-type
    node-grammar-symbol node-start-byte node-end-byte node-start-point node-end-point
    node-text node-sexp node-named? node-missing? node-extra? node-error?
    node-has-error? node-has-changes? node-parse-state node-next-parse-state
    node-parent node-child node-named-child node-child-count node-named-child-count
    node-children node-named-children node-child-by-field-name node-child-by-field-id
    node-children-by-field-name node-children-by-field-id node-field-name-for-child
    node-field-name-for-named-child node-next-sibling node-previous-sibling
    node-next-named-sibling node-previous-named-sibling node-first-child-for-byte
    node-first-named-child-for-byte node-child-with-descendant node-descendant-count
    node-descendant-for-byte-range node-named-descendant-for-byte-range
    node-descendant-for-point-range node-named-descendant-for-point-range

    make-tree-cursor tree-cursor? tree-cursor-node tree-cursor-field-name
    tree-cursor-field-id tree-cursor-depth tree-cursor-descendant-index
    tree-cursor-goto-parent! tree-cursor-goto-first-child! tree-cursor-goto-last-child!
    tree-cursor-goto-next-sibling! tree-cursor-goto-previous-sibling!
    tree-cursor-goto-descendant! tree-cursor-goto-first-child-for-byte!
    tree-cursor-goto-first-child-for-point! tree-cursor-reset! tree-cursor-reset-to!
    tree-cursor-copy

    make-query query? query-language query-pattern-count query-capture-count
    query-capture-names query-pattern-start-byte query-pattern-end-byte
    query-pattern-rooted? query-pattern-non-local? query-step-guaranteed?
    query-capture-quantifier query-pattern-settings query-pattern-assertions
    query-pattern-negated-assertions query-pattern-directives query-property?
    query-property-key query-property-value query-property-capture query-without-pattern
    query-without-capture query-matches query-captures
    query-match? query-match-pattern query-match-captures
    query-capture? query-capture-node query-capture-name query-capture-index
    query-capture-pattern
    make-query-cursor query-cursor? query-cursor-exec! query-cursor-next-match!
    query-cursor-next-capture! query-cursor-match-limit query-cursor-match-limit-set!
    query-cursor-match-limit-exceeded? query-cursor-byte-range-set!
    query-cursor-point-range-set! query-cursor-containing-byte-range-set!
    query-cursor-containing-point-range-set! query-cursor-max-start-depth-set!)
  (import (chezscheme))

  (define shared-object
    (unless (foreign-entry? "chez_ts_live_bytes")
      (let ([name "tree-sitter/libchez-tree-sitter.so"])
        (let loop ([directories (library-directories)])
          (cond
            [(null? directories)
             (errorf 'tree-sitter "~a is in none of the library directories ~s" name
               (library-directories))]
            [(file-exists? (path-build (cdar directories) name))
             (load-shared-object (path-build (cdar directories) name))]
            [else (loop (cdr directories))])))))

  (define-condition-type &language-load-error &error
    make-language-load-error language-load-error?
    (path language-load-error-path)
    (symbol language-load-error-symbol))

  (define-condition-type &language-version-error &language-load-error
    make-language-version-error language-version-error?
    (version language-version-error-version))

  (define-condition-type &query-error &error
    make-query-error query-error?
    (kind query-error-kind)
    (offset query-error-offset)
    (row query-error-row)
    (column query-error-column))

  (define u32-max #xffffffff)
  (define (u32? x) (and (fixnum? x) (fx<= 0 x u32-max)))
  (define (u16? x) (and (fixnum? x) (fx<= 0 x #xffff)))

  (define-syntax check
    (syntax-rules ()
      [(_ who ok? x what)
       (let ([v x]) (unless (ok? v) (assertion-violationf who "~s is not ~a" v what)))]))

  (define (check-byte-range who start end)
    (check who u32? start "an unsigned 32-bit byte offset")
    (check who u32? end "an unsigned 32-bit byte offset")
    (unless (fx<= start end)
      (assertion-violationf who "start ~s is after end ~s" start end)))

  (define (c-bytes->string address length)
    (let ([bv (make-bytevector length)])
      (%copy-bytes address bv length)
      (utf8->string bv)))

  (define (bytevector-c-string bv)
    (let loop ([n 0])
      (if (or (fx= n (bytevector-length bv)) (fx= (bytevector-u8-ref bv n) 0))
          (utf8->string (let ([s (make-bytevector n)]) (bytevector-copy! bv 0 s 0 n) s))
          (loop (fx+ n 1)))))

  (define (out-u32) (make-bytevector 4 0))
  (define (out-u32-ref bv) (bytevector-u32-native-ref bv 0))

  (define %live-bytes (foreign-procedure "chez_ts_live_bytes" () unsigned-64))
  (define %free (foreign-procedure "chez_ts_free" (uptr) void))
  (define %copy-bytes (foreign-procedure "chez_ts_copy_bytes" (uptr u8* size_t) void))

  (define-ftype ts-point
    (struct [row unsigned-32] [column unsigned-32]))
  (define-ftype ts-range
    (struct [start-point ts-point] [end-point ts-point]
            [start-byte unsigned-32] [end-byte unsigned-32]))
  (define-ftype ts-input-edit
    (struct [start-byte unsigned-32] [old-end-byte unsigned-32] [new-end-byte unsigned-32]
            [start-point ts-point] [old-end-point ts-point] [new-end-point ts-point]))
  (define-ftype ts-predicate-step
    (struct [type unsigned-32] [value-id unsigned-32]))
  (define-ftype ts-language-metadata
    (struct [major unsigned-8] [minor unsigned-8] [patch unsigned-8]))
  (define node-size 32)

  (define-syntax define-field-offset
    (syntax-rules ()
      [(_ name ftype field ...)
       (define name
         (ftype-pointer-address (ftype-&ref ftype (field ...) (make-ftype-pointer ftype 0))))]))

  (define %layout (foreign-procedure "chez_ts_layout" (unsigned-32) unsigned-32))

  ;; A tree-sitter built with other struct layouts than these would be read
  ;; wrongly, and silently, so loading refuses it.
  (define layout-checked
    (let ()
      (define-syntax offset
        (syntax-rules () [(_ ftype field ...) (let () (define-field-offset o ftype field ...) o)]))
      (for-each
        (lambda (what here index)
          (unless (= here (%layout index))
            (errorf 'tree-sitter "~a is ~a in tree-sitter's C headers but ~a here" what
              (%layout index) here)))
        '("sizeof(TSNode)" "sizeof(TSPoint)" "TSPoint.column" "sizeof(TSRange)"
          "TSRange.end_point" "TSRange.start_byte" "TSRange.end_byte" "sizeof(TSInputEdit)"
          "TSInputEdit.old_end_byte" "TSInputEdit.new_end_byte" "TSInputEdit.start_point"
          "TSInputEdit.old_end_point" "TSInputEdit.new_end_point" "sizeof(TSQueryPredicateStep)"
          "TSQueryPredicateStep.value_id" "TSLanguageMetadata.minor_version"
          "TSLanguageMetadata.patch_version" "sizeof(TSSymbol)" "sizeof(TSQueryPredicateStepType)"
          "sizeof(TSQueryError)")
        (list node-size (ftype-sizeof ts-point) (offset ts-point column) (ftype-sizeof ts-range)
          (offset ts-range end-point) (offset ts-range start-byte) (offset ts-range end-byte)
          (ftype-sizeof ts-input-edit) (offset ts-input-edit old-end-byte)
          (offset ts-input-edit new-end-byte) (offset ts-input-edit start-point)
          (offset ts-input-edit old-end-point) (offset ts-input-edit new-end-point)
          (ftype-sizeof ts-predicate-step) (offset ts-predicate-step value-id)
          (offset ts-language-metadata minor) (offset ts-language-metadata patch) 2 4 4)
        '(0 1 3 4 6 7 8 9 11 12 13 14 15 16 18 21 22 23 24 32))))

  ;; What a guardian hands back for a dropped owner: what freeing needs,
  ;; never the owner, which would otherwise stay reachable through it.
  (define-record-type native
    (nongenerative)
    (sealed #t)
    (fields address free))

  (define native-guardian (make-guardian))
  ;; Taking from a guardian is not atomic, and threads release in turn.
  (define native-mutex (make-mutex))

  (define (release-dropped!)
    (with-mutex native-mutex
      (let loop ()
        (let ([n (native-guardian)])
          (when n
            ((native-free n) (native-address n))
            (loop))))))

  (define (owned owner address free)
    (native-guardian owner (make-native address free))
    owner)

  ;; The collector does not see tree-sitter's memory, so a program that drops
  ;; large trees while allocating little in Scheme would hold them all.
  (define native-floor (* 64 1024 1024))
  (define native-limit native-floor)

  (define (check-native-memory!)
    (when (> (%live-bytes) native-limit)
      (collect-rendezvous)
      (release-dropped!)
      (set! native-limit (max native-floor (* 2 (%live-bytes))))))

  ;; Parsers and cursors have state tree-sitter does not guard, so two
  ;; threads, or a callback reentering its own parse, must not use one at
  ;; once. A box rather than a mutex: the check costs a compare-and-swap.
  (define (acquire! who busy what)
    (let loop ()
      (unless (box-cas! busy #f #t)
        (if (unbox busy)
            (assertion-violationf who "the ~a is in use by another thread or a callback" what)
            (loop))))
    (memory-order-acquire))

  (define (release! busy)
    (memory-order-release)
    (set-box! busy #f))

  (define-syntax with-exclusive
    (syntax-rules ()
      [(_ who busy-expr what body1 body2 ...)
       (let ([busy busy-expr])
         (dynamic-wind
           (lambda () (acquire! who busy what))
           (lambda () body1 body2 ...)
           (lambda () (release! busy))))]))

  ;; Without dynamic-wind, which costs a quarter of a cursor step, so the
  ;; body must not raise: its object would stay taken. A reset from a
  ;; keyboard break inside the body, or an engine that expires inside any
  ;; exclusive operation and is never resumed, leaves the object taken too.
  (define-syntax with-exclusive-step
    (syntax-rules ()
      [(_ who busy-expr what body1 body2 ...)
       (let ([busy busy-expr])
         (acquire! who busy what)
         (let ([result (let () body1 body2 ...)])
           (release! busy)
           result))]))

  (define (structural-equality! rtd)
    (let ([accessors
           (let loop ([i (fx- (vector-length (record-type-field-names rtd)) 1)] [acc '()])
             (if (fx< i 0) acc (loop (fx- i 1) (cons (record-accessor rtd i) acc))))])
      (record-type-equal-procedure rtd
        (lambda (a b equal?)
          (for-all (lambda (field) (equal? (field a) (field b))) accessors)))
      (record-type-hash-procedure rtd
        (lambda (a hash)
          (fold-left (lambda (h field) (fxlogxor (fx* 31 (fxand h #xffffff)) (hash (field a))))
            17 accessors)))))

  (define-record-type point
    (nongenerative)
    (sealed #t)
    (fields row column)
    (protocol
      (lambda (new)
        (lambda (row column)
          (check 'make-point u32? row "an unsigned 32-bit row")
          (check 'make-point u32? column "an unsigned 32-bit column")
          (new row column)))))

  (define-record-type range
    (nongenerative)
    (sealed #t)
    (fields start-byte end-byte start-point end-point)
    (protocol
      (lambda (new)
        (lambda (start-byte end-byte start-point end-point)
          (check-byte-range 'make-range start-byte end-byte)
          (check 'make-range point? start-point "a point")
          (check 'make-range point? end-point "a point")
          (new start-byte end-byte start-point end-point)))))

  (define-record-type input-edit
    (nongenerative)
    (sealed #t)
    (fields start-byte old-end-byte new-end-byte start-point old-end-point new-end-point)
    (protocol
      (lambda (new)
        (lambda (start-byte old-end-byte new-end-byte start-point old-end-point new-end-point)
          (check-byte-range 'make-input-edit start-byte old-end-byte)
          (check-byte-range 'make-input-edit start-byte new-end-byte)
          (for-each (lambda (p) (check 'make-input-edit point? p "a point"))
            (list start-point old-end-point new-end-point))
          (new start-byte old-end-byte new-end-byte start-point old-end-point
            new-end-point)))))

  (define installed-equality
    (for-each structural-equality!
      (list (record-type-descriptor point) (record-type-descriptor range)
            (record-type-descriptor input-edit))))

  (define installed-writers
    (begin
      (record-writer (record-type-descriptor point)
        (lambda (p port write)
          (fprintf port "#<point ~a:~a>" (point-row p) (point-column p))))
      (record-writer (record-type-descriptor range)
        (lambda (r port write)
          (fprintf port "#<range ~a-~a ~a:~a-~a:~a>" (range-start-byte r) (range-end-byte r)
            (point-row (range-start-point r)) (point-column (range-start-point r))
            (point-row (range-end-point r)) (point-column (range-end-point r)))))))

  (define (point-bits->point bits)
    (make-point (bitwise-arithmetic-shift-right bits 32) (bitwise-and bits u32-max)))

  (define-field-offset range-start-row ts-range start-point row)
  (define-field-offset range-start-column ts-range start-point column)
  (define-field-offset range-end-row ts-range end-point row)
  (define-field-offset range-end-column ts-range end-point column)
  (define-field-offset range-start-byte-offset ts-range start-byte)
  (define-field-offset range-end-byte-offset ts-range end-byte)

  (define (ranges->bytevector ranges)
    (let ([bv (make-bytevector (fx* (length ranges) (ftype-sizeof ts-range)) 0)])
      (let loop ([ranges ranges] [base 0])
        (unless (null? ranges)
          (let ([r (car ranges)])
            (define (put! offset value) (bytevector-u32-native-set! bv (fx+ base offset) value))
            (put! range-start-row (point-row (range-start-point r)))
            (put! range-start-column (point-column (range-start-point r)))
            (put! range-end-row (point-row (range-end-point r)))
            (put! range-end-column (point-column (range-end-point r)))
            (put! range-start-byte-offset (range-start-byte r))
            (put! range-end-byte-offset (range-end-byte r))
            (loop (cdr ranges) (fx+ base (ftype-sizeof ts-range))))))
      bv))

  (define (c-ranges address count)
    (let ([p (make-ftype-pointer ts-range address)])
      (let loop ([i (fx- count 1)] [acc '()])
        (if (fx< i 0)
            acc
            (loop (fx- i 1)
              (cons (make-range
                      (ftype-ref ts-range (start-byte) p i)
                      (ftype-ref ts-range (end-byte) p i)
                      (make-point (ftype-ref ts-range (start-point row) p i)
                                  (ftype-ref ts-range (start-point column) p i))
                      (make-point (ftype-ref ts-range (end-point row) p i)
                                  (ftype-ref ts-range (end-point column) p i)))
                    acc))))))

  (define %language-load
    (foreign-procedure "chez_ts_language_load" (utf-8 utf-8 u8* unsigned-32) uptr))
  (define %language-abi-version (foreign-procedure "ts_language_abi_version" (uptr) unsigned-32))
  (define %language-metadata (foreign-procedure "ts_language_metadata" (uptr) uptr))
  (define %language-name (foreign-procedure "ts_language_name" (uptr) utf-8))
  (define %language-symbol-count (foreign-procedure "ts_language_symbol_count" (uptr) unsigned-32))
  (define %language-state-count (foreign-procedure "ts_language_state_count" (uptr) unsigned-32))
  (define %language-field-count (foreign-procedure "ts_language_field_count" (uptr) unsigned-32))
  (define %language-symbol-name
    (foreign-procedure "ts_language_symbol_name" (uptr unsigned-16) utf-8))
  (define %language-symbol-for-name
    (foreign-procedure "ts_language_symbol_for_name" (uptr u8* unsigned-32 stdbool) unsigned-16))
  (define %language-symbol-type
    (foreign-procedure "ts_language_symbol_type" (uptr unsigned-16) int))
  (define %language-field-name
    (foreign-procedure "ts_language_field_name_for_id" (uptr unsigned-16) utf-8))
  (define %language-supertypes (foreign-procedure "ts_language_supertypes" (uptr u8*) uptr))
  (define %language-subtypes
    (foreign-procedure "ts_language_subtypes" (uptr unsigned-16 u8*) uptr))
  (define %language-next-state
    (foreign-procedure "ts_language_next_state" (uptr unsigned-16 unsigned-16) unsigned-16))
  (define %language-lookahead
    (foreign-procedure "chez_ts_language_lookahead" (uptr unsigned-16 u8* unsigned-32)
      unsigned-32))
  (define minimum-abi-version (%layout 26))
  (define maximum-abi-version (%layout 25))

  ;; Names are cached as immutable strings: node-type is called for every
  ;; node a walk visits.
  (define-record-type language
    (nongenerative)
    (sealed #t)
    (fields address name path symbol-names field-names field-ids))

  (define (c-identifier? s)
    (and (fx> (string-length s) 0)
         (for-all (lambda (c) (or (char<=? #\a c #\z) (char<=? #\A c #\Z) (char<=? #\0 c #\9)
                                  (char=? c #\_)))
           (string->list s))))

  (define (load-language path name)
    (check 'load-language string? path "a string")
    (check 'load-language (lambda (s) (and (string? s) (c-identifier? s))) name
      "a language name of letters, digits and underscores")
    (let* ([symbol (string-append "tree_sitter_" name)]
           [message (make-bytevector 1024 0)]
           [address (%language-load path symbol message (bytevector-length message))])
      (when (eqv? address 0)
        (raise
          (condition
            (make-language-load-error path symbol)
            (make-who-condition 'load-language)
            (make-message-condition
              (format "cannot load ~a from ~a: ~a" symbol path (bytevector-c-string message)))
            (make-irritants-condition (list path)))))
      (let ([version (%language-abi-version address)])
        (unless (<= minimum-abi-version version maximum-abi-version)
          (raise
            (condition
              (make-language-version-error path symbol version)
              (make-who-condition 'load-language)
              (make-message-condition
                (format "~a has ABI version ~a; this tree-sitter supports ~a to ~a" path version
                  minimum-abi-version maximum-abi-version))
              (make-irritants-condition (list path version))))))
      (let ([symbol-count (%language-symbol-count address)]
            [field-count (%language-field-count address)]
            [field-ids (make-hashtable string-hash string=?)])
        (let ([field-names (make-vector (fx+ field-count 1) #f)])
          (do ([id 1 (fx+ id 1)]) ((fx> id field-count))
            (let ([field (string->immutable-string (%language-field-name address id))])
              (vector-set! field-names id field)
              (hashtable-set! field-ids field id)))
          (make-language address (or (%language-name address) name) path
            (let ([names (make-vector symbol-count)])
              (do ([s 0 (fx+ s 1)]) ((fx= s symbol-count) names)
                (vector-set! names s
                  (string->immutable-string (or (%language-symbol-name address s) "")))))
            field-names
            (hashtable-copy field-ids #f))))))

  (define (check-language who x) (check who language? x "a language"))

  (define (language-abi-version language)
    (check-language 'language-abi-version language)
    (%language-abi-version (language-address language)))

  (define (language-version language)
    (check-language 'language-version language)
    (let ([address (%language-metadata (language-address language))])
      (and (not (eqv? address 0))
           (let ([p (make-ftype-pointer ts-language-metadata address)])
             (format "~a.~a.~a" (ftype-ref ts-language-metadata (major) p)
               (ftype-ref ts-language-metadata (minor) p)
               (ftype-ref ts-language-metadata (patch) p))))))

  (define (language-symbol-count language)
    (check-language 'language-symbol-count language)
    (vector-length (language-symbol-names language)))

  (define (language-state-count language)
    (check-language 'language-state-count language)
    (%language-state-count (language-address language)))

  (define (language-field-count language)
    (check-language 'language-field-count language)
    (fx- (vector-length (language-field-names language)) 1))

  (define (symbol-name language symbol)
    (let ([names (language-symbol-names language)])
      (if (fx< symbol (vector-length names))
          (vector-ref names symbol)
          (let ([name (%language-symbol-name (language-address language) symbol)])
            (and name (string->immutable-string name))))))

  ;; tree-sitter reads its symbol tables unchecked, and keeps its two error
  ;; symbols at the top of the range.
  (define (check-symbol who language symbol)
    (unless (and (fixnum? symbol)
                 (or (fx< -1 symbol (vector-length (language-symbol-names language)))
                     (fx= symbol #xffff) (fx= symbol #xfffe)))
      (assertion-violationf who "~s is not a symbol of ~a" symbol (language-name language))))

  (define (language-symbol-name language symbol)
    (check-language 'language-symbol-name language)
    (check-symbol 'language-symbol-name language symbol)
    (symbol-name language symbol))

  (define (symbol-for-name who language name named?)
    (check-language who language)
    (check who string? name "a string")
    (let* ([bytes (string->utf8 name)]
           [symbol (%language-symbol-for-name (language-address language) bytes
                     (bytevector-length bytes) named?)])
      (and (not (fx= symbol 0)) symbol)))

  (define (language-named-symbol language name)
    (symbol-for-name 'language-named-symbol language name #t))

  (define (language-anonymous-symbol language name)
    (symbol-for-name 'language-anonymous-symbol language name #f))

  (define (language-symbol-type language symbol)
    (check-language 'language-symbol-type language)
    (check-symbol 'language-symbol-type language symbol)
    (case (%language-symbol-type (language-address language) symbol)
      [(0) 'regular]
      [(1) 'anonymous]
      [(2) 'supertype]
      [else 'auxiliary]))

  (define (language-field-name language id)
    (check-language 'language-field-name language)
    (check 'language-field-name u16? id "a field id")
    (let ([names (language-field-names language)])
      (and (fx< id (vector-length names)) (vector-ref names id))))

  (define (language-field-id language name)
    (check-language 'language-field-id language)
    (check 'language-field-id string? name "a string")
    (hashtable-ref (language-field-ids language) name #f))

  (define (c-symbols address count)
    (let loop ([i (fx- count 1)] [acc '()])
      (if (fx< i 0)
          acc
          (loop (fx- i 1) (cons (foreign-ref 'unsigned-16 address (fx* 2 i)) acc)))))

  (define (language-supertypes language)
    (check-language 'language-supertypes language)
    (let* ([count (out-u32)] [address (%language-supertypes (language-address language) count)])
      (c-symbols address (out-u32-ref count))))

  (define (language-subtypes language supertype)
    (check-language 'language-subtypes language)
    (check-symbol 'language-subtypes language supertype)
    (let* ([count (out-u32)]
           [address (%language-subtypes (language-address language) supertype count)])
      (c-symbols address (out-u32-ref count))))

  (define (language-next-state language state symbol)
    (check-language 'language-next-state language)
    (check 'language-next-state u16? state "a parse state")
    (check-symbol 'language-next-state language symbol)
    (%language-next-state (language-address language) state symbol))

  (define (language-lookahead-symbols language state)
    (check-language 'language-lookahead-symbols language)
    (check 'language-lookahead-symbols u16? state "a parse state")
    (let loop ([capacity (fx+ (vector-length (language-symbol-names language)) 2)])
      (let* ([out (make-bytevector (fx* 2 capacity))]
             [count (%language-lookahead (language-address language) state out capacity)])
        (if (fx> count capacity)
            (loop count)
            (let collect ([i (fx- count 1)] [acc '()])
              (if (fx< i 0)
                  acc
                  (collect (fx- i 1) (cons (bytevector-u16-native-ref out (fx* 2 i)) acc))))))))

  (define %tree-delete (foreign-procedure "ts_tree_delete" (uptr) void))
  (define %tree-root-node (foreign-procedure "chez_ts_tree_root_node" (uptr u8*) void))
  (define %tree-edited-copy (foreign-procedure "chez_ts_tree_edited_copy" (uptr u8*) uptr))
  (define %tree-changed-ranges
    (foreign-procedure "ts_tree_get_changed_ranges" (uptr uptr u8*) uptr))
  (define %tree-included-ranges (foreign-procedure "ts_tree_included_ranges" (uptr u8*) uptr))

  ;; Trees are never changed once made: tree-edit edits a copy. A node then
  ;; stays valid as long as its tree, and threads can share a tree, as
  ;; tree-sitter allows for trees no one is editing.
  (define-record-type tree
    (nongenerative)
    (sealed #t)
    (fields address language source))

  (define (make-owned-tree address language source)
    (owned (make-tree address language source) address %tree-delete))

  (define (check-tree who x) (check who tree? x "a tree"))

  ;; raw holds a TSNode, which points into its tree without owning it.
  (define-record-type node
    (nongenerative)
    (sealed #t)
    (fields tree raw))

  ;; keep-live: a collection between reading the node's bytes and the call
  ;; could otherwise drop the last reference to the tree, and another thread
  ;; free it while C reads it.
  (define-syntax with-node
    (syntax-rules ()
      [(_ who n (raw) body1 body2 ...)
       (identifier? #'n)
       (begin
         (check who node? n "a node")
         (let* ([raw (node-raw n)] [result (let () body1 body2 ...)])
           (keep-live n)
           result))]))

  (define-syntax new-node
    (syntax-rules ()
      [(_ tree (proc arg ...))
       (let ([out (make-bytevector node-size)])
         (and (proc arg ... out) (make-node tree out)))]))

  (define (tree-root-node tree)
    (check-tree 'tree-root-node tree)
    (let ([out (make-bytevector node-size)])
      (%tree-root-node (tree-address tree) out)
      (make-node tree out)))

  (define (input-edit->bytevector edit)
    (let ([bv (make-bytevector (ftype-sizeof ts-input-edit) 0)])
      (define-syntax put!
        (syntax-rules ()
          [(_ value field ...)
           (bytevector-u32-native-set! bv
             (ftype-pointer-address
               (ftype-&ref ts-input-edit (field ...) (make-ftype-pointer ts-input-edit 0)))
             value)]))
      (put! (input-edit-start-byte edit) start-byte)
      (put! (input-edit-old-end-byte edit) old-end-byte)
      (put! (input-edit-new-end-byte edit) new-end-byte)
      (put! (point-row (input-edit-start-point edit)) start-point row)
      (put! (point-column (input-edit-start-point edit)) start-point column)
      (put! (point-row (input-edit-old-end-point edit)) old-end-point row)
      (put! (point-column (input-edit-old-end-point edit)) old-end-point column)
      (put! (point-row (input-edit-new-end-point edit)) new-end-point row)
      (put! (point-column (input-edit-new-end-point edit)) new-end-point column)
      bv))

  ;; The edited tree's positions describe text the caller has and this
  ;; library does not, so it has no source.
  (define (tree-edit tree edit)
    (check-tree 'tree-edit tree)
    (check 'tree-edit input-edit? edit "an input edit")
    (release-dropped!)
    (let ([address (%tree-edited-copy (tree-address tree) (input-edit->bytevector edit))])
      (keep-live tree)
      (let ([edited (make-owned-tree address (tree-language tree) #f)])
        (check-native-memory!)
        edited)))

  ;; tree-sitter reads one tree's symbols in the other's tables.
  (define (check-same-language who a b)
    (unless (eqv? (language-address a) (language-address b))
      (assertion-violationf who "a tree of ~a and a tree of ~a" (language-name a)
        (language-name b))))

  (define (tree-changed-ranges old-tree new-tree)
    (check-tree 'tree-changed-ranges old-tree)
    (check-tree 'tree-changed-ranges new-tree)
    (check-same-language 'tree-changed-ranges (tree-language old-tree) (tree-language new-tree))
    (let* ([count (out-u32)]
           [address (%tree-changed-ranges (tree-address old-tree) (tree-address new-tree) count)])
      (keep-live old-tree)
      (keep-live new-tree)
      (let ([ranges (c-ranges address (out-u32-ref count))])
        (%free address)
        ranges)))

  (define (tree-included-ranges tree)
    (check-tree 'tree-included-ranges tree)
    (let* ([count (out-u32)] [address (%tree-included-ranges (tree-address tree) count)])
      (keep-live tree)
      (let ([ranges (c-ranges address (out-u32-ref count))])
        (%free address)
        ranges)))

  (define %node-symbol (foreign-procedure "chez_ts_node_symbol" (u8*) unsigned-16))
  (define %node-grammar-symbol
    (foreign-procedure "chez_ts_node_grammar_symbol" (u8*) unsigned-16))
  (define %node-start-byte (foreign-procedure "chez_ts_node_start_byte" (u8*) unsigned-32))
  (define %node-end-byte (foreign-procedure "chez_ts_node_end_byte" (u8*) unsigned-32))
  (define %node-start-point (foreign-procedure "chez_ts_node_start_point" (u8*) unsigned-64))
  (define %node-end-point (foreign-procedure "chez_ts_node_end_point" (u8*) unsigned-64))
  (define %node-named? (foreign-procedure "chez_ts_node_is_named" (u8*) stdbool))
  (define %node-missing? (foreign-procedure "chez_ts_node_is_missing" (u8*) stdbool))
  (define %node-extra? (foreign-procedure "chez_ts_node_is_extra" (u8*) stdbool))
  (define %node-error? (foreign-procedure "chez_ts_node_is_error" (u8*) stdbool))
  (define %node-has-changes? (foreign-procedure "chez_ts_node_has_changes" (u8*) stdbool))
  (define %node-has-error? (foreign-procedure "chez_ts_node_has_error" (u8*) stdbool))
  (define %node-parse-state (foreign-procedure "chez_ts_node_parse_state" (u8*) unsigned-16))
  (define %node-next-parse-state
    (foreign-procedure "chez_ts_node_next_parse_state" (u8*) unsigned-16))
  (define %node-child-count (foreign-procedure "chez_ts_node_child_count" (u8*) unsigned-32))
  (define %node-named-child-count
    (foreign-procedure "chez_ts_node_named_child_count" (u8*) unsigned-32))
  (define %node-descendant-count
    (foreign-procedure "chez_ts_node_descendant_count" (u8*) unsigned-32))
  (define %node-parent (foreign-procedure "chez_ts_node_parent" (u8* u8*) boolean))
  (define %node-next-sibling (foreign-procedure "chez_ts_node_next_sibling" (u8* u8*) boolean))
  (define %node-prev-sibling (foreign-procedure "chez_ts_node_prev_sibling" (u8* u8*) boolean))
  (define %node-next-named-sibling
    (foreign-procedure "chez_ts_node_next_named_sibling" (u8* u8*) boolean))
  (define %node-prev-named-sibling
    (foreign-procedure "chez_ts_node_prev_named_sibling" (u8* u8*) boolean))
  (define %node-child (foreign-procedure "chez_ts_node_child" (u8* unsigned-32 u8*) boolean))
  (define %node-named-child
    (foreign-procedure "chez_ts_node_named_child" (u8* unsigned-32 u8*) boolean))
  (define %node-first-child-for-byte
    (foreign-procedure "chez_ts_node_first_child_for_byte" (u8* unsigned-32 u8*) boolean))
  (define %node-first-named-child-for-byte
    (foreign-procedure "chez_ts_node_first_named_child_for_byte" (u8* unsigned-32 u8*) boolean))
  (define %node-child-by-field-id
    (foreign-procedure "chez_ts_node_child_by_field_id" (u8* unsigned-16 u8*) boolean))
  (define %node-child-with-descendant
    (foreign-procedure "chez_ts_node_child_with_descendant" (u8* u8* u8*) boolean))
  (define %node-descendant-for-byte-range
    (foreign-procedure "chez_ts_node_descendant_for_byte_range"
      (u8* unsigned-32 unsigned-32 boolean u8*) boolean))
  (define %node-descendant-for-point-range
    (foreign-procedure "chez_ts_node_descendant_for_point_range"
      (u8* unsigned-32 unsigned-32 unsigned-32 unsigned-32 boolean u8*) boolean))
  (define %node-field-id-for-child
    (foreign-procedure "chez_ts_node_field_id_for_child" (u8* boolean unsigned-32) unsigned-16))
  (define %node-eq (foreign-procedure "chez_ts_node_eq" (u8* u8*) boolean))
  (define %node-string (foreign-procedure "chez_ts_node_string" (u8* u8*) uptr))
  (define %node-children
    (foreign-procedure "chez_ts_node_children" (u8* boolean unsigned-16 u8* unsigned-32)
      unsigned-32))

  (define (node=? a b)
    (with-node 'node=? a (raw-a)
      (with-node 'node=? b (raw-b)
        (%node-eq raw-a raw-b))))

  (define installed-node-equality
    (let ([rtd (record-type-descriptor node)])
      (record-type-equal-procedure rtd
        (lambda (a b equal?)
          (let ([same? (%node-eq (node-raw a) (node-raw b))])
            (keep-live a)
            (keep-live b)
            same?)))
      (record-type-hash-procedure rtd
        (lambda (n hash)
          (let* ([raw (node-raw n)]
                 [h (fxlogxor (hash (%node-start-byte raw)) (hash (%node-symbol raw)))])
            (keep-live n)
            h)))
      (record-writer rtd
        (lambda (n port write)
          (fprintf port "#<node ~a ~a-~a>" (node-type n) (node-start-byte n) (node-end-byte n))))))

  (define (node-language n)
    (check 'node-language node? n "a node")
    (tree-language (node-tree n)))

  (define (node-type n)
    (with-node 'node-type n (raw)
      (symbol-name (tree-language (node-tree n)) (%node-symbol raw))))

  (define (node-grammar-type n)
    (with-node 'node-grammar-type n (raw)
      (symbol-name (tree-language (node-tree n)) (%node-grammar-symbol raw))))

  (define-syntax define-node-getter
    (syntax-rules ()
      [(_ name proc) (define (name n) (with-node 'name n (raw) (proc raw)))]))

  (define-node-getter node-symbol %node-symbol)
  (define-node-getter node-grammar-symbol %node-grammar-symbol)
  (define-node-getter node-start-byte %node-start-byte)
  (define-node-getter node-end-byte %node-end-byte)
  (define-node-getter node-named? %node-named?)
  (define-node-getter node-missing? %node-missing?)
  (define-node-getter node-extra? %node-extra?)
  (define-node-getter node-error? %node-error?)
  (define-node-getter node-has-changes? %node-has-changes?)
  (define-node-getter node-has-error? %node-has-error?)
  (define-node-getter node-parse-state %node-parse-state)
  (define-node-getter node-next-parse-state %node-next-parse-state)
  (define-node-getter node-child-count %node-child-count)
  (define-node-getter node-named-child-count %node-named-child-count)
  (define-node-getter node-descendant-count %node-descendant-count)

  (define (node-start-point n)
    (point-bits->point (with-node 'node-start-point n (raw) (%node-start-point raw))))

  (define (node-end-point n)
    (point-bits->point (with-node 'node-end-point n (raw) (%node-end-point raw))))

  (define (source-text who source start end)
    (unless source
      (assertion-violationf who
        "the tree has no source text: it was edited, or parsed from a procedure"))
    (unless (fx<= end (bytevector-length source))
      (assertion-violationf who "the source text ends at byte ~s, before ~s"
        (bytevector-length source) end))
    (let ([bv (make-bytevector (fx- end start))])
      (bytevector-copy! source start bv 0 (fx- end start))
      (utf8->string bv)))

  (define (node-text n)
    (with-node 'node-text n (raw)
      (source-text 'node-text (tree-source (node-tree n)) (%node-start-byte raw)
        (%node-end-byte raw))))

  (define (node-sexp n)
    (with-node 'node-sexp n (raw)
      (let* ([size (make-bytevector 8)]
             [address (%node-string raw size)]
             [text (c-bytes->string address (bytevector-u64-native-ref size 0))])
        (%free address)
        text)))

  (define-syntax define-node-step
    (syntax-rules ()
      [(_ name proc)
       (define (name n) (with-node 'name n (raw) (new-node (node-tree n) (proc raw))))]))

  (define-node-step node-parent %node-parent)
  (define-node-step node-next-sibling %node-next-sibling)
  (define-node-step node-previous-sibling %node-prev-sibling)
  (define-node-step node-next-named-sibling %node-next-named-sibling)
  (define-node-step node-previous-named-sibling %node-prev-named-sibling)

  (define-syntax define-node-index-step
    (syntax-rules ()
      [(_ name proc what)
       (define (name n i)
         (check 'name u32? i what)
         (with-node 'name n (raw) (new-node (node-tree n) (proc raw i))))]))

  (define-node-index-step node-child %node-child "a child index")
  (define-node-index-step node-named-child %node-named-child "a child index")
  (define-node-index-step node-first-child-for-byte %node-first-child-for-byte "a byte offset")
  (define-node-index-step node-first-named-child-for-byte %node-first-named-child-for-byte
    "a byte offset")

  (define (field-id who n name)
    (check who string? name "a string")
    (or (hashtable-ref (language-field-ids (node-language n)) name #f)
        (assertion-violationf who "~a has no field ~s" (language-name (node-language n)) name)))

  (define (node-child-by-field-id n id)
    (check 'node-child-by-field-id u16? id "a field id")
    (with-node 'node-child-by-field-id n (raw)
      (new-node (node-tree n) (%node-child-by-field-id raw id))))

  (define (node-child-by-field-name n name)
    (check 'node-child-by-field-name node? n "a node")
    (node-child-by-field-id n (field-id 'node-child-by-field-name n name)))

  (define (children who n named? field)
    (with-node who n (raw)
      (let* ([capacity (%node-child-count raw)]
             [out (make-bytevector (fx* node-size capacity))]
             [count (%node-children raw named? field out capacity)]
             [tree (node-tree n)])
        (let loop ([i (fx- count 1)] [acc '()])
          (if (fx< i 0)
              acc
              (let ([child (make-bytevector node-size)])
                (bytevector-copy! out (fx* i node-size) child 0 node-size)
                (loop (fx- i 1) (cons (make-node tree child) acc))))))))

  (define (node-children n) (children 'node-children n #f 0))
  (define (node-named-children n) (children 'node-named-children n #t 0))

  (define (node-children-by-field-id n id)
    (check 'node-children-by-field-id (lambda (x) (and (u16? x) (fx> x 0))) id "a field id")
    (children 'node-children-by-field-id n #f id))

  (define (node-children-by-field-name n name)
    (check 'node-children-by-field-name node? n "a node")
    (children 'node-children-by-field-name n #f (field-id 'node-children-by-field-name n name)))

  (define (field-name-for-child who n named? i)
    (check who u32? i "a child index")
    (with-node who n (raw)
      (let ([id (%node-field-id-for-child raw named? i)])
        (and (not (fx= id 0)) (vector-ref (language-field-names (node-language n)) id)))))

  (define (node-field-name-for-child n i)
    (field-name-for-child 'node-field-name-for-child n #f i))

  (define (node-field-name-for-named-child n i)
    (field-name-for-child 'node-field-name-for-named-child n #t i))

  (define (node-child-with-descendant n descendant)
    (with-node 'node-child-with-descendant n (raw)
      (with-node 'node-child-with-descendant descendant (descendant-raw)
        (unless (eq? (node-tree n) (node-tree descendant))
          (assertion-violationf 'node-child-with-descendant "~s and ~s are in different trees"
            n descendant))
        (new-node (node-tree n) (%node-child-with-descendant raw descendant-raw)))))

  (define (descendant-for-byte-range who n start end named?)
    (check-byte-range who start end)
    (with-node who n (raw)
      (new-node (node-tree n) (%node-descendant-for-byte-range raw start end named?))))

  (define (node-descendant-for-byte-range n start end)
    (descendant-for-byte-range 'node-descendant-for-byte-range n start end #f))

  (define (node-named-descendant-for-byte-range n start end)
    (descendant-for-byte-range 'node-named-descendant-for-byte-range n start end #t))

  (define (descendant-for-point-range who n start end named?)
    (check who point? start "a point")
    (check who point? end "a point")
    (with-node who n (raw)
      (new-node (node-tree n)
        (%node-descendant-for-point-range raw (point-row start) (point-column start)
          (point-row end) (point-column end) named?))))

  (define (node-descendant-for-point-range n start end)
    (descendant-for-point-range 'node-descendant-for-point-range n start end #f))

  (define (node-named-descendant-for-point-range n start end)
    (descendant-for-point-range 'node-named-descendant-for-point-range n start end #t))

  (define %parser-new (foreign-procedure "ts_parser_new" () uptr))
  (define %parser-delete (foreign-procedure "ts_parser_delete" (uptr) void))
  (define %parser-set-language
    (foreign-procedure "ts_parser_set_language" (uptr uptr) stdbool))
  (define %parser-reset (foreign-procedure "ts_parser_reset" (uptr) void))
  (define %parser-set-included-ranges
    (foreign-procedure "ts_parser_set_included_ranges" (uptr u8* unsigned-32) stdbool))
  (define %parser-included-ranges
    (foreign-procedure "ts_parser_included_ranges" (uptr u8*) uptr))
  (define %parser-set-logger (foreign-procedure "chez_ts_parser_set_logger" (uptr uptr) void))
  ;; Collect-safe, so that other threads can collect during a long parse.
  (define %parse-bytes
    (foreign-procedure __collect_safe "chez_ts_parse_bytes" (uptr uptr u8* unsigned-32 uptr)
      uptr))
  (define %parse-reader
    (foreign-procedure __collect_safe "chez_ts_parse_reader" (uptr uptr uptr uptr) uptr))

  (define-record-type (parser %make-parser parser?)
    (nongenerative)
    (sealed #t)
    (fields address (mutable language parser-language %parser-language-set!)
      (mutable logger parser-logger %parser-logger-set!) busy))

  (define (check-parser who x) (check who parser? x "a parser"))

  (define (make-parser language)
    (check-language 'make-parser language)
    (release-dropped!)
    (let* ([address (%parser-new)]
           [parser (owned (%make-parser address language #f (box #f)) address %parser-delete)])
      (%parser-set-language address (language-address language))
      parser))

  (define (parser-language-set! parser language)
    (check-parser 'parser-language-set! parser)
    (check-language 'parser-language-set! language)
    (with-exclusive 'parser-language-set! (parser-busy parser) "parser"
      (%parser-set-language (parser-address parser) (language-address language))
      (%parser-language-set! parser language)
      (keep-live parser)))

  (define (parser-reset! parser)
    (check-parser 'parser-reset! parser)
    (with-exclusive 'parser-reset! (parser-busy parser) "parser"
      (%parser-reset (parser-address parser))
      (keep-live parser)))

  (define (parser-included-ranges parser)
    (check-parser 'parser-included-ranges parser)
    (with-exclusive 'parser-included-ranges (parser-busy parser) "parser"
      (let* ([count (out-u32)]
             [address (%parser-included-ranges (parser-address parser) count)]
             [ranges (c-ranges address (out-u32-ref count))])
        (keep-live parser)
        ranges)))

  (define (parser-included-ranges-set! parser ranges)
    (check-parser 'parser-included-ranges-set! parser)
    (check 'parser-included-ranges-set! (lambda (x) (and (list? x) (for-all range? x))) ranges
      "a list of ranges")
    (with-exclusive 'parser-included-ranges-set! (parser-busy parser) "parser"
      (let ([ok? (%parser-set-included-ranges (parser-address parser)
                   (ranges->bytevector ranges) (length ranges))])
        (keep-live parser)
        (unless ok?
          (assertion-violationf 'parser-included-ranges-set!
            "the ranges ~s overlap or are out of order" ranges)))))

  (define (parser-logger-set! parser logger)
    (check-parser 'parser-logger-set! parser)
    (check 'parser-logger-set! (lambda (x) (or (not x) (procedure? x))) logger
      "a procedure or #f")
    (with-exclusive 'parser-logger-set! (parser-busy parser) "parser"
      (%parser-logger-set! parser logger)))

  (define parser-parse
    (case-lambda
      [(parser source) (parse parser source #f #f)]
      [(parser source old-tree) (parse parser source old-tree #f)]
      [(parser source old-tree continue?) (parse parser source old-tree continue?)]))

  ;; A copy of mutable bytes, so that the tree's text cannot change under it.
  (define (source-bytes who source)
    (let ([bytes (cond
                   [(string? source) (string->utf8 source)]
                   [(bytevector? source)
                    (if (immutable-bytevector? source) source (bytevector-copy source))]
                   [else #f])])
      (when (and bytes (> (bytevector-length bytes) u32-max))
        (assertion-violationf who "the source is ~s bytes, more than tree-sitter's limit"
          (bytevector-length bytes)))
      bytes))

  (define (parse parser source old-tree continue?)
    (define who 'parser-parse)
    (check-parser who parser)
    (check who (lambda (x) (or (string? x) (bytevector? x) (procedure? x))) source
      "a string, bytevector or procedure")
    (check who (lambda (x) (or (not x) (tree? x))) old-tree "a tree or #f")
    (check who (lambda (x) (or (not x) (procedure? x))) continue? "a procedure or #f")
    (release-dropped!)
    (let* ([bytes (source-bytes who source)]
           [address (with-exclusive who (parser-busy parser) "parser"
                      (run-parser parser bytes source old-tree continue?))])
      (and (not (eqv? address 0))
           (let ([tree (make-owned-tree address (parser-language parser) bytes)])
             (check-native-memory!)
             tree))))

  (define empty-chunk (make-immobile-bytevector 1 0))

  ;; __disable_interrupts enables interrupts again only when the callable
  ;; returns, so a continuation that leaves it would keep them off for good:
  ;; no more collections, timers or engines on the thread.
  (define (callback-body thunk)
    (let ([returned? #f])
      (dynamic-wind
        void
        (lambda () (let ([v (thunk)]) (set! returned? #t) v))
        (lambda () (unless returned? (enable-interrupts))))))

  ;; Callbacks run inside tree-sitter's C frames, which a raise must not
  ;; unwind: each callback catches what it raises, ends the parse, and the
  ;; condition is raised again once tree-sitter has returned. Interrupts stay
  ;; off inside them so that no timer or keyboard handler unwinds them either.
  (define (run-parser parser bytes source old-tree continue?)
    (when old-tree
      (check-same-language 'parser-parse (tree-language old-tree) (parser-language parser)))
    (let ([failure #f] [locked '()] [chunk #f] [logger (parser-logger parser)]
          [address (parser-address parser)] [finished? #f])
      (define who 'parser-parse)
      (define (fail! c) (unless failure (set! failure c)))
      (define (callback-entry code)
        (lock-object code)
        (set! locked (cons code locked))
        (foreign-callable-entry-point code))
      (define (release-chunk!)
        (when chunk (unlock-object chunk) (set! chunk #f)))
      (define (read-chunk byte row column)
        (let* ([text (source byte (make-point row column))]
               [bytes (cond
                        [(not text) #f]
                        [(string? text) (string->utf8 text)]
                        [(bytevector? text) text]
                        [else (assertion-violationf who
                                "the reader returned ~s, not a string, bytevector or #f" text)])])
          (when (and bytes (> (bytevector-length bytes) u32-max))
            (assertion-violationf who "the reader returned ~s bytes, more than tree-sitter's limit"
              (bytevector-length bytes)))
          bytes))
      (define progress
        (if continue?
            (callback-entry
              (foreign-callable __collect_safe __disable_interrupts
                (lambda (byte has-error)
                  (callback-body
                    (lambda ()
                      (and (not failure)
                           (guard (c [#t (fail! c) #f])
                             (and (continue? byte (not (fx= has-error 0))) #t))))))
                (unsigned-32 int) boolean))
            0))
      (define reader
        (if bytes
            0
            (callback-entry
              (foreign-callable __collect_safe __disable_interrupts
                (lambda (byte row column bytes-read)
                  (callback-body
                    (lambda ()
                      (release-chunk!)
                      (let ([next (and (not failure)
                                       (guard (c [#t (fail! c) #f])
                                         (read-chunk byte row column)))])
                        (cond
                          [(and next (fx> (bytevector-length next) 0))
                           (lock-object next)
                           (set! chunk next)
                           (foreign-set! 'unsigned-32 bytes-read 0 (bytevector-length next))
                           (object->reference-address next)]
                          [else
                           (foreign-set! 'unsigned-32 bytes-read 0 0)
                           (object->reference-address empty-chunk)])))))
                (unsigned-32 unsigned-32 unsigned-32 uptr) uptr))))
      (define result
        (dynamic-wind
          (lambda () (when bytes (lock-object bytes)))
          (lambda ()
            (when logger
              (%parser-set-logger address
                (callback-entry
                  (foreign-callable __collect_safe __disable_interrupts
                    (lambda (lex? message)
                      (callback-body
                        (lambda ()
                          (unless failure
                            (guard (c [#t (fail! c)])
                              (logger (if (fx= lex? 0) 'parse 'lex) message))))))
                    (int utf-8) void))))
            (let ([old (if old-tree (tree-address old-tree) 0)])
              (let ([tree (if bytes
                              (%parse-bytes address old bytes (bytevector-length bytes) progress)
                              (%parse-reader address old reader progress))])
                (keep-live old-tree)
                (set! finished? #t)
                tree)))
          (lambda ()
            (when bytes (unlock-object bytes))
            (when logger (%parser-set-logger address 0))
            (release-chunk!)
            (for-each unlock-object locked)
            (unless finished? (%parser-reset address))
            (keep-live parser))))
      (when failure
        (unless (eqv? result 0) (%tree-delete result))
        (%parser-reset address)
        (raise failure))
      result))

  (define %tree-cursor-new (foreign-procedure "chez_ts_tree_cursor_new" (u8*) uptr))
  (define %tree-cursor-copy (foreign-procedure "chez_ts_tree_cursor_copy" (uptr) uptr))
  (define %tree-cursor-delete (foreign-procedure "chez_ts_tree_cursor_delete" (uptr) void))
  (define %tree-cursor-reset (foreign-procedure "chez_ts_tree_cursor_reset" (uptr u8*) void))
  (define %tree-cursor-reset-to (foreign-procedure "ts_tree_cursor_reset_to" (uptr uptr) void))
  (define %tree-cursor-node
    (foreign-procedure "chez_ts_tree_cursor_current_node" (uptr u8*) void))
  (define %tree-cursor-field-id
    (foreign-procedure "ts_tree_cursor_current_field_id" (uptr) unsigned-16))
  (define %tree-cursor-depth
    (foreign-procedure "ts_tree_cursor_current_depth" (uptr) unsigned-32))
  (define %tree-cursor-descendant-index
    (foreign-procedure "ts_tree_cursor_current_descendant_index" (uptr) unsigned-32))
  (define %tree-cursor-goto-parent
    (foreign-procedure "ts_tree_cursor_goto_parent" (uptr) stdbool))
  (define %tree-cursor-goto-first-child
    (foreign-procedure "ts_tree_cursor_goto_first_child" (uptr) stdbool))
  (define %tree-cursor-goto-last-child
    (foreign-procedure "ts_tree_cursor_goto_last_child" (uptr) stdbool))
  (define %tree-cursor-goto-next-sibling
    (foreign-procedure "ts_tree_cursor_goto_next_sibling" (uptr) stdbool))
  (define %tree-cursor-goto-previous-sibling
    (foreign-procedure "ts_tree_cursor_goto_previous_sibling" (uptr) stdbool))
  (define %tree-cursor-goto-descendant
    (foreign-procedure "ts_tree_cursor_goto_descendant" (uptr unsigned-32) void))
  (define %tree-cursor-goto-first-child-for-byte
    (foreign-procedure "ts_tree_cursor_goto_first_child_for_byte" (uptr unsigned-32) integer-64))
  (define %tree-cursor-goto-first-child-for-point
    (foreign-procedure "chez_ts_tree_cursor_goto_first_child_for_point"
      (uptr unsigned-32 unsigned-32) integer-64))

  (define-record-type (tree-cursor %make-tree-cursor tree-cursor?)
    (nongenerative)
    (sealed #t)
    (fields address (mutable tree) busy))

  (define (owned-tree-cursor address tree)
    (owned (%make-tree-cursor address tree (box #f)) address %tree-cursor-delete))

  (define (make-tree-cursor n)
    (release-dropped!)
    (with-node 'make-tree-cursor n (raw)
      (owned-tree-cursor (%tree-cursor-new raw) (node-tree n))))

  (define-syntax with-tree-cursor
    (syntax-rules ()
      [(_ who c (address) body1 body2 ...)
       (identifier? #'c)
       (begin
         (check who tree-cursor? c "a tree cursor")
         (with-exclusive-step who (tree-cursor-busy c) "tree cursor"
           (let* ([address (tree-cursor-address c)] [result (let () body1 body2 ...)])
             (keep-live c)
             result)))]))

  (define (tree-cursor-node c)
    (with-tree-cursor 'tree-cursor-node c (address)
      (let ([out (make-bytevector node-size)])
        (%tree-cursor-node address out)
        (make-node (tree-cursor-tree c) out))))

  (define (tree-cursor-field-id c)
    (with-tree-cursor 'tree-cursor-field-id c (address)
      (let ([id (%tree-cursor-field-id address)])
        (and (not (fx= id 0)) id))))

  (define (tree-cursor-field-name c)
    (check 'tree-cursor-field-name tree-cursor? c "a tree cursor")
    (let ([id (tree-cursor-field-id c)])
      (and id (vector-ref (language-field-names (tree-language (tree-cursor-tree c))) id))))

  (define-syntax define-tree-cursor-operation
    (syntax-rules ()
      [(_ name proc)
       (define (name c) (with-tree-cursor 'name c (address) (proc address)))]))

  (define-tree-cursor-operation tree-cursor-depth %tree-cursor-depth)
  (define-tree-cursor-operation tree-cursor-descendant-index %tree-cursor-descendant-index)
  (define-tree-cursor-operation tree-cursor-goto-parent! %tree-cursor-goto-parent)
  (define-tree-cursor-operation tree-cursor-goto-first-child! %tree-cursor-goto-first-child)
  (define-tree-cursor-operation tree-cursor-goto-last-child! %tree-cursor-goto-last-child)
  (define-tree-cursor-operation tree-cursor-goto-next-sibling! %tree-cursor-goto-next-sibling)
  (define-tree-cursor-operation tree-cursor-goto-previous-sibling!
    %tree-cursor-goto-previous-sibling)

  (define (tree-cursor-goto-descendant! c index)
    (check 'tree-cursor-goto-descendant! u32? index "a descendant index")
    (with-tree-cursor 'tree-cursor-goto-descendant! c (address)
      (%tree-cursor-goto-descendant address index)))

  (define (tree-cursor-goto-first-child-for-byte! c byte)
    (check 'tree-cursor-goto-first-child-for-byte! u32? byte "a byte offset")
    (with-tree-cursor 'tree-cursor-goto-first-child-for-byte! c (address)
      (let ([index (%tree-cursor-goto-first-child-for-byte address byte)])
        (and (>= index 0) index))))

  (define (tree-cursor-goto-first-child-for-point! c point)
    (check 'tree-cursor-goto-first-child-for-point! point? point "a point")
    (with-tree-cursor 'tree-cursor-goto-first-child-for-point! c (address)
      (let ([index (%tree-cursor-goto-first-child-for-point address (point-row point)
                     (point-column point))])
        (and (>= index 0) index))))

  (define (tree-cursor-reset! c n)
    (with-node 'tree-cursor-reset! n (raw)
      (with-tree-cursor 'tree-cursor-reset! c (address)
        (%tree-cursor-reset address raw)
        (tree-cursor-tree-set! c (node-tree n)))))

  (define (tree-cursor-reset-to! c source)
    (check 'tree-cursor-reset-to! tree-cursor? source "a tree cursor")
    (check 'tree-cursor-reset-to! tree-cursor? c "a tree cursor")
    (unless (eq? c source)
      (with-exclusive 'tree-cursor-reset-to! (tree-cursor-busy source) "tree cursor"
        (with-tree-cursor 'tree-cursor-reset-to! c (address)
          (%tree-cursor-reset-to address (tree-cursor-address source))
          (tree-cursor-tree-set! c (tree-cursor-tree source))
          (keep-live source)))))

  (define (tree-cursor-copy c)
    (release-dropped!)
    (with-tree-cursor 'tree-cursor-copy c (address)
      (owned-tree-cursor (%tree-cursor-copy address) (tree-cursor-tree c))))

  (define %query-new
    (foreign-procedure "ts_query_new" (uptr u8* unsigned-32 u8* u8*) uptr))
  (define %query-delete (foreign-procedure "ts_query_delete" (uptr) void))
  (define %query-copy-without
    (foreign-procedure "chez_ts_query_copy_without" (uptr boolean unsigned-32 u8* unsigned-32)
      uptr))
  (define %query-pattern-count (foreign-procedure "ts_query_pattern_count" (uptr) unsigned-32))
  (define %query-capture-count (foreign-procedure "ts_query_capture_count" (uptr) unsigned-32))
  (define %query-string-count (foreign-procedure "ts_query_string_count" (uptr) unsigned-32))
  (define %query-start-byte
    (foreign-procedure "ts_query_start_byte_for_pattern" (uptr unsigned-32) unsigned-32))
  (define %query-end-byte
    (foreign-procedure "ts_query_end_byte_for_pattern" (uptr unsigned-32) unsigned-32))
  (define %query-predicates
    (foreign-procedure "ts_query_predicates_for_pattern" (uptr unsigned-32 u8*) uptr))
  (define %query-rooted?
    (foreign-procedure "ts_query_is_pattern_rooted" (uptr unsigned-32) stdbool))
  (define %query-non-local?
    (foreign-procedure "ts_query_is_pattern_non_local" (uptr unsigned-32) stdbool))
  (define %query-guaranteed-at-step?
    (foreign-procedure "ts_query_is_pattern_guaranteed_at_step" (uptr unsigned-32) stdbool))
  (define %query-capture-name
    (foreign-procedure "ts_query_capture_name_for_id" (uptr unsigned-32 u8*) uptr))
  (define %query-string-value
    (foreign-procedure "ts_query_string_value_for_id" (uptr unsigned-32 u8*) uptr))
  (define %query-capture-quantifier
    (foreign-procedure "ts_query_capture_quantifier_for_id" (uptr unsigned-32 unsigned-32) int))
  (define %regex-new (foreign-procedure "chez_ts_regex_new" (u8* u8* unsigned-32) uptr))
  (define %regex-free (foreign-procedure "chez_ts_regex_free" (uptr) void))
  (define %regex-search
    (foreign-procedure "chez_ts_regex_search" (uptr u8* unsigned-32 unsigned-32) boolean))

  (define predicate-step-done (%layout 27))
  (define predicate-step-capture (%layout 28))

  (define-record-type query-property
    (nongenerative)
    (sealed #t)
    (fields key value capture))

  (define-record-type pattern
    (nongenerative)
    (sealed #t)
    (fields predicates text? settings assertions negated-assertions directives))

  (define-record-type (query %make-query query?)
    (nongenerative)
    (sealed #t)
    (fields address language source (immutable capture-names %query-capture-names) patterns
      handler))

  (define (check-query who x) (check who query? x "a query"))

  (define make-query
    (case-lambda
      [(language source) (make-query* 'make-query language source #f)]
      [(language source handler)
       (check 'make-query procedure? handler "a procedure")
       (make-query* 'make-query language source handler)]))

  (define query-error-kinds '#(none syntax node-type field capture structure language))

  (define (raise-query-error who kind bytes offset message)
    (let loop ([i 0] [row 0] [line-start 0])
      (if (fx< i (fxmin offset (bytevector-length bytes)))
          (if (fx= (bytevector-u8-ref bytes i) 10)
              (loop (fx+ i 1) (fx+ row 1) (fx+ i 1))
              (loop (fx+ i 1) row line-start))
          (let ([line-end (let find ([j offset])
                            (if (or (fx>= j (bytevector-length bytes))
                                    (fx= (bytevector-u8-ref bytes j) 10))
                                j
                                (find (fx+ j 1))))])
            (raise
              (condition
                (make-query-error kind offset row (fx- offset line-start))
                (make-who-condition who)
                (make-message-condition
                  (format "~a at row ~a, column ~a" message row (fx- offset line-start)))
                (make-irritants-condition
                  (list (utf8->string
                          (let ([bv (make-bytevector (fx- (fxmax offset line-end) offset))])
                            (bytevector-copy! bytes offset bv 0 (bytevector-length bv))
                            bv))))))))))

  (define (make-query* who language source handler)
    (check-language who language)
    (check who string? source "a string")
    (release-dropped!)
    (let* ([bytes (string->utf8 source)]
           [offset (out-u32)]
           [kind (out-u32)]
           [address (%query-new (language-address language) bytes (bytevector-length bytes)
                      offset kind)])
      (when (eqv? address 0)
        (let ([kind (vector-ref query-error-kinds (fxmin (out-u32-ref kind) 6))])
          (raise-query-error who kind bytes (out-u32-ref offset)
            (case kind
              [(syntax) "invalid query syntax"]
              [(node-type) "invalid node type"]
              [(field) "invalid field name"]
              [(capture) "invalid capture name"]
              [(structure) "impossible pattern"]
              [else "query incompatible with the language"]))))
      (query-from-address who language bytes address handler)))

  (define (query-from-address who language bytes address handler)
    (let ([regexes '()] [done? #f])
      (define (compile-regex! pattern fail)
        (let* ([message (make-bytevector 256 0)]
               [posix (rust-regex->posix pattern
                        (lambda (what)
                          (fail (format "regular expression ~s uses ~a, which is not supported"
                                  pattern what))))]
               [regex (%regex-new (string->utf8 (string-append posix "\x0;")) message
                        (bytevector-length message))])
          (when (eqv? regex 0)
            (fail (format "invalid regular expression ~s: ~a" pattern
                    (bytevector-c-string message))))
          (set! regexes (cons regex regexes))
          regex))
      (dynamic-wind
        void
        (lambda ()
          (let* ([capture-names (query-strings address (%query-capture-count address)
                                  %query-capture-name)]
                 [strings (query-strings address (%query-string-count address) %query-string-value)]
                 [patterns
                  (let ([count (%query-pattern-count address)])
                    (do ([i 0 (fx+ i 1)]
                         [acc '() (cons (read-pattern who address bytes i capture-names strings
                                          handler compile-regex!)
                                        acc)])
                        ((fx= i count) (list->vector (reverse acc)))))]
                 [query (%make-query address language bytes capture-names patterns handler)])
            (let ([regexes regexes])
              (owned query address
                (lambda (address)
                  (for-each %regex-free regexes)
                  (%query-delete address))))
            (set! done? #t)
            query))
        (lambda ()
          (unless done?
            (for-each %regex-free regexes)
            (%query-delete address))))))

  (define (query-strings address count ref)
    (let ([v (make-vector count)] [size (out-u32)])
      (do ([i 0 (fx+ i 1)]) ((fx= i count) v)
        (let ([chars (ref address i size)])
          (vector-set! v i
            (string->immutable-string (c-bytes->string chars (out-u32-ref size))))))))

  (define (read-pattern who address bytes index capture-names strings handler compile-regex!)
    (let* ([count (out-u32)]
           [steps (%query-predicates address index count)]
           [step-count (out-u32-ref count)]
           [start (%query-start-byte address index)])
      (define step-pointer (make-ftype-pointer ts-predicate-step steps))
      (define (step-type i) (ftype-ref ts-predicate-step (type) step-pointer i))
      (define (step-value i) (ftype-ref ts-predicate-step (value-id) step-pointer i))
      (define (fail message) (raise-query-error who 'predicate bytes start message))
      (define predicates
        (let loop ([i 0] [current '()] [acc '()])
          (cond
            [(fx= i step-count)
             (reverse (if (null? current) acc (cons (reverse current) acc)))]
            [(fx= (step-type i) predicate-step-done)
             (loop (fx+ i 1) '() (if (null? current) acc (cons (reverse current) acc)))]
            [(fx= (step-type i) predicate-step-capture)
             (loop (fx+ i 1) (cons (cons 'capture (step-value i)) current) acc)]
            [else (loop (fx+ i 1) (cons (vector-ref strings (step-value i)) current) acc)])))
      (define (capture? x) (pair? x))
      (define (capture-name x) (vector-ref capture-names (cdr x)))
      (let loop ([predicates predicates] [tests '()] [text? #f] [settings '()] [assertions '()]
                 [negated '()] [directives '()])
        (if (null? predicates)
            (make-pattern (reverse tests) text? (reverse settings) (reverse assertions)
              (reverse negated) (reverse directives))
            (let* ([p (car predicates)] [name (car p)] [args (cdr p)])
              (define (next test text?*)
                (loop (cdr predicates) (if test (cons test tests) tests) (or text? text?*)
                  settings assertions negated directives))
              (define (property)
                (let collect ([args args] [capture #f] [strings '()])
                  (cond
                    [(null? args)
                     (let ([strings (reverse strings)])
                       (when (or (null? strings) (fx> (length strings) 2))
                         (fail (format "#~a takes a key, an optional value and an optional capture"
                                 name)))
                       (make-query-property (car strings) (and (pair? (cdr strings)) (cadr strings))
                         capture))]
                    [(capture? (car args))
                     (when capture (fail (format "#~a takes at most one capture" name)))
                     (collect (cdr args) (capture-name (car args)) strings)]
                    [else (collect (cdr args) capture (cons (car args) strings))])))
              (unless (string? name) (fail "a predicate must start with its name"))
              (cond
                [(member name '("eq?" "not-eq?" "any-eq?" "any-not-eq?"))
                 (unless (and (fx= (length args) 2) (capture? (car args)))
                   (fail (format "#~a takes a capture and a capture or string" name)))
                 (next (eq-test (cdar args) (cadr args)
                         (and (member name '("eq?" "any-eq?")) #t)
                         (and (member name '("eq?" "not-eq?")) #t))
                   #t)]
                [(member name '("match?" "not-match?" "any-match?" "any-not-match?"))
                 (unless (and (fx= (length args) 2) (capture? (car args)) (string? (cadr args)))
                   (fail (format "#~a takes a capture and a regular expression" name)))
                 (next (match-test (cdar args) (compile-regex! (cadr args) fail)
                         (and (member name '("match?" "any-match?")) #t)
                         (and (member name '("match?" "not-match?")) #t))
                   #t)]
                [(member name '("any-of?" "not-any-of?"))
                 (unless (and (pair? args) (capture? (car args)) (for-all string? (cdr args)))
                   (fail (format "#~a takes a capture and strings" name)))
                 (next (any-of-test (cdar args) (map string->utf8 (cdr args))
                         (string=? name "any-of?"))
                   #t)]
                [(string=? name "set!")
                 (loop (cdr predicates) tests text? (cons (property) settings) assertions negated
                   directives)]
                [(string=? name "is?")
                 (loop (cdr predicates) tests text? settings (cons (property) assertions) negated
                   directives)]
                [(string=? name "is-not?")
                 (loop (cdr predicates) tests text? settings assertions (cons (property) negated)
                   directives)]
                ;; A name ending in ! is an instruction to the host, such as
                ;; #strip!, and filters no match.
                [(and (fx> (string-length name) 0)
                      (char=? (string-ref name (fx- (string-length name) 1)) #\!))
                 (loop (cdr predicates) tests text? settings assertions negated
                   (cons (cons name (map (lambda (arg) (if (capture? arg) (cdr arg) arg)) args))
                     directives))]
                [handler
                 (next (handler-test handler name
                         (map (lambda (arg) (if (capture? arg) (cdr arg) arg)) args)
                         (map capture? args))
                   #f)]
                [else (fail (format "unknown predicate #~a" name))]))))))

  (define (capture-nodes captures index)
    (let loop ([captures captures] [acc '()])
      (cond
        [(null? captures) (reverse acc)]
        [(fx= (query-capture-index (car captures)) index)
         (loop (cdr captures) (cons (query-capture-node (car captures)) acc))]
        [else (loop (cdr captures) acc)])))

  (define (node-span n)
    (let* ([raw (node-raw n)] [start (%node-start-byte raw)] [end (%node-end-byte raw)])
      (keep-live n)
      (values start end)))

  (define (check-span source end)
    (unless (fx<= end (bytevector-length source))
      (assertion-violationf 'query-captures "the source text ends at byte ~s, before ~s"
        (bytevector-length source) end)))

  (define (bytes=? source start end bytes)
    (check-span source end)
    (and (fx= (fx- end start) (bytevector-length bytes))
         (let loop ([i 0])
           (or (fx= i (bytevector-length bytes))
               (and (fx= (bytevector-u8-ref source (fx+ start i)) (bytevector-u8-ref bytes i))
                    (loop (fx+ i 1)))))))

  (define (node-text=? source a b)
    (let-values ([(a-start a-end) (node-span a)] [(b-start b-end) (node-span b)])
      (check-span source a-end)
      (check-span source b-end)
      (and (fx= (fx- a-end a-start) (fx- b-end b-start))
           (let loop ([i 0])
             (or (fx= i (fx- a-end a-start))
                 (and (fx= (bytevector-u8-ref source (fx+ a-start i))
                           (bytevector-u8-ref source (fx+ b-start i)))
                      (loop (fx+ i 1))))))))

  ;; A quantified capture passes when every node does, or with any- when
  ;; one does. tree-sitter's Rust binding also passes an any- test that no
  ;; node passes, against its documentation; this follows the documentation.
  (define (quantified nodes positive? all? test)
    (define (passes? n) (eq? (and (test n) #t) positive?))
    (if all? (for-all passes? nodes) (exists passes? nodes)))

  (define (eq-test index other positive? all?)
    (if (pair? other)
        (let ([other-index (cdr other)])
          (lambda (captures source)
            (let ([as (capture-nodes captures index)] [bs (capture-nodes captures other-index)])
              (define (passes? a b) (eq? (node-text=? source a b) positive?))
              (if all?
                  (and (fx= (length as) (length bs)) (for-all passes? as bs))
                  (let loop ([as as] [bs bs])
                    (and (pair? as) (pair? bs)
                         (or (passes? (car as) (car bs)) (loop (cdr as) (cdr bs)))))))))
        (let ([bytes (string->utf8 other)])
          (lambda (captures source)
            (quantified (capture-nodes captures index) positive? all?
              (lambda (n)
                (let-values ([(start end) (node-span n)]) (bytes=? source start end bytes))))))))

  (define (match-test index regex positive? all?)
    (lambda (captures source)
      (quantified (capture-nodes captures index) positive? all?
        (lambda (n)
          (let-values ([(start end) (node-span n)])
            (check-span source end)
            (%regex-search regex source start end))))))

  (define (any-of-test index strings positive?)
    (lambda (captures source)
      (for-all
        (lambda (n)
          (let-values ([(start end) (node-span n)])
            (eq? (and (exists (lambda (bytes) (bytes=? source start end bytes)) strings) #t)
                 positive?)))
        (capture-nodes captures index))))

  (define (handler-test handler name args capture-args?)
    (lambda (captures source)
      (handler name
        (map (lambda (arg capture?) (if capture? (capture-nodes captures arg) arg))
          args capture-args?))))

  ;; tree-sitter's tools take Rust's regular expression syntax; this
  ;; translates its common forms to POSIX extended syntax for regcomp, and
  ;; refuses what has no equivalent rather than match differently.
  (define (rust-regex->posix pattern unsupported)
    (define n (string-length pattern))
    (define (escape-outside c)
      (case c
        [(#\d) "[0-9]"]
        [(#\D) "[^0-9]"]
        [(#\s #\S #\w #\W #\b #\B) (string #\\ c)]
        [(#\n) "\n"]
        [(#\t) "\t"]
        [(#\r) "\r"]
        [(#\A) "^"]
        [(#\z) "$"]
        [else
         (if (or (char-alphabetic? c) (char-numeric? c))
             (unsupported (string #\\ c))
             (string #\\ c))]))
    (define (bracket i out)
      ;; POSIX reads a class's members literally only in this order: "]"
      ;; first, "-" last, "^" not first.
      (let-values ([(negated? i) (if (and (fx< i n) (char=? (string-ref pattern i) #\^))
                                     (values #t (fx+ i 1))
                                     (values #f i))])
        (let loop ([i i] [members '()] [close? #f] [dash? #f] [caret? #f] [first? #t])
          (when (fx>= i n) (unsupported "an unclosed ["))
          (let ([c (string-ref pattern i)])
            (define (add s) (loop (fx+ i 1) (cons s members) close? dash? caret? #f))
            (cond
              [(and (char=? c #\]) (not first?) caret? (not negated?) (null? members) (not close?)
                    (not dash?))
               (put-string out "\\^")
               (fx+ i 1)]
              [(and (char=? c #\]) (not first?))
               (put-char out #\[)
               (when negated? (put-char out #\^))
               (when close? (put-char out #\]))
               (for-each (lambda (s) (put-string out s)) (reverse members))
               (when caret? (put-char out #\^))
               (when dash? (put-char out #\-))
               (put-char out #\])
               (fx+ i 1)]
              [(char=? c #\]) (loop (fx+ i 1) members #t dash? caret? #f)]
              [(char>? c #\x7f) (unsupported "a non-ASCII character in a class")]
              [(and (char=? c #\[) (fx< (fx+ i 1) n) (char=? (string-ref pattern (fx+ i 1)) #\:))
               (let find ([j (fx+ i 2)])
                 (cond
                   [(fx>= (fx+ j 1) n) (unsupported "an unclosed [:")]
                   [(and (char=? (string-ref pattern j) #\:)
                         (char=? (string-ref pattern (fx+ j 1)) #\]))
                    (loop (fx+ j 2) (cons (substring pattern i (fx+ j 2)) members) close? dash?
                      caret? #f)]
                   [else (find (fx+ j 1))]))]
              [(char=? c #\\)
               (when (fx>= (fx+ i 1) n) (unsupported "a trailing \\"))
               (let ([e (string-ref pattern (fx+ i 1))])
                 (define (add-escape s)
                   (loop (fx+ i 2) (cons s members) close? dash? caret? #f))
                 (case e
                   [(#\d) (add-escape "0-9")]
                   [(#\s) (add-escape "[:space:]")]
                   [(#\w) (add-escape "[:alnum:]_")]
                   [(#\n) (add-escape "\n")]
                   [(#\t) (add-escape "\t")]
                   [(#\r) (add-escape "\r")]
                   [(#\]) (loop (fx+ i 2) members #t dash? caret? #f)]
                   [(#\-) (loop (fx+ i 2) members close? #t caret? #f)]
                   [(#\^) (loop (fx+ i 2) members close? dash? #t #f)]
                   [else
                    (if (or (char-alphabetic? e) (char-numeric? e) (char>? e #\x7f))
                        (unsupported (string #\\ e))
                        (add-escape (string e)))]))]
              [(and (char=? c #\-) (or first? (and (fx< (fx+ i 1) n)
                                                   (char=? (string-ref pattern (fx+ i 1)) #\]))))
               (loop (fx+ i 1) members close? #t caret? #f)]
              [(char=? c #\^) (loop (fx+ i 1) members close? dash? #t #f)]
              [else (add (string c))])))))
    (call-with-string-output-port
      (lambda (out)
        (let loop ([i 0])
          (when (fx< i n)
            (let ([c (string-ref pattern i)])
              (cond
                [(char=? c #\\)
                 (when (fx>= (fx+ i 1) n) (unsupported "a trailing \\"))
                 (put-string out (escape-outside (string-ref pattern (fx+ i 1))))
                 (loop (fx+ i 2))]
                [(char=? c #\[) (loop (bracket (fx+ i 1) out))]
                [(and (char=? c #\() (fx< (fx+ i 1) n) (char=? (string-ref pattern (fx+ i 1)) #\?))
                 (if (and (fx< (fx+ i 2) n) (char=? (string-ref pattern (fx+ i 2)) #\:))
                     (begin (put-char out #\() (loop (fx+ i 3)))
                     (unsupported "a group flag or lookaround"))]
                [(and (char=? c #\?) (fx> i 0)
                      (memv (string-ref pattern (fx- i 1)) '(#\* #\+ #\? #\})))
                 ;; A lazy quantifier matches the same texts as a greedy one.
                 (if (and (fx> i 1) (char=? (string-ref pattern (fx- i 2)) #\\))
                     (put-char out c)
                     (void))
                 (loop (fx+ i 1))]
                [else (put-char out c) (loop (fx+ i 1))])))))))

  (define (query-pattern-count query)
    (check-query 'query-pattern-count query)
    (vector-length (query-patterns query)))

  (define (query-capture-count query)
    (check-query 'query-capture-count query)
    (vector-length (%query-capture-names query)))

  (define (query-capture-names query)
    (check-query 'query-capture-names query)
    (vector->list (%query-capture-names query)))

  (define (check-pattern who query index)
    (check-query who query)
    (unless (and (fixnum? index) (fx< -1 index (vector-length (query-patterns query))))
      (assertion-violationf who "~s is not a pattern index of the query" index)))

  (define (query-pattern-start-byte query index)
    (check-pattern 'query-pattern-start-byte query index)
    (let ([start (%query-start-byte (query-address query) index)]) (keep-live query) start))

  (define (query-pattern-end-byte query index)
    (check-pattern 'query-pattern-end-byte query index)
    (let ([end (%query-end-byte (query-address query) index)]) (keep-live query) end))

  (define (query-pattern-rooted? query index)
    (check-pattern 'query-pattern-rooted? query index)
    (let ([rooted? (%query-rooted? (query-address query) index)]) (keep-live query) rooted?))

  (define (query-pattern-non-local? query index)
    (check-pattern 'query-pattern-non-local? query index)
    (let ([non-local? (%query-non-local? (query-address query) index)])
      (keep-live query)
      non-local?))

  (define (query-step-guaranteed? query byte)
    (check-query 'query-step-guaranteed? query)
    (check 'query-step-guaranteed? u32? byte "a byte offset")
    (let ([guaranteed? (%query-guaranteed-at-step? (query-address query) byte)])
      (keep-live query)
      guaranteed?))

  (define (query-capture-quantifier query index capture)
    (check-pattern 'query-capture-quantifier query index)
    (let ([id (cond
                [(string? capture)
                 (let find ([i 0])
                   (cond
                     [(fx= i (vector-length (%query-capture-names query)))
                      (assertion-violationf 'query-capture-quantifier "the query has no capture ~s"
                        capture)]
                     [(string=? (vector-ref (%query-capture-names query) i) capture) i]
                     [else (find (fx+ i 1))]))]
                [else (assertion-violationf 'query-capture-quantifier "~s is not a capture name"
                        capture)])])
      (let ([q (%query-capture-quantifier (query-address query) index id)])
        (keep-live query)
        (case q
          [(0) 'zero]
          [(1) 'zero-or-one]
          [(2) 'zero-or-more]
          [(3) 'one]
          [else 'one-or-more]))))

  (define (query-pattern-settings query index)
    (check-pattern 'query-pattern-settings query index)
    (pattern-settings (vector-ref (query-patterns query) index)))

  (define (query-pattern-assertions query index)
    (check-pattern 'query-pattern-assertions query index)
    (pattern-assertions (vector-ref (query-patterns query) index)))

  (define (query-pattern-negated-assertions query index)
    (check-pattern 'query-pattern-negated-assertions query index)
    (pattern-negated-assertions (vector-ref (query-patterns query) index)))

  (define (query-pattern-directives query index)
    (check-pattern 'query-pattern-directives query index)
    (pattern-directives (vector-ref (query-patterns query) index)))

  (define (query-without who query pattern? index capture)
    (release-dropped!)
    (let* ([name (if pattern? (make-bytevector 0) (string->utf8 capture))]
           [address (%query-copy-without (query-address query) pattern? index name
                      (bytevector-length name))])
      (keep-live query)
      (query-from-address who (query-language query) (query-source query) address
        (query-handler query))))

  (define (query-without-pattern query index)
    (check-pattern 'query-without-pattern query index)
    (query-without 'query-without-pattern query #t index #f))

  (define (query-without-capture query name)
    (check-query 'query-without-capture query)
    (check 'query-without-capture string? name "a string")
    (query-without 'query-without-capture query #f 0 name))

  (define %query-cursor-new (foreign-procedure "chez_ts_query_cursor_new" () uptr))
  (define %query-cursor-delete (foreign-procedure "chez_ts_query_cursor_delete" (uptr) void))
  (define %query-cursor-exec
    (foreign-procedure "chez_ts_query_cursor_exec" (uptr uptr u8*) void))
  (define %query-cursor-set-match-limit
    (foreign-procedure "chez_ts_query_cursor_set_match_limit" (uptr unsigned-32) void))
  (define %query-cursor-match-limit
    (foreign-procedure "chez_ts_query_cursor_match_limit" (uptr) unsigned-32))
  (define %query-cursor-exceeded?
    (foreign-procedure "chez_ts_query_cursor_did_exceed_match_limit" (uptr) boolean))
  (define %query-cursor-set-max-start-depth
    (foreign-procedure "chez_ts_query_cursor_set_max_start_depth" (uptr unsigned-32) void))
  (define %query-cursor-set-byte-range
    (foreign-procedure "chez_ts_query_cursor_set_byte_range"
      (uptr boolean unsigned-32 unsigned-32) boolean))
  (define %query-cursor-set-point-range
    (foreign-procedure "chez_ts_query_cursor_set_point_range"
      (uptr boolean unsigned-32 unsigned-32 unsigned-32 unsigned-32) boolean))
  (define %query-cursor-next
    (foreign-procedure "chez_ts_query_cursor_next" (uptr boolean u8*) boolean))
  (define %query-cursor-capture
    (foreign-procedure "chez_ts_query_cursor_capture" (uptr unsigned-32 u8*) unsigned-32))
  (define %query-cursor-remove-match
    (foreign-procedure "chez_ts_query_cursor_remove_match" (uptr unsigned-32) void))

  (define-record-type query-match
    (nongenerative)
    (sealed #t)
    (fields pattern captures))

  (define-record-type query-capture
    (nongenerative)
    (sealed #t)
    (fields node name index pattern))

  (define-record-type (query-cursor %make-query-cursor query-cursor?)
    (nongenerative)
    (sealed #t)
    (fields address (mutable query) (mutable tree) (mutable source) busy))

  (define (make-query-cursor)
    (release-dropped!)
    (let ([address (%query-cursor-new)])
      (owned (%make-query-cursor address #f #f #f (box #f)) address %query-cursor-delete)))

  (define-syntax with-query-cursor
    (syntax-rules ()
      [(_ who c (address) body1 body2 ...)
       (identifier? #'c)
       (begin
         (check who query-cursor? c "a query cursor")
         (with-exclusive-step who (query-cursor-busy c) "query cursor"
           (let* ([address (query-cursor-address c)] [result (let () body1 body2 ...)])
             (keep-live c)
             result)))]))

  (define query-cursor-exec!
    (case-lambda
      [(c query n) (exec! 'query-cursor-exec! c query n #f)]
      [(c query n source)
       (check 'query-cursor-exec! bytevector? source "a bytevector")
       (exec! 'query-cursor-exec! c query n source)]))

  (define (exec! who c query n source)
    (check-query who query)
    (with-node who n (raw)
      (let ([source (or source (tree-source (node-tree n)))])
        (when (and (not source) (exists pattern-text? (vector->list (query-patterns query))))
          (assertion-violationf who
            "the query tests text, and the node's tree has no source text: give the source"))
        (unless (eqv? (language-address (query-language query))
                      (language-address (tree-language (node-tree n))))
          (assertion-violationf who "the query's language is ~a and the node's is ~a"
            (language-name (query-language query))
            (language-name (tree-language (node-tree n)))))
        (with-query-cursor who c (address)
          (%query-cursor-exec address (query-address query) raw)
          (query-cursor-query-set! c query)
          (query-cursor-tree-set! c (node-tree n))
          (query-cursor-source-set! c source)))))

  (define (match-captures c address count pattern)
    (let ([query (query-cursor-query c)] [tree (query-cursor-tree c)])
      (let loop ([i (fx- count 1)] [acc '()])
        (if (fx< i 0)
            acc
            (loop (fx- i 1) (cons (capture-at query tree address i pattern) acc))))))

  (define (capture-at query tree address i pattern)
    (let* ([raw (make-bytevector node-size)] [index (%query-cursor-capture address i raw)])
      (make-query-capture (make-node tree raw) (vector-ref (%query-capture-names query) index)
        index pattern)))

  (define (passes? query pattern captures source)
    (for-all (lambda (test) (test captures source))
      (pattern-predicates (vector-ref (query-patterns query) pattern))))

  ;; Predicates run with the cursor released, so that a handler may use the
  ;; cursor; each step takes the query and source it ran with.
  (define-record-type cursor-step
    (nongenerative)
    (sealed #t)
    (fields query source match-id pattern capture captures))

  (define (next-match! who c)
    (let ([header (make-bytevector 16)])
      (let loop ()
        (let ([step (with-query-cursor who c (address)
                      (and (query-cursor-query c)
                           (%query-cursor-next address #f header)
                           (let ([pattern (bytevector-u32-native-ref header 4)])
                             (make-cursor-step (query-cursor-query c) (query-cursor-source c) #f
                               pattern #f
                               (match-captures c address (bytevector-u32-native-ref header 8)
                                 pattern)))))])
          (cond
            [(not step) #f]
            [(passes? (cursor-step-query step) (cursor-step-pattern step)
               (cursor-step-captures step) (cursor-step-source step))
             (make-query-match (cursor-step-pattern step) (cursor-step-captures step))]
            [else (loop)])))))

  (define (next-capture! who c)
    (let ([header (make-bytevector 16)])
      (let loop ()
        (let ([step (with-query-cursor who c (address)
                      (and (query-cursor-query c)
                           (%query-cursor-next address #t header)
                           (let* ([query (query-cursor-query c)]
                                  [pattern (bytevector-u32-native-ref header 4)]
                                  [tests? (pair? (pattern-predicates
                                                   (vector-ref (query-patterns query) pattern)))])
                             (make-cursor-step query (query-cursor-source c)
                               (bytevector-u32-native-ref header 0)
                               pattern
                               (capture-at query (query-cursor-tree c) address
                                 (bytevector-u32-native-ref header 12) pattern)
                               (and tests?
                                    (match-captures c address
                                      (bytevector-u32-native-ref header 8) pattern))))))])
          (cond
            [(not step) #f]
            [(or (not (cursor-step-captures step))
                 (passes? (cursor-step-query step) (cursor-step-pattern step)
                   (cursor-step-captures step) (cursor-step-source step)))
             (cursor-step-capture step)]
            [else
             (with-query-cursor who c (address)
               (%query-cursor-remove-match address (cursor-step-match-id step)))
             (loop)])))))

  (define (query-cursor-next-match! c) (next-match! 'query-cursor-next-match! c))
  (define (query-cursor-next-capture! c) (next-capture! 'query-cursor-next-capture! c))

  (define (query-cursor-match-limit c)
    (with-query-cursor 'query-cursor-match-limit c (address)
      (let ([limit (%query-cursor-match-limit address)])
        (and (not (= limit u32-max)) limit))))

  (define (query-cursor-match-limit-set! c limit)
    (check 'query-cursor-match-limit-set! (lambda (x) (or (not x) (and (u32? x) (fx> x 0)))) limit
      "a positive 32-bit limit or #f")
    (with-query-cursor 'query-cursor-match-limit-set! c (address)
      (%query-cursor-set-match-limit address (or limit u32-max))))

  (define (query-cursor-match-limit-exceeded? c)
    (with-query-cursor 'query-cursor-match-limit-exceeded? c (address)
      (%query-cursor-exceeded? address)))

  (define (query-cursor-max-start-depth-set! c depth)
    (check 'query-cursor-max-start-depth-set! (lambda (x) (or (not x) (u32? x))) depth
      "an unsigned 32-bit depth or #f")
    (with-query-cursor 'query-cursor-max-start-depth-set! c (address)
      (%query-cursor-set-max-start-depth address (or depth u32-max))))

  (define (set-byte-range! who c containing? start end)
    (check-byte-range who start end)
    (with-query-cursor who c (address)
      (%query-cursor-set-byte-range address containing? start end)))

  (define (query-cursor-byte-range-set! c start end)
    (set-byte-range! 'query-cursor-byte-range-set! c #f start end))

  (define (query-cursor-containing-byte-range-set! c start end)
    (set-byte-range! 'query-cursor-containing-byte-range-set! c #t start end))

  (define (set-point-range! who c containing? start end)
    (check who point? start "a point")
    (check who point? end "a point")
    (unless (or (< (point-row start) (point-row end))
                (and (= (point-row start) (point-row end))
                     (<= (point-column start) (point-column end))))
      (assertion-violationf who "start ~s is after end ~s" start end))
    (with-query-cursor who c (address)
      (%query-cursor-set-point-range address containing? (point-row start) (point-column start)
        (point-row end) (point-column end))))

  (define (query-cursor-point-range-set! c start end)
    (set-point-range! 'query-cursor-point-range-set! c #f start end))

  (define (query-cursor-containing-point-range-set! c start end)
    (set-point-range! 'query-cursor-containing-point-range-set! c #t start end))

  (define (run-query who query n start end source next!)
    (when source (check who bytevector? source "a bytevector"))
    (let ([c (make-query-cursor)])
      (unless (and (eqv? start 0) (eqv? end u32-max)) (set-byte-range! who c #f start end))
      (exec! who c query n source)
      (let loop ([acc '()])
        (let ([x (next! who c)])
          (if x (loop (cons x acc)) (reverse acc))))))

  (define query-matches
    (case-lambda
      [(query n) (run-query 'query-matches query n 0 u32-max #f next-match!)]
      [(query n source) (run-query 'query-matches query n 0 u32-max source next-match!)]
      [(query n start end) (run-query 'query-matches query n start end #f next-match!)]
      [(query n start end source)
       (run-query 'query-matches query n start end source next-match!)]))

  (define query-captures
    (case-lambda
      [(query n) (run-query 'query-captures query n 0 u32-max #f next-capture!)]
      [(query n source) (run-query 'query-captures query n 0 u32-max source next-capture!)]
      [(query n start end) (run-query 'query-captures query n start end #f next-capture!)]
      [(query n start end source)
       (run-query 'query-captures query n start end source next-capture!)])))
