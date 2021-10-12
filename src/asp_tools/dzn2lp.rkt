#lang racket

(define input-file
  (if (vector-empty? (current-command-line-arguments))
      "../../examples/dzn-to-lp/R046.lp"
      (first (vector->list (current-command-line-arguments)))))

(define (common-atomic-constraint s dzn-tag lp-tag rule-name)
  (let* ([pat (pregexp (format "(?s:~a\\s+=\\s+\\[\\|(.*)\\]\\s*;.*)" dzn-tag))]
         [match (regexp-match pat s)]
         [c (if (list? match) (second match) "")] ; all constraints as string
         [col (if (empty? c)
                 ""
                 (for/list ([c (regexp-match* #px"\\d+,\\s+\\d+\\|" c)])
                   (let ([m (regexp-match* #px"\\d+" c)])
                     (format "~acon(~a,~a).\n" rule-name (first m) (second m)))))]) ; list with one formated string per constraint
    (string-append* (format "% ~a constraints\n" lp-tag) col)))

; embedded unit test for atomic constraints
(module+ test
  (require rackunit)
  (let ([dzn-str-empty "AtomicConstraints =  [ ] ;"]
        [dzn-str-1-constraint "AtomicConstraints =  [|
                                           14, 16|];
                                         "]
        [dzn-str-2-constraint "AtomicConstraints =  [|
                                           4, 9|
                                           14, 16|];
                                         "]
        )
    (check-equal? (atomic-constraint dzn-str-empty) "% atomic constraints\n")
    (check-equal? (atomic-constraint dzn-str-1-constraint) "% atomic constraints\natomiccon(14,16).\n")
    (check-equal? (atomic-constraint dzn-str-2-constraint) "% atomic constraints\natomiccon(4,9).\natomiccon(14,16).\n")))

; Convert an Atomic Constraint from DZN to LP format"
(define (atomic-constraint s)
  (common-atomic-constraint s "AtomicConstraints" "atomic" "atomic"))

; embedded unit test for disjunctive constraints
(module+ test
  (let ([dzn-str-empty "DisjunctiveConstraints =  [ ] ;"]
        [dzn-str-1-constraint "DisjunctiveConstraints =  [|
                                           4, 3, 4, 5|];
                                         "]
        [dzn-str-3-constraint "DisjunctiveConstraints =  [|4, 3, 4, 5|
                                                13, 2, 13, 11|
                                                13, 7, 13, 16|];
                                         "]
        )
    (check-equal? (disjunctive-constraint dzn-str-empty) "% disjunctive constraints\n")
    (check-equal? (disjunctive-constraint dzn-str-1-constraint) "% disjunctive constraints\ndiscon(4,3,4,5).\n")
    (check-equal? (disjunctive-constraint dzn-str-3-constraint) "% disjunctive constraints\ndiscon(4,3,4,5).\ndiscon(13,2,13,11).\ndiscon(13,7,13,16).\n")
))

; Convert a Disjunctive Constraint from DZN to LP format"
(define (disjunctive-constraint s)
  (let* ([pat (pregexp "(?s:DisjunctiveConstraints\\s+=\\s+\\[\\|(.*)\\]\\s*;.*)")]
         [match (regexp-match pat s)]
         [c (if (list? match) (second match) "")] ; all constraints as string
         [col (if (empty? c)
                 ""
                 (for/list ([c (regexp-match* #px"\\d+,\\s+\\d+,\\s+\\d+,\\s+\\d+\\|" c)])
                   (let ([m (regexp-match* #px"\\d+" c)])
                     (format "discon(~a,~a,~a,~a).\n" (first m) (second m) (third m) (fourth m)))))] ; list with one formated string per constraint
                 
         )
    (string-append* "% disjunctive constraints\n" col)))

; embedded unit test for soft atomic constraints
(module+ test
  (let ([dzn-str-empty "SoftAtomicConstraints  =  [ ] ;"]
        [dzn-str-1-constraint "SoftAtomicConstraints  =  [|
                                           9, 1|];
                                         "]
        [dzn-str-3-constraint "SoftAtomicConstraints  =  [|
                                           9, 1|
                                           1, 2|
                                           17, 18|];
                                         "]
        )
    (check-equal? (soft-atomic-constraint dzn-str-empty) "% soft atomic constraints\n")
    (check-equal? (soft-atomic-constraint dzn-str-1-constraint) "% soft atomic constraints\nsoftcon(9,1).\n")
    (check-equal? (soft-atomic-constraint dzn-str-3-constraint) "% soft atomic constraints\nsoftcon(9,1).\nsoftcon(1,2).\nsoftcon(17,18).\n")
))

; Convert an Soft Atomic Constraint from DZN to LP format"
(define (soft-atomic-constraint s)
  (common-atomic-constraint s "SoftAtomicConstraints" "soft atomic" "soft"))

; embedded unit test for direct successor constraints
(module+ test
  (let ([dzn-str-empty "DirectSuccessors   =  [ ] ;"]
        [dzn-str-1-constraint "DirectSuccessors   =  [|
                                           14, 16|];
                                         "]
        [dzn-str-2-constraint "DirectSuccessors   =  [|
                                           4, 9|
                                           14, 16|];
                                         "]
        )
    (check-equal? (direct-successor-constraint dzn-str-empty) "% direct successor constraints\n")
    (check-equal? (direct-successor-constraint dzn-str-1-constraint) "% direct successor constraints\ndirsuccon(14,16).\n")
    (check-equal? (direct-successor-constraint dzn-str-2-constraint) "% direct successor constraints\ndirsuccon(4,9).\ndirsuccon(14,16).\n")
))

; Convert an Direct Successors Constraint from DZN to LP format"
(define (direct-successor-constraint s)
  (common-atomic-constraint s "DirectSuccessors" "direct successor" "dirsuc"))

; embedded unit test to convert a complete DZN problem into LP representation
(module+ test
  (let ([dzn-str-empty "   "]
        [dzn-str-k-b " k = 18;\n   b = 9;\n  " ]
        )
    (check-equal? (dzn-to-lp "A031" dzn-str-empty)
                  (string-join (list
                                "% benchmark A031\n\n"
                                "% atomic constraints\n\n"
                                "% disjunctive constraints\n\n"
                                "% soft atomic constraints\n\n"
                                "% direct successor constraints\n\n")
                               ""))
    (check-equal? (dzn-to-lp "A031" dzn-str-k-b)
                  (string-join (list
                                "% benchmark A031\n% num of cables\n#const k=18.\n% num of 2-sided cables\n#const b=9.\n\n"
                                "% atomic constraints\n\n"
                                "% disjunctive constraints\n\n"
                                "% soft atomic constraints\n\n"
                                "% direct successor constraints\n\n")
                               ""))
    ))

; Convert a DZN problem to LP format
(define (dzn-to-lp prb-name dzn-str)
  (string-join (list
                 (format "% benchmark ~a\n" prb-name)
                 (dzn-to-lp-k (get-section dzn-str "k"))
                 (dzn-to-lp-b (get-section dzn-str "b"))
                 "\n"
                 (atomic-constraint (get-section dzn-str "AtomicConstraints"))
                 "\n"
                 (disjunctive-constraint (get-section dzn-str "DisjunctiveConstraints"))
                 "\n"
                 (soft-atomic-constraint (get-section dzn-str "SoftAtomicConstraints"))
                 "\n"
                 (direct-successor-constraint (get-section dzn-str "DirectSuccessors"))
                 "\n"
                 )
               ""))

; unit test for get-section
(module+ test
  (check-equal? (get-section "  k = 18; \n b = 9; \n  " "k") "k = 18;"))

; Get a section from a DZN problem
(define (get-section dzn-str tag)
  (let* ([pat (pregexp (format "(?s:.*?(\\b~a\\s+=\\s+.*?;).*)" tag))]
         [m (regexp-match pat dzn-str)])
    (if m (second m) "")))

; unit test for dzn-to-lp-k
(module+ test
  (check-equal? (dzn-to-lp-k "  k = 18; \n b = 9; \n  ") "% num of cables\n#const k=18.\n"))

; Convert K spec from DZN format to LP format
(define (dzn-to-lp-k dzn-str)
  (let ([m (regexp-match #px"(?s:\\s*k\\s+=\\s+(\\d+)\\s*;)" dzn-str)])
    (if m (format "% num of cables\n#const k=~a.\n" (second m)) "")))

; embedded unit test for dzn-to-lp-b
(module+ test
  (check-equal? (dzn-to-lp-b "  k = 18; \n b = 9; \n  ") "% num of 2-sided cables\n#const b=9.\n"))

; Convert B spec from DZN format to LP format
(define (dzn-to-lp-b dzn-str)
  (let ([m (regexp-match #px"(?s:\\s*b\\s+=\\s+(\\d+)\\s*;)" dzn-str)])
    (if m (format "% num of 2-sided cables\n#const b=~a.\n" (second m)) "")))
