#lang racket

(define input-file
  (if (vector-empty? (current-command-line-arguments))
      "../../examples/dzn-to-lp/R046.lp"
      (first (vector->list (current-command-line-arguments)))))

; embedded unit test for atomic-constraints
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
    (check-equal? (atomic-constraint dzn-str-empty) "%% atomic constraints\n")
    (check-equal? (atomic-constraint dzn-str-1-constraint) "%% atomic constraints\natomiccon(14,16).\n")
    (check-equal? (atomic-constraint dzn-str-2-constraint) "%% atomic constraints\natomiccon(4,9).\natomiccon(14,16).\n"))) 

; Convert an Atomic Constraint from DZN to LP format"
(define (atomic-constraint s)
  (common-atomic-constraint s "AtomicConstraints" "atomic" "atomic"))

(define (common-atomic-constraint s dzn-tag lp-tag rule-name)
  (let* ([pat (pregexp (format "(?s:~a\\s+=\\s+\\[\\|(.*)\\]\\s*;.*)" dzn-tag))]
         [match (regexp-match pat s)]
         [c (if (list? match) (second match) "")] ; all constraints as string
         [col (if (empty? c)
                 ""
                 (for/list ([c (regexp-match* #px"\\d+,\\s+\\d+\\|" c)])
                   (let ([m (regexp-match* #px"\\d+" c)])
                     (format "~acon(~a,~a).\n" rule-name (first m) (second m)))))] ; list with one formated string per constraint
                 
         )
    (string-append* (format "%% ~a constraints\n" lp-tag) col)))
