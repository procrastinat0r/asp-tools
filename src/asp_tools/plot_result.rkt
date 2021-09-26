#lang racket
(require plot)

(define input-file
  (if (vector-empty? (current-command-line-arguments))
      "../../examples/plot_result/clingo_DL_V2_out2.txt"
      (first (vector->list (current-command-line-arguments)))))
  
; get all the lines with optimization values from a file
(define (get-opt-values file)
  (filter (lambda (line) (regexp-match #rx"Optimization:" line))
          (sequence->list (in-lines file))))
  
(define relevant-lines (call-with-input-file input-file get-opt-values))

(define plot-values
  (for/list ([line relevant-lines])
    (let* ([match (regexp-match #px"(\\d*):(\\d{2})\\.(\\d{3}) Optimization: ([\\d ]+)" line)]
           [time (+ (* (string->number (second match)) 60 1000)
                    (* (string->number (third match)) 1000)
                    (string->number (fourth match)))]
           [values (map string->number (string-split (fifth match)))]
           [opt-value (apply + values)])
      (list time opt-value))))

; We need to filter out values which have the same time stamp.
; Here we always use the latest one (see unit test below for an example).
(define (filter-duplicate-times values)
  (reverse (remove-duplicates (reverse values) #:key first)))

; an embedded unit test for filter-duplicate-times
(module+ test
  (require rackunit)
  (check-equal? (filter-duplicate-times '((50 31) (50 12) (122 10) (122 6) (329 5) (329 3) (329 1)))
                '((50 12) (122 6) (329 1)) ))

(define filtered-plot-values (filter-duplicate-times plot-values))

;filtered-plot-values

(define opt-values (map second filtered-plot-values))

;(plot (discrete-histogram plot-values
;                          ;#:y-min (* (apply min opt-values) 0.9)
;                          #:y-max (* (apply max opt-values) 1.1)))
(plot (lines plot-values
             #:y-min (* (apply min opt-values) 0.9)
             #:y-max (* (apply max opt-values) 1.1)))
