#lang racket

;; kinda working - just + for now
;; handle more stuff later maybe

(define (tokenize input)
  (define (tokenize-helper chars current-token tokens)
    (cond
      [(null? chars)
       (if (null? current-token)
           (reverse tokens)
           (reverse (cons (list->string (reverse current-token)) tokens)))]
      [(char-whitespace? (car chars)) ;; split by space
       (if (null? current-token)
           (tokenize-helper (cdr chars) '() tokens)
           (tokenize-helper (cdr chars) '() (cons (list->string (reverse current-token)) tokens))))]
      [(or (member (car chars) (list #\+ #\* #\/)) ; handles ops
           (and (char=? (car chars) #\-) (null? current-token)))
       (tokenize-helper (cdr chars) '() (cons (string (car chars))
        (if (null? current-token)
                                                  tokens
                                                  (cons (list->string (reverse current-token)) tokens))))]
      [else
       (tokenize-helper (cdr chars) (cons (car chars) current-token) tokens)]))
  (tokenize-helper (string->list input) '() '()))


;; evaluator (only does + rn)
(define (evaluate tokens)
  (cond
    [(null? tokens) #f] ;; empty is bad
    [else
     (let ([token (car tokens)]
           [rest (cdr tokens)])
       (cond
         ;; number
         [(number? (string->number token))
          (list (string->number token) rest)]
         
         ;; + operator (assume 2 args)
         [(string=? token "+")
          (let ([eval1 (evaluate rest)])
            (if eval1
                (let* ([v1 (first eval1)]
                       [rest1 (second eval1)]
                       [eval2 (evaluate rest1)])
                  (if eval2
                      (list (+ v1 (first eval2)) (second eval2))
                      #f)) ; bad second arg
                #f))] ; bad first arg
         
         ;; fallback
         [else #f]))]))


;; run it (prints result or error)
(define (run input)
  (let* ([tokens (tokenize input)]
   [result (evaluate tokens)])
    (if (and result (null? (second result)))
        (displayln (real->double-flonum (first result)))
        (displayln "there is an Error: Invalid expression lol")))) ;; very scientific

;; some tests
(run "+ 5 10")
(run "+ 8")
(run "+ 2 2 2")
(run "7")
