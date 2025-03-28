#lang racket

;; adding operations + 

(define (tokenize input)
  (define (is-symbol-char? c)
    (or (char-numeric? c)
        (char=? c #\.)s
        (char=? c #\$)
        (char=? c #\-)))
  (define (tokenize-helper chars current-token tokens)
    (cond
      [(null? chars)
       (if (null? current-token)
           (reverse tokens)
           (reverse (cons (list->string (reverse current-token)) tokens)))]
      [(char-whitespace? (car chars))
       (if (null? current-token)
           (tokenize-helper (cdr chars) '() tokens)
           (tokenize-helper (cdr chars) '() (cons (list->string (reverse current-token)) tokens)))]
      [(or (member (car chars) (list #\+ #\* #\/))
           (and (char=? (car chars) #\-) (null? current-token)))
       (tokenize-helper (cdr chars) '() (cons (string (car chars))
                                              (if (null? current-token)
                                                  tokens
                                                  (cons (list->string (reverse current-token)) tokens))))]
      [else
       (tokenize-helper (cdr chars) (cons (car chars) current-token) tokens)]))
  (tokenize-helper (string->list input) '() '()))

;; Evaluator for + only
(define (evaluate tokens)
  (cond
    [(null? tokens) #f]
    [else
     (let ([token (car tokens)]
           [rest (cdr tokens)])
       (cond
         [(number? (string->number token))
          (list (string->number token) rest)]
         [(string=? token "+")
          (let ([eval1 (evaluate rest)])
            (if eval1
                (let* ([v1 (first eval1)]
                       [rest1 (second eval1)]
                       [eval2 (evaluate rest1)])
                  (if eval2
                      (list (+ v1 (first eval2)) (second eval2))
                      #f))
                #f))]
         [else #f]))]))

;; Wrapper
(define (run input)
  (let ([tokens (tokenize input)])
    (let ([result (evaluate tokens)])
      (if (and result (null? (second result)))
          (displayln (real->double-flonum (first result)))
          (displayln "Error")))))
          

;; Example usage
(run "+ 3 4") ; => 7.0
(run "+ 3")   ; => Error
