#lang racket
(require "mode.rkt")

;; --- Tokenizer ---
(define (tokenize input)
  (define (is-symbol-char? c)
    (or (char-numeric? c)
        (char=? c #\.)
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

;; history/History Accesss
(define (get-history-value id history)
  (let ([index (- id 1)])
    (if (or (< index 0) (>= index (length history)))
        #f
        (list-ref (reverse history) index))))

;; --- the core of the prefix ---
(define (evaluate-expr tokens history)
  (cond
    [(null? tokens) #f]
    [else
     (let ([token (car tokens)]
           [rest (cdr tokens)])
       (cond
         ;; Number literal
         [(number? (string->number token))
          (list (string->number token) rest)]

         ;; History reference like $1
         [(and (equal? (substring token 0 1) "$")
               (> (string-length token) 1)
               (number? (string->number (substring token 1))))
          (let* ([id (string->number (substring token 1))]
                 [val (get-history-value id history)])
            (if val
                (list val rest)
                #f))]

         ;; binary operators -
         [(string=? token "-")
          (let ([eval1 (evaluate-expr rest history)])
            (if (and eval1 (list? eval1))
                (let ([v1 (first eval1)]
                      [r1 (second eval1)])
                  (list (- v1) r1))
                #f))]

         ;; Binary operators +, *, /
         [(or (string=? token "+") (string=? token "*") (string=? token "/"))
          (let ([eval1 (evaluate-expr rest history)])
            (if (and eval1 (list? eval1))
                (let* ([v1 (first eval1)]
                       [rest1 (second eval1)]
                       [eval2 (evaluate-expr rest1 history)])
                  (if (and eval2 (list? eval2))
                      (let ([v2 (first eval2)]
                        [rest2 (second eval2)])
                        (cond
                        [(string=? token "+") (list (+ v1 v2) rest2)]
                         [(string=? token "*") (list (* v1 v2) rest2)]
                         [(string=? token "/")
                           (if (= v2 0)
                               #f
                               (list (quotient v1 v2) rest2))]))
                      #f))
                #f))]

         
         [else #f]))]))
         ;; anything other then that should be error or Invalid

;; funtion for any left overs
(define (evaluate tokens history)
  (let ([result (evaluate-expr tokens history)])
    (if (and result (null? (second result)))
        (first result)
        #f)))

;; the main:
(define (main-loop history)
  (let ([input (if prompt?
                   (begin (display "enter here:: ") (read-line))
                   (read-line))])
    (cond
      [(eof-object? input) (void)]
      [(equal? input "quit") (void)]
      [else
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (displayln "you have entered an Invalid Expression, try again")
                          (main-loop history))])
         (let* ([tokens (tokenize input)]
                [eval-result (evaluate tokens history)])
           (if (number? eval-result)
               (let ([new-id (+ 1 (length history))])
                 (display new-id)
                 (display ":: ")
                 (display (real->double-flonum eval-result))
                 (newline)
                 (main-loop (cons eval-result history)))
               (begin
                 (displayln "Invalid epression try again!!")
                 (main-loop history)))))])))


(main-loop '())
;;done