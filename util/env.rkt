#lang racket

(provide (all-defined-out))

; Definição do Environment
(define empty-env
  (lambda (var)
    (error "No bind")))

(define (extend-env var value env)
  (lambda (svar)
    (if (equal? svar var) value
        (apply-env env svar))))

(define (apply-env env var)
  (
    (displayln env)
    (displayln var)
    (env var)
  )
)

(define init-env empty-env)
