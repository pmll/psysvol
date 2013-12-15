#lang racket

; exit error handling

(provide (contract-out (exit-error (->* (string?) () #:rest (listof any/c) any))
                       (display-usage (-> string? any))
                       (with-user-error-handled (-> procedure? any))))

(define (exit-error str . arg)
  (apply eprintf str arg)
  (eprintf "\n")
  (exit 1))

(define (display-usage usage)
  (exit-error "Usage: ~a ~a" (find-system-path 'run-file) usage))

(define (with-user-error-handled proc)
  (with-handlers
    ((exn:fail:user? (lambda (e) (exit-error (exn-message e)))))
    (proc)
    (exit 0)))