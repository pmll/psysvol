#lang racket

; get free blocks in given volume
; use -s to total free blocks in sub-volumes too

(require "psysvol.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (extract-params)
  (define (set-mode params)
    (cond ((null? params) (values #f #f #f))
          ((string=? (car params) "-s") (set-vol (cdr params) 'inner-free))
          (else (set-vol params 'outer-free))))
  (define (set-vol params mode)
    (cond ((null? params) (values #f #f #f))
          (else (set-sub-vol (cdr params) mode (car params)))))
  (define (set-sub-vol params mode vol)
    (cond ((null? params) (values mode vol ""))
          ((null? (cdr params)) (values mode vol (car params)))
          (else (values #f #f #f))))
  (set-mode params))

(with-user-error-handled
  (lambda ()
    (let-values (((mode vol-file svol) (extract-params)))
      (when (not (and mode vol-file svol))
        (display-usage "[-s] vol-file [p-system-sub-volume]"))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (free (apply psys-vol 
                              mode
                              (psys-vol 'vol-name)
                              (string-split (string-upcase svol) "/"))))
            (if free
                (printf "~a blocks free (i.e. ~a bytes)\n"
                        free
                        (block-bytes free))
                (exit-error "P-system subvolume: ~a not found." svol)))
          (exit-error "Container File: ~a not found." vol-file)))))
