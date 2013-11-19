#lang racket

; get free blocks in given volume
; use -s to total free blocks in sub-volumes too

(require "psysvol.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (display "Usage: ")
  (display (find-system-path 'run-file))
  (display " [-s] vol-file [p-system-sub-volume]")
  (newline))

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

(let-values (((mode vol-file svol) (extract-params)))
  (if (and mode vol-file svol)
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
                (printf "P-system subvolume: ~a not found." svol)))
          (printf "Container File: ~a not found." vol-file))
      (display-usage)))
