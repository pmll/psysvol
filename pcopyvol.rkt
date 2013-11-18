#lang racket

; copy volume
; this file is only here for test porpoises
; its reason for being will go away

(require "psysvol.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (display "Usage: ")
  (display (find-system-path 'run-file))
  (display " vol-file p-system-path")
  (newline))

; we allow for / and \ style path separators in the container file
(define (vol-name path-name) 
  (caar (map (lambda (s) (string-split s ".VOL")) 
             (regexp-match "[^/\\\\]*$" (string-upcase path-name)))))

(if (= 1 (length params))
    (let ((vol-file  (car params)))
      (if (file-exists? vol-file)
          (let ((psys-vol (make-psys-vol vol-file)))
             (printf "~a" (psys-vol 'bin-image)))
          (printf "Container File: ~a not found." vol-file)))
    (display-usage))
