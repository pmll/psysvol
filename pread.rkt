#lang racket

; read required text file from p-system image to standard output

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

(if (= 2 (length params))
    (let ((vol-file  (car params))
          (text-file (cadr params)))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (text (apply psys-vol 
                              'read
                              (vol-name vol-file)
                              (string-split (string-upcase text-file) "/"))))
            (if text
                (display text)
                (printf "P-system file: ~a not found." text-file)))
          (printf "Container File: ~a not found." vol-file)))
    (display-usage))
