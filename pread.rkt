#lang racket

; read required text file from p-system image to standard output

(require "psysvol.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (eprintf "Usage: ~a vol-file p-system-path" (find-system-path 'run-file)))

(with-handlers 
  ((exn:fail:user?
     (lambda (exn) (eprintf (exn-message exn)))))
  (if (= 2 (length params))
      (let ((vol-file  (car params))
            (text-file (cadr params)))
        (if (file-exists? vol-file)
            (let* ((psys-vol (make-psys-vol vol-file))
                   (text (apply psys-vol 
                                'read
                                (psys-vol 'vol-name)
                                (string-split (string-upcase text-file) "/"))))
              (if text
                  (display text)
                  (eprintf "P-system file: ~a not found." text-file)))
            (eprintf "Container File: ~a not found." vol-file)))
      (display-usage)))
