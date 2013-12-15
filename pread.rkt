#lang racket

; read required text file from p-system image to standard output

(require "psysvol.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(with-user-error-handled
  (lambda ()
    (when (not (= 2 (length params))) (display-usage "vol-file p-system-path"))
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
                (exit-error "P-system file: ~a not found." text-file)))
          (exit-error "Container File: ~a not found." vol-file)))))
