#lang racket

; "crunch" p-system image

(require "psysvol.rkt")
(require "ripple.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(with-user-error-handled
  (lambda ()
    (when (not (= 1 (length params))) (display-usage "vol-file"))
    (let ((vol-file (car params)))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (byte-str (psys-vol 'crunch)))
            (ripple vol-file)
            (let ((out (open-output-file vol-file)))
              (write-bytes byte-str out)
              (close-output-port out)))
          (exit-error "Container file ~a not found." vol-file)))))
