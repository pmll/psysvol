#lang racket

; "crunch" p-system image

(require "psysvol.rkt")
(require "ripple.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (eprintf "Usage: ~a vol-file\n" (find-system-path 'run-file)))

(with-handlers
  ((exn:fail:user?
     (lambda (e) (eprintf "~a\n" (exn-message e)))))
  (if (= 1 (length params))
      (let ((vol-file (car params)))
        (if (file-exists? vol-file)
            (let* ((psys-vol (make-psys-vol vol-file))
                   (byte-str (psys-vol 'crunch)))
              (ripple vol-file)
              (let ((out (open-output-file vol-file)))
                (write-bytes byte-str out)
                (close-output-port out)))
            (eprintf "Container file ~a not found.\n" vol-file)))
      (display-usage)))
