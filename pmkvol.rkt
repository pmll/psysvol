#lang racket

; create a new empty sub-volume

(require "psysvol.rkt")
(require "ripple.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(with-user-error-handled
  (lambda ()
    (when (not (= 2 (length params))) (display-usage "vol-file p-system-path"))
    (let* ((vol-file (car params))
           (svol     (cadr params)))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (file-path (cons (psys-vol 'vol-name)
                                  (string-split (string-upcase svol) "/")))
                 (vol-path (take file-path (- (length file-path) 1))))
            (if (and (apply psys-vol 'vol-exists? vol-path)
                     (not (apply psys-vol 'vol-exists? file-path)))
                (let ((byte-str (apply psys-vol 'create-vol file-path)))
                  (ripple vol-file)
                  (let ((out (open-output-file vol-file)))
                    (write-bytes byte-str out)
                    (close-output-port out)))
                (exit-error "Cannot create sub-volume ~a." svol)))
          (exit-error "Container file ~a not found." vol-file)))))
