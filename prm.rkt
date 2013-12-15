#lang racket

; delete file from p-system image

(require "psysvol.rkt")
(require "ripple.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(with-user-error-handled
  (lambda ()
    (when (not (= 2 (length params))) (display-usage "vol-file p-system-path"))
    (let* ((vol-file (car params))
           (del-file (cadr params)))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (file-path (cons (psys-vol 'vol-name)
                                  (string-split (string-upcase del-file) "/")))
                 (vol-path (take file-path (- (length file-path) 1))))
            (if (apply psys-vol 'file-exists? file-path)
                (let ((byte-str (apply psys-vol 
                                       'delete
                                       file-path)))
                  (ripple vol-file)
                  (let ((out (open-output-file vol-file)))
                    (write-bytes byte-str out)
                    (close-output-port out)))
                (exit-error "Cannot delete ~a." del-file)))
          (exit-error "Container file ~a not found." vol-file)))))
