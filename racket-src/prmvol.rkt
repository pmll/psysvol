#lang racket

; delete sub volume from p-system image

(require "psysvol.rkt")
(require "ripple.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (extract-params)
  (cond ((null? params) (values #f #f #f))
        ((string=? (car params) "-f")
         (if (= (length params) 3)
             (values #t (cadr params) (caddr params))
             (values #f #f #f)))
        ((= (length params) 2) (values #f (car params) (cadr params)))
        (else (values #f #f #f))))

(with-user-error-handled
  (lambda ()
    (let-values (((delete-sub-files? vol-file del-vol) (extract-params)))
      (when (not (and vol-file del-vol))
        (display-usage "vol-file p-system-path"))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (file-path (cons (psys-vol 'vol-name)
                                  (string-split (string-upcase del-vol) "/"))))
            (cond ((not (apply psys-vol 'vol-exists? file-path))
                   (exit-error "Sub-volume ~a not found." del-vol))
                  ((and (not delete-sub-files?)
                        (not (apply psys-vol 'vol-empty? file-path)))
                   (exit-error "Sub-volume ~a is not empty." del-vol))
                  (else
                    (let ((byte-str (apply psys-vol 'delete-vol file-path)))
                      (ripple vol-file)
                      (let ((out (open-output-file vol-file)))
                        (write-bytes byte-str out)
                        (close-output-port out))))))
          (exit-error "Container file ~a not found." vol-file)))))
