#lang racket

; delete sub volume from p-system image

(require "psysvol.rkt")
(require "ripple.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (eprintf "Usage: ~a [-f] vol-file p-system-path\n" (find-system-path 'run-file)))

(define (extract-params)
  (cond ((null? params) (values #f #f #f))
        ((string=? (car params) "-f")
         (if (= (length params) 3)
             (values #t (cadr params) (caddr params))
             (values #f #f #f)))
        ((= (length params) 2) (values #f (car params) (cadr params)))
        (else (values #f #f #f))))

(with-handlers
  ((exn:fail:user?
     (lambda (e) (eprintf "~a\n" (exn-message e)))))
  (let-values (((delete-sub-files? vol-file del-vol) (extract-params)))
    (if (and vol-file del-vol)
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (file-path (cons (psys-vol 'vol-name)
                                  (string-split (string-upcase del-vol) "/"))))
            (cond ((not (apply psys-vol 'vol-exists? file-path))
                   (eprintf "Sub-volume ~a not found.\n" del-vol))
                  ((and (not delete-sub-files?)
                        (not (apply psys-vol 'vol-empty? file-path)))
                   (eprintf "Sub-volume ~a is not empty.\n" del-vol))
                  (else
                    (let ((byte-str (apply psys-vol 'delete-vol file-path)))
                      (ripple vol-file)
                      (let ((out (open-output-file vol-file)))
                        (write-bytes byte-str out)
                        (close-output-port out))))))
          (eprintf "Container file ~a not found.\n" vol-file))
      (display-usage))))
