#lang racket

; delete file from p-system image

(require "psysvol.rkt")
(require "ripple.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (eprintf "Usage: ~a vol-file p-system-path\n" (find-system-path 'run-file)))

(with-handlers
  ((exn:fail:user?
     (lambda (e) (eprintf "~a\n" (exn-message e)))))
  (if (= 2 (length params))
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
                  (eprintf "Cannot delete ~a.\n" del-file)))
            (eprintf "Container file ~a not found.\n" vol-file)))
      (display-usage)))
