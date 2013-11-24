#lang racket

; create a new empty sub-volume

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
                  (eprintf "Cannot create sub-volume ~a.\n" svol)))
            (eprintf "Container file ~a not found.\n" vol-file)))
      (display-usage)))
