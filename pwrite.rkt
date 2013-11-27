#lang racket

; write text from standard input to p-system image

(require "psysvol.rkt")
(require "ripple.rkt")

(define read-block-size 1024)

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (eprintf "Usage: ~a vol-file p-system-path\n" (find-system-path 'run-file)))

(define (input-bytes)
  (let ((bytes-read (read-bytes read-block-size)))
    (if (eof-object? bytes-read)
        #""
        (bytes-append bytes-read (input-bytes)))))

(with-handlers
  ((exn:fail:user?
     (lambda (e) (eprintf "~a\n" (exn-message e)))))
  (if (= 2 (length params))
      (let* ((vol-file  (car params))
             (text-file (cadr params)))
        (if (file-exists? vol-file)
            (let* ((psys-vol (make-psys-vol vol-file))
                   (file-path (cons (psys-vol 'vol-name)
                                    (string-split (string-upcase text-file) "/")))
                   (vol-path (take file-path (- (length file-path) 1)))
                   (mode (cond ((apply psys-vol 'file-exists? file-path) 'write)
                               ((apply psys-vol 'vol-exists? vol-path) 'create-file)
                               (else #f))))
              (if mode
                  (let ((byte-str (apply psys-vol 
                                         mode
                                         (append file-path (list (input-bytes))))))
                    (ripple vol-file)
                    (let ((out (open-output-file vol-file)))
                      (write-bytes byte-str out)
                      (close-output-port out)))
                  (eprintf "Cannot create ~a.\n" text-file)))
            (eprintf "Container file ~a not found.\n" vol-file)))
      (display-usage)))
