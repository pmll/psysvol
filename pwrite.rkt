#lang racket

; write text from standard input to p-system image

(require "psysvol.rkt")
(require "ripple.rkt")

(define read-block-size 1024)

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (eprintf "Usage: ~a vol-file p-system-path" (find-system-path 'run-file)))

(define (input-bytes)
  (let ((bytes-read (read-bytes read-block-size)))
    (if (eof-object? bytes-read)
        #""
        (bytes-append bytes-read (input-bytes)))))

(if (= 2 (length params))
    (let ((vol-file  (car params))
          (text-file (cadr params)))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (can-write? (apply psys-vol
                                    'can-write
                                    (psys-vol 'vol-name)
                                    (string-split (string-upcase text-file)
                                                  "/"))))
            (if can-write?
                (let ((byte-str (apply psys-vol
                                       'write
                                       (psys-vol 'vol-name)
                                       (append (string-split (string-upcase text-file)
                                                             "/")
                                               (list (input-bytes))))))
                  (ripple vol-file)
                  (let ((out (open-output-file vol-file)))
                    (write-bytes byte-str out)
                    (close-output-port out)))
                (eprintf "Can't write to ~a" text-file)))
          (eprintf "Container File: ~a not found." vol-file)))
    (display-usage))
