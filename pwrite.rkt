#lang racket

; write text from standard in to p-system image

(require "psysvol.rkt")

(define read-block-size 1024)

(define params (vector->list (current-command-line-arguments)))

(define (display-usage)
  (display "Usage: ")
  (display (find-system-path 'run-file))
  (display " vol-file p-system-path")
  (newline))

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
                              (string-split (string-upcase text-file) "/"))))
            (if can-write?
                (printf "~a"
                        (apply psys-vol
                               'write
                               (psys-vol 'vol-name)
                               (append (string-split (string-upcase text-file) "/")
                                       (list (input-bytes)))))
                (printf "Can't write to ~a" text-file)))
          (printf "Container File: ~a not found." vol-file)))
    (display-usage))
