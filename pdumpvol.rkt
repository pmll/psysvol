#lang racket

; dump out volume contents to assist debugging

(require "psysvol.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(define (volume-type? t) (or (eq? t 'Svol) (eq? t 'Vol)))

(with-user-error-handled
  (lambda ()
    (when (not (= (length params) 1)) (display-usage "vol-file"))
    (let ((vol-file (car params)))
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (listing  (psys-vol 'list)))
            (let disp-vol ((listing listing) (indent 1) (file-path '()))
              (let ((file-info (car listing)))
                (display (make-string indent #\>))
                (display (fi-file-name file-info))
                (newline)
                (if (volume-type? (fi-file-kind file-info))
                    (for-each (lambda (x) (disp-vol x 
                                                    (+ indent 1)
                                                    (append file-path
                                                            (list (fi-file-name file-info)))))
                              (cadr listing))
                    (begin
                      (display (apply 
                                psys-vol 
                                'dump (append file-path
                                              (list (fi-file-name file-info)))))
                      (newline))))))
          (exit-error "Container file ~a not found" vol-file)))))
