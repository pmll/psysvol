#lang racket

; display listing of given p-system volume image

(require "psysvol.rkt")
(require "error.rkt")

(define params (vector->list (current-command-line-arguments)))

(define indent-size 2)

; * becomes .*, ? becomes ., [] stays as is
; . becomes \.
; any other chars I should worry about escaping (in terms of psys file names)?
(define (glob->regexp g)
  (define (convert-char c)
    (cond ((char=? c #\*) ".*")
          ((char=? c #\?) ".")
          ((char=? c #\.) "\\.")
          (else (make-string 1 c))))
  (string-append (apply string-append 
                        "^" 
                        (map convert-char (string->list g)))
                 "$"))

(define (member-regexp? str pattern-lst)
  (ormap (lambda (pattern) (regexp-match? pattern str)) pattern-lst))

(define (format-date dt)
  (define (format-digits d)
    (if (< d 10)
        (string-append "0" (number->string d))
        (number->string d)))
  (format "~a/~a/~a ~a:~a"
          (format-digits (date-day dt))
          (format-digits (date-month dt))
          (format-digits (remainder (date-year dt) 100))
          (format-digits (date-hour dt))
          (format-digits (date-minute dt))))

(define (spaces n) (make-string n #\space))

(define (left-justify str w)
  (string-append str (spaces (- w (string-length str)))))

(define (right-justify str w)
  (string-append (spaces (- w (string-length str))) str))

(define (display-heading)
  (display "File/Volume Name          F.Blk  Blks Kind     Size Modified")
  (newline)
  (display "------------------------- ----- ----- ---- -------- --------------")
  (newline))

(define (file->string file-info indent-level)
  (format "~a ~a ~a ~a ~a ~a\n"
          (left-justify (string-append (spaces (* indent-level indent-size))
                                       (fi-file-name file-info))
                        25)
          (right-justify (number->string (fi-first-block file-info)) 5)
          (right-justify (number->string (- (fi-last-block file-info)
                                            (fi-first-block file-info)))
                         5)
          (left-justify (symbol->string (fi-file-kind file-info)) 4)
          (right-justify (number->string
                          (+ (block-bytes (- (fi-last-block file-info)
                                             (fi-first-block file-info)
                                             1))
                             (fi-last-byte file-info)))
                         8)
          (format-date (fi-last-access file-info))))

(define (listing->string file-lst indent-level match-lst)
  (define (file-obj->string file-obj)
    (let* ((file-info   (car file-obj))
           (file-wanted (or (null? match-lst)
                            (member-regexp? (fi-file-name file-info)
                                            match-lst)))
           (file-str    (file->string file-info indent-level))
           (listing-str (listing->string (cadr file-obj)
                                         (+ indent-level 1)
                                         (if file-wanted
                                             '()
                                             match-lst))))
      (if (or file-wanted (string>? listing-str ""))
          (string-append file-str listing-str)
          "")))
  (apply string-append (map file-obj->string file-lst)))

(with-user-error-handled
  (lambda ()
    (when (null? params) (display-usage "vol-file [file-1 file-2 ...]"))
    (let ((vol-file (car params))
          (match-lst (cdr params)))      
      (if (file-exists? vol-file)
          (let* ((psys-vol (make-psys-vol vol-file))
                 (listing-str (listing->string (list (psys-vol 'list)) 
                                               0 
                                               (map (lambda (str)
                                                      (glob->regexp 
                                                       (string-upcase str)))
                                                    match-lst))))
            (display-heading)
            (display listing-str)
            (when (string=? listing-str "")
              (display "No matching entries found") (newline)))
          (exit-error "Container File: ~a not found" vol-file)))))
