#lang racket

; todo: duplicate, touch, write, make sub vol
;       debugging tool: serialise volume content so as to allow easy comparison
;                       between volumes

; fixme: need to check for vol expanding beyond max-vol-blocks

; points of corruption/non psys checking:
;        fi date/time parts out of range (eg month 13)
;        pstring length byte zero or greater than space set aside
;        fi block start/end beyond containing vol eov
;        fi last byte zero or beyond 512
;        vi number of files goes beyond vi last-block
;        vi first block - should be zero
;        no two file objects should overlap

(require racket/date)

(provide (contract-out (block-bytes (-> exact-integer? exact-integer?))
                       (make-psys-vol (-> string? 
                                          (->* (symbol?) () 
                                               #:rest (listof any/c) any/c))))
         (struct-out fi))

(define block-size 512)
(define dir-entry-size 26)
(define volume-header-blocks 2)
(define text-file-header-blocks 2)
(define max-vol-blocks 32767)
(define dle 16) ; dunno what dle stands for, but it denotes indentation
(define cr 13)
(define lf 10)
(define space 32)

(define (int16->bytes i) (bytes (remainder i 256) (quotient i 256)))

(define (bytes->int16 byte-str offset)
  (+ (bytes-ref byte-str offset) (* (bytes-ref byte-str (+ offset 1)) 256)))

; we convert between racket strings and byte-strings in the knowledge that all
; strings are ascii only. it's just that racket strings are nicer to work with
(define (string->bytes s) (list->bytes (map char->integer (string->list s))))

(define (string->pstring s len)
  (let ((str-len (min (string-length s) (- len 1))))
    (bytes-append (bytes str-len)
                  (subbytes (string->bytes s) 0 str-len)
                  (make-bytes (- len str-len 1) 0))))

(define (pstring->string byte-str offset)
  (let ((len (bytes-ref byte-str offset)))
    (format "~a" (subbytes byte-str (+ offset 1) (+ offset len 1)))))

; convert block count into a byte count
(define (block-bytes b) (* b block-size))

; convert byte count into a block count
(define (byte-blocks b) (quotient (+ b block-size -1) block-size))

(define (bytes-last-block b) (+ (remainder (- b 1) block-size) 1))

(define (time-stamp time-b date-b)
  (make-date 0
             (bitwise-bit-field time-b 9 15)
             (- (bitwise-bit-field time-b 4 9) 1)
             (bitwise-bit-field date-b 4 9)
             (bitwise-bit-field date-b 0 4)
             (bitwise-bit-field date-b 9 16)
             0
             0
             #f
             0))

; this seems silly, is there a (sensible) way I could define a reversible operation?
(define (file-kind kind-b)
  (case (bitwise-bit-field kind-b 0 4)
    ((0) 'None)
    ((1) 'XDsk)
    ((2) 'Code)
    ((3) 'Text)
    ((4) 'Info)
    ((5) 'Data)
    ((6) 'Graf)
    ((7) 'Foto)
    ((8) 'Secu)
    ((9) 'Svol)    
    (else 'Unkn)))

(define (file-kind->byte fk)
  (case fk
    ('None 0)
    ('XDsk 1)
    ('Code 2)
    ('Text 3)
    ('Info 4)
    ('Data 5)
    ('Graf 6)
    ('Foto 7)
    ('Secu 8)
    ('Svol 9)
    (else 0)))

; file information
(define-struct fi (first-block
                   last-block
                   file-kind
                   file-name
                   last-byte
                   last-access))

(define (fbytes->fi in-port start)
  (file-position in-port start)
  (let ((byte-str (read-bytes dir-entry-size in-port)))
    (make-fi (bytes->int16 byte-str 0)
             (bytes->int16 byte-str 2)
             (file-kind (bytes->int16 byte-str 4))
             (pstring->string byte-str 6)
             (bytes->int16 byte-str 22)
             (time-stamp (bytes->int16 byte-str 4)
                         (bytes->int16 byte-str 24)))))

(define (fi->bytes file-info)
  (let ((last-access (fi-last-access file-info)))
    (bytes-append (int16->bytes (fi-first-block file-info))
                  (int16->bytes (fi-last-block file-info))
                  (int16->bytes
                    (bitwise-ior 
                      (file-kind->byte (fi-file-kind file-info))
                      (arithmetic-shift (date-minute last-access) 9)
                      (arithmetic-shift (+ (date-hour last-access) 1) 4)))
                  (string->pstring (fi-file-name file-info) 16)
                  (int16->bytes (fi-last-byte file-info))
                  (int16->bytes
                    (bitwise-ior
                      (arithmetic-shift (date-day last-access) 4)
                      (date-month last-access)
                      (arithmetic-shift (remainder (date-year last-access) 100) 
                                        9))))))

; volume information
(define-struct vi (first-block
                   last-block
                   file-kind
                   volume-name
                   eov-block
                   number-of-files
                   load-time
                   last-boot))

(define (fbytes->vi in-port start)
  (file-position in-port start)
  (let ((byte-str (read-bytes 22 in-port)))
    (make-vi (bytes->int16 byte-str 0)
             (bytes->int16 byte-str 2)
             (bytes->int16 byte-str 4)
             (pstring->string byte-str 6)
             (bytes->int16 byte-str 14)
             (bytes->int16 byte-str 16)
             (bytes->int16 byte-str 18)
             (bytes->int16 byte-str 20))))

(define (vi->bytes vol-info)
  (bytes-append (int16->bytes (vi-first-block vol-info))
                (int16->bytes (vi-last-block vol-info))
                (int16->bytes (vi-file-kind vol-info))
                (string->pstring (vi-volume-name vol-info) 8)
                (int16->bytes (vi-eov-block vol-info))
                (int16->bytes (vi-number-of-files vol-info))
                (int16->bytes (vi-load-time vol-info))
                (int16->bytes (vi-last-boot vol-info))
                (make-bytes (- dir-entry-size 22) 0)))

; really need to look into file locking possibilities...
(define (register-container-file container-file)
  (if (file-exists? container-file)
      (let ((file-modified (file-or-directory-modify-seconds container-file)))
        (lambda (op)
          (cond ((not (file-exists? container-file))
                 (raise-user-error 'container-file
                                   "File ~a has been removed!"
                                   container-file))
                ((not (= (file-or-directory-modify-seconds container-file)
                         file-modified))
                 (raise-user-error 'container-file
                                   "File ~a has been modified."
                                   container-file))
                (else
                 (cond ((eq? op 'open-input)
                        (open-input-file container-file #:mode 'binary))
                       ((eq? op 'file-info)
                        (make-fi 0
                                 (byte-blocks (file-size container-file))
                                 'Vol
                                 container-file
                                 (bytes-last-block (file-size container-file))
                                 (seconds->date file-modified)))
                       (else (raise-user-error 'container-file
                                               "Invalid operation ~a."
                                               op)))))))
      (raise-user-error 'container-file 
                        "File not found: ~a." 
                        container-file)))

(define (fi-blocks-used file-info)
  (- (fi-last-block file-info) (fi-first-block file-info)))

(define (make-psys-vol container-file)
  (let* ((cf (register-container-file container-file))
         (in-port (cf 'open-input)))
    (begin0
      (make-volume in-port 0 (cf 'file-info) cf)
      (close-input-port in-port))))

(define (make-volume in-port block-offset file-info container-file)
  (let ((vol-info-start (block-bytes (+ block-offset volume-header-blocks))))
    (define (dir-entry n)
      (let ((file-info (fbytes->fi in-port (+ vol-info-start 
                                              (* n dir-entry-size)))))
        (if (eq? (fi-file-kind file-info) 'Svol)
            (make-volume in-port 
                         (+ block-offset (fi-first-block file-info))
                         file-info 
                         container-file)
            (make-file block-offset file-info container-file))))
    (let* ((vol-info (fbytes->vi in-port vol-info-start))
           (dir (append (map dir-entry
                             (range 1 (+ (vi-number-of-files vol-info) 1)))
                        (list (make-new-object)))))
      (lambda (op . arg)
        (cond ((eq? op 'list)
               (list (make-fi
                      block-offset
                      (+ block-offset (vi-eov-block vol-info))
                      (fi-file-kind file-info)
                      (vi-volume-name vol-info)
                      block-size
                      (fi-last-access file-info))
                     (filter identity (map (lambda (f) (f 'list)) dir))))
              ((eq? op 'vol-name) (vi-volume-name vol-info))
              ((eq? op 'outer-used) (fi-blocks-used file-info))
              ((eq? op 'inner-used)
               (foldl + (vi-last-block vol-info) (map (lambda (f) (f op)) dir)))
              ((eq? op 'outer-free)
               (if (string=? (car arg) (vi-volume-name vol-info))
                   (if (= (length arg) 1)
                       (- (vi-eov-block vol-info)
                          (foldl + 
                                 (vi-last-block vol-info)
                                 (map (lambda (f) (f 'outer-used)) dir)))
                       (ormap (lambda (f) (apply f op (cdr arg))) dir))
                   #f))
              ((eq? op 'inner-free)
               (if (string=? (car arg) (vi-volume-name vol-info))
                   (if (= (length arg) 1)
                       (- (vi-eov-block vol-info)
                          (foldl + 
                                 (vi-last-block vol-info)
                                 (map (lambda (f) (f 'inner-used)) dir)))
                       (ormap (lambda (f) (apply f op (cdr arg))) dir))
                   #f))
              ((eq? op 'bin-image)
               ; in the final version, we will be able to do a straight
               ; forward copy of the entire volume image, but for now, 
               ; this is proof of concept...
               (let ((vol-image (vol->bytes vol-info dir 'bin-image)))
                 (if (eq? (fi-file-kind file-info) 'Vol)
                     vol-image
                    (values vol-image
                            (make-fi (fi-first-block file-info)
                                     (+ (byte-blocks (bytes-length vol-image))
                                        (fi-first-block file-info))
                                     (fi-file-kind file-info)
                                     (fi-file-name file-info)
                                     (fi-last-byte file-info)
                                     (fi-last-access file-info))))))
              ((or (eq? op 'write) (eq? op 'create-file))
               (if (string=? (car arg) (vi-volume-name vol-info))
                 ; it's on this path, we have to descend it
                 (let ((vol-image (apply vol->bytes vol-info dir op (cdr arg))))
                   (if (eq? (fi-file-kind file-info) 'Vol)
                     vol-image
                     (values vol-image
                             (make-fi (fi-first-block file-info)
                                      (+ (byte-blocks (bytes-length vol-image))
                                         (fi-first-block file-info))
                                      (fi-file-kind file-info)
                                      (fi-file-name file-info)
                                      (fi-last-byte file-info)
                                      (fi-last-access file-info)))))
                   ; it's not on this path,
                   ; we only need to take the entire svol image as is
                   (let ((in-port (container-file 'open-input)))
                     (begin0
                       (values
                         (file->bytes in-port 0 file-info)
                         file-info)
                       (close-input-port in-port)))))
              ((eq? op 'file-exists?)
               (and (> (length arg) 1)
                    (string=? (car arg) (vi-volume-name vol-info))
                    (ormap (lambda (f) (apply f op (cdr arg))) dir)))
              ((eq? op 'vol-exists?)
               (and (not (null? arg))
                    (string=? (car arg) (vi-volume-name vol-info))
                    (or (= (length arg) 1)
                        (ormap (lambda (f) (apply f op (cdr arg))) dir))))
              ((eq? op 'read)
               (if (and (> (length arg) 1) (string=? (car arg) (vi-volume-name vol-info)))
                   ; using ormap we get the converted text or #f
                   (ormap (lambda (f) (apply f op (cdr arg))) dir)
                   #f))
              (else (raise-user-error 'volume-obj "Unknown operation: ~a." op)))))))

                                                    
(define (vol->bytes vol-info dir op . arg)
  (define (vol-iter dir block-offset dir-bytes vol-bytes cnt)
    (if (null? dir)
      ; here would be the place to introduce new objects
      ; new file required if op is 'create-file, length arg = 3
      ;(let* ((new-object (if ... 
      (let ((new-eov-block (max (vi-eov-block vol-info)
                                (+ (vi-last-block vol-info)
                                   (byte-blocks (bytes-length vol-bytes))))))
        (bytes-append (make-bytes (block-bytes volume-header-blocks) 0)
                      (vi->bytes (make-vi (vi-first-block vol-info)
                                          (vi-last-block vol-info)
                                          (vi-file-kind vol-info)
                                          (vi-volume-name vol-info)
                                          new-eov-block
                                          cnt
                                          (vi-load-time vol-info)
                                          (vi-last-boot vol-info)))
                      dir-bytes
                      (make-bytes (max (- (block-bytes (vi-last-block vol-info))
                                          (block-bytes volume-header-blocks)
                                          (bytes-length dir-bytes)
                                          dir-entry-size)
                                       0)
                                  0)
                      vol-bytes
                      (make-bytes (- (block-bytes new-eov-block)
                                     (bytes-length vol-bytes)
                                     (block-bytes (vi-last-block vol-info)))
                                  0)))
      (let-values (((this-image this-fi) (apply (car dir) op arg)))
        (if this-image
          (let ((new-block-offset (+ block-offset (fi-blocks-used this-fi))))
            (vol-iter (cdr dir)
                      new-block-offset
                      (bytes-append dir-bytes
                                    (fi->bytes
                                      (make-fi block-offset
                                               new-block-offset
                                               (fi-file-kind this-fi)
                                               (fi-file-name this-fi)
                                               (fi-last-byte this-fi)
                                               (fi-last-access this-fi))))
                      (bytes-append vol-bytes this-image)
                      (+ cnt 1)))
          (vol-iter (cdr dir) block-offset dir-bytes vol-bytes cnt)))))
  (vol-iter dir (vi-last-block vol-info) #"" #"" 0))
 
(define (make-file block-offset file-info container-file)
  (lambda (op . arg)
    (cond ((eq? op 'list)
           (list (make-fi (+ (fi-first-block file-info) block-offset)
                          (+ (fi-last-block file-info) block-offset)
                          (fi-file-kind file-info)
                          (fi-file-name file-info)
                          (fi-last-byte file-info)
                          (fi-last-access file-info))
                 '()))
          ; the two type of used forms are meaningless for files, we just
          ; provide a consistant interface for volume and file objects
          ((eq? op 'outer-used) (fi-blocks-used file-info))
          ((eq? op 'inner-used) (fi-blocks-used file-info))
          ((eq? op 'bin-image)
           (let ((in-port (container-file 'open-input)))
             (begin0
               (values
                 (file->bytes in-port block-offset file-info)
                 file-info)
               (close-input-port in-port))))
          ((or (eq? op 'write) (eq? op 'create-file))
           (if (string=? (car arg) (fi-file-name file-info))
             ; this is the file we are meant to be overwriting
             (if (eq? (fi-file-kind file-info) 'Text)
               (let ((new-image (text->file (cadr arg))))
                 (values (bytes-append new-image
                                       (make-bytes (- block-size
                                                      (bytes-last-block (bytes-length new-image)))
                                                   0))
                         (make-fi (fi-first-block file-info)
                                  (+ (byte-blocks (bytes-length new-image))
                                     (fi-first-block file-info))
                                  (fi-file-kind file-info)
                                  (fi-file-name file-info)
                                  (bytes-last-block (bytes-length new-image))
                                  (current-date))))
               (raise-user-error "Unable to write to file kind ~a."
                                 (fi-file-kind file-info)))
             ; it's not this file, just produce a bin-image of it
             (let ((in-port (container-file 'open-input)))
               (begin0
                 (values
                   (file->bytes in-port block-offset file-info)
                   file-info)
                 (close-input-port in-port)))))
          ((eq? op 'file-exists?)
           (and (= (length arg) 1)
                (string=? (car arg) (fi-file-name file-info))))
          ((eq? op 'vol-exists?) #f)
          ((eq? op 'read)
           (if (and (= (length arg) 1)
                    (string=? (car arg) (fi-file-name file-info)))
               (if (eq? (fi-file-kind file-info) 'Text)
                   (let ((in-port (container-file 'open-input)))
                     (begin0
                       (file->text in-port block-offset file-info)
                       (close-input-port in-port)))
                   (raise-user-error 'file-obj 
                                     "Unable to read file kind ~a."
                                     (fi-file-kind file-info)))
               #f))
          (else #f))))

(define (file->bytes in-port block-offset file-info)
  (file-position in-port (block-bytes (+ block-offset
                                         (fi-first-block file-info))))
  (read-bytes (block-bytes (fi-blocks-used file-info))
              in-port))
             
(define (file->text in-port block-offset file-info)
  (define (dle-indent n) (make-bytes (- n space) space))
  (file-position in-port (block-bytes (+ block-offset
                                         (fi-first-block file-info)
                                         text-file-header-blocks)))
  (let* ((file-length (+ (block-bytes (- (fi-blocks-used file-info)
                                         text-file-header-blocks
                                         1))
                         (fi-last-byte file-info)))
         (file-bytes (read-bytes file-length in-port)))
    (let convert-bytes ((pos 0) (start-of-line #t) (text #""))
      (if (= pos file-length)
          text
          (let ((pos-byte (bytes-ref file-bytes pos)))
            (cond ((zero? pos-byte) (convert-bytes (+ pos 1) 
                                                   start-of-line 
                                                   text))
                  ((= pos-byte cr) (convert-bytes (+ pos 1)
                                                  #t
                                                  (bytes-append text #"\n")))
                  ((and (= pos-byte dle) start-of-line)
                   (convert-bytes (+ pos 2)
                                  #f
                                  (bytes-append text 
                                                (dle-indent
                                                  (bytes-ref file-bytes
                                                             (+ pos 1))))))
                  (else (convert-bytes (+ pos 1)
                                       #f
                                       (bytes-append text
                                                     (bytes pos-byte))))))))))
 
    
; we use current-input-port & eof-object? eventually...
(define (text->file byte-str)
  (define (number-of-spaces pos)
    (if (and (< pos (bytes-length byte-str)) (= (bytes-ref byte-str pos) space))
        (+ 1 (number-of-spaces (+ pos 1)))
        0))
  (define (crlf? pos)
    (and (< pos (- (bytes-length byte-str) 1))
         (= (bytes-ref byte-str pos) cr)
         (= (bytes-ref byte-str (+ pos 1)) lf)))
  (let convert-bytes ((pos 0) (start-of-line #t) (file-text #""))
    (if (= pos (bytes-length byte-str))
        (bytes-append (make-bytes (block-bytes text-file-header-blocks) 0)
                      file-text 
                      (bytes 0))
        (let ((pos-byte (bytes-ref byte-str pos)))
          (cond (start-of-line
                 (let ((indent (min (number-of-spaces pos) 223)))
                   (if (> indent 2)
                       (convert-bytes (+ pos indent)
                                      #f
                                      (bytes-append file-text
                                                    (bytes dle (+ space 
                                                                  indent))))
                       (convert-bytes pos #f file-text))))
                ((crlf? pos)
                 (convert-bytes (+ pos 2)
                                #t
                                (bytes-append file-text (bytes cr))))
                ((or (= pos-byte cr) (= pos-byte lf))
                 (convert-bytes (+ pos 1)
                                #t
                                (bytes-append file-text (bytes cr))))
                (else (convert-bytes (+ pos 1)
                                     #f
                                     (bytes-append file-text
                                                   (bytes pos-byte)))))))))

; place holder for new file system objects
(define (make-new-object)
  (lambda (op . arg)
    (cond ((eq? op 'outer-used) 0)
          ((eq? op 'inner-used) 0)
          ((eq? op 'write) (values #f #f))
          ((eq? op 'create-file)
           (if (= (length arg) 2)
             (let ((new-image (text->file (cadr arg))))
               (values (bytes-append new-image
                                     (make-bytes (- block-size
                                                    (bytes-last-block (bytes-length new-image)))
                                                 0))
                       (make-fi 0
                                (byte-blocks (bytes-length new-image))
                                'Text
                                (car arg) ; fixme: too long a filename will just get truncate
                                (bytes-last-block (bytes-length new-image))
                                (current-date))))
             (values #f #f)))
          ; 'create-vol
          (else #f))))





; structure of listing return :

; '(file-info-object-for-vol (... list for things contained in vol ...))
; file '(file-info ())
; svol '(file-info (... list for things contained in vol ...))

; eg (source-vol ( ... ))
; ((file1.text ()) (file2.text ()) (123.svol ((file3.text ()) (file4.text ()))))

