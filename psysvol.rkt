#lang racket

; todo: duplicate, touch, write, make sub vol

; the next step := duplicate objects - pass responsibility down to each object to
; duplicate itself. when that comes to files, they read in their entire contents as
; is and make a full binary copy of it - then pass back the details of space used. 
; when it comes to volumes, we rewrite the directory list based on the details
; received back from each sub object and we pass on the space used by the sub-object.

; need to think about - how to write back volume info and dir (file-info)

(provide (contract-out (block-bytes (-> exact-integer? exact-integer?))
                       (make-psys-vol (-> string? 
                                          (->* (symbol?) () 
                                               #:rest (listof any/c) any/c))))
         (struct-out fi))

(define block-size 512)
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
(define (byte-blocks b)
  (+ (quotient b block-size)
     (if (zero? (remainder b block-size)) 0 1)))

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

(define-struct fi (first-block
                   last-block
                   file-kind
                   file-name
                   last-byte
                   last-access))

(define (fbytes->fi in-port start)
  (file-position in-port start)
  (let ((byte-str (read-bytes 26 in-port)))
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
                      (arithmetic-shift (date-year last-access) 9))))))

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
                (make-bytes 4 0)))

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
                                 ; fixme: a result of 0 really means 512
                                 (remainder (file-size container-file)
                                            block-size)
                                 (seconds->date file-modified)))
                       (else (raise-user-error 'container-file
                                               "Invalid operation ~a."
                                               op)))))))
      (raise-user-error 'container-file 
                        "File not found: ~a." 
                        container-file)))

(define (file-blocks-used file-info)
  (- (fi-last-block file-info) (fi-first-block file-info)))

(define (make-psys-vol container-file)
  (let* ((cf (register-container-file container-file))
         (in-port (cf 'open-input)))
    (begin0
      (make-volume in-port 0 (cf 'file-info) cf)
      (close-input-port in-port))))

(define (make-volume in-port block-offset file-info container-file)
  (let ((vol-info-start (block-bytes (+ block-offset volume-header-blocks))))
    (define (add-dir-entry n)
      (let ((file-info (fbytes->fi in-port (+ vol-info-start (* n 26)))))
        (if (eq? (fi-file-kind file-info) 'Svol)
            (make-volume in-port 
                         (+ block-offset (fi-first-block file-info))
                         file-info 
			 container-file)
            (make-file in-port block-offset file-info container-file))))
    (let* ((vol-info (fbytes->vi in-port vol-info-start))
           (dir (map add-dir-entry (range 1 (+ (vi-number-of-files vol-info) 1)))))
      (lambda (op . arg)
        (cond ((eq? op 'list)
               (list (make-fi
                      block-offset
                      (+ block-offset (vi-eov-block vol-info))
                      (fi-file-kind file-info)
                      (vi-volume-name vol-info)
                      block-size
                      (fi-last-access file-info))
                     (map (lambda (f) (f 'list)) dir)))
              ((eq? op 'outer-used) (file-blocks-used file-info))
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
               (let ((vol-image (vol->bytes vol-info dir)))
                 (if (eq? (fi-file-kind file-info) 'Vol)
                     vol-image
                    (values vol-image
                            (make-fi (fi-first-block file-info)
                                     (byte-blocks (bytes-length vol-image))
                                     (fi-file-kind file-info)
                                     (fi-file-name file-info)
                                     (fi-last-byte file-info)
                                     (fi-last-access file-info))))))
              ((eq? op 'read)
               (if (and (> (length arg) 1) (string=? (car arg) (vi-volume-name vol-info)))
                   ; using ormap we get the converted text or #f
                   (ormap (lambda (f) (apply f op (cdr arg))) dir)
                   #f))
              (else (raise-user-error 'volume-obj "Unknown operation: ~a." op)))))))

                                                    
(define (vol->bytes vol-info dir)
  (define (vol-iter dir block-offset dir-bytes vol-bytes)
    (if (null? dir)
      (let ((new-eov-block (max (vi-eov-block vol-info)
                                (+ (vi-last-block vol-info)
                                   (byte-blocks (bytes-length vol-bytes))))))
        (bytes-append (make-bytes (block-bytes volume-header-blocks) 0)
                      (vi->bytes (make-vi (vi-first-block vol-info)
                                          (vi-last-block vol-info)
                                          (vi-file-kind vol-info)
                                          (vi-volume-name vol-info)
                                          new-eov-block
                                          (vi-number-of-files vol-info)
                                          (vi-load-time vol-info)
                                          (vi-last-boot vol-info)))
                      dir-bytes
                      (make-bytes (max (- (block-bytes (vi-last-block vol-info))
                                          (bytes-length dir-bytes)
                                          26) ; fixme: magic number
                                       0)
                                  0)
                      vol-bytes
                      (make-bytes (max (- (block-bytes (vi-eov-block vol-info))
                                          (bytes-length vol-bytes)
                                          (block-bytes (vi-last-block vol-info)))
                                       0)
                                  0)))
      (let-values (((this-image this-fi) ((car dir) 'bin-image)))
        (let ((new-block-offset (+ block-offset
                                   (- (fi-last-block this-fi)
                                      (fi-first-block this-fi)))))
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
                    (bytes-append vol-bytes this-image))))))
  (vol-iter dir (vi-last-block vol-info) #"" #""))
 
(define (make-file in-port block-offset file-info container-file)
  (lambda (op . arg)
    (cond ((eq? op 'list)
           (list (make-fi
                  (+ (fi-first-block file-info) block-offset)
                  (+ (fi-last-block file-info) block-offset)
                  (fi-file-kind file-info)
                  (fi-file-name file-info)
                  (fi-last-byte file-info)
                  (fi-last-access file-info))
                 '()))
          ; the two used forms are meaningless for files, we just provide a
          ; consistant interface for volume and file objects
          ((eq? op 'outer-used) (file-blocks-used file-info))
          ((eq? op 'inner-used) (file-blocks-used file-info))
          ((eq? op 'outer-free) #f)
          ((eq? op 'inner-free) #f)
          ((eq? op 'bin-image)
           (let ((in-port (container-file 'open-input)))
             (begin0
               (values
                 (file->bytes in-port block-offset file-info)
                 file-info)
               (close-input-port in-port))))
          ((eq? op 'read)
           (if (and (= (length arg) 1)
                    (string=? (car arg) (fi-file-name file-info)))
               (if (eq? (fi-file-kind file-info) 'Text)
		   (let ((in-port (container-file 'open-input)))
		     (begin0
                       (file->text in-port block-offset file-info)
		       (close-input-port in-port)))
                   (raise-user-error 'file-obj 
                                     "Unable to read ~a file kind."
                                     (fi-file-kind file-info)))
                   #f)))))

(define (file->bytes in-port block-offset file-info)
  (file-position in-port (block-bytes (+ block-offset
                                         (fi-first-block file-info))))
  (read-bytes (block-bytes (- (fi-last-block file-info)
                              (fi-first-block file-info)))
              in-port))
             
(define (file->text in-port block-offset file-info)
  (define (dle-indent n) (make-bytes (- n space) space))
  (file-position in-port (block-bytes (+ block-offset
					 (fi-first-block file-info)
					 text-file-header-blocks)))
  (let* ((file-length (+ (block-bytes (- (fi-last-block file-info)
					 (fi-first-block file-info)
					 text-file-header-blocks
					 1))
			 (fi-last-byte file-info)))
	 (file-bytes (read-bytes file-length in-port)))
    ; maybe best off replacing nulls from the get go, then deal with each
    ; line one cr at a time (ie do the indents, then append the rest up to
    ; the next cr
    ; would it be better to convert to list?
    (let convert-bytes ((pos 0) (start-of-line #t) (text #""))
      ;(display text)
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
                                                (dle-indent (bytes-ref file-bytes (+ pos 1))))))
                  (else (convert-bytes (+ pos 1) #f (bytes-append text (bytes pos-byte))))))))))
 
    
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
        (bytes-append file-text (bytes 0))
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
                                     (bytes-append file-text (bytes pos-byte)))))))))





; structure of listing return :

; '(file-info-object-for-vol (... list for things contained in vol ...))
; file '(file-info ())
; svol '(file-info (... list for things contained in vol ...))

; eg (source-vol ( ... ))
; ((file1.text ()) (file2.text ()) (123.svol ((file3.text ()) (file4.text ()))))

