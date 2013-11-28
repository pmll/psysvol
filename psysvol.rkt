#lang racket

; todo: lock container file, do we need a rename?
;       fix incredibly inefficient text file conversions

; fixme: looks like text file lines are not allowed to span blocks...

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
(define max-filename-len 15)
(define max-volname-len 7)
; The DLE character is used for indentation in P-System text files.
; For our purposes, it stands for "Drag Line East" :-)
(define dle 16)
(define cr 13)
(define lf 10)
(define space 32)

; volume information
(define-struct vi (first-block
                   last-block
                   file-kind
                   volume-name
                   eov-block
                   number-of-files
                   load-time
                   last-boot))

; file information
(define-struct fi (first-block
                   last-block
                   file-kind
                   file-name
                   last-byte
                   last-access))

(define file-kind (vector-immutable 'None
                                    'XDsk
                                    'Code
                                    'Text
                                    'Info 
                                    'Data
                                    'Graf
                                    'Foto
                                    'Secu
                                    'Svol))

(define (raise-corruption-exception)
  (raise-user-error "Container file is corrupt or not a P-System volume.")) 

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

(define (pstring->string byte-str offset max-len)
  (let ((len (bytes-ref byte-str offset)))
    (when (or (zero? len) (> len max-len)) (raise-user-error "Bad pstring"))
    (format "~a" (subbytes byte-str (+ offset 1) (+ offset len 1)))))

; convert block count into a byte count
(define (block-bytes b) (* b block-size))

; convert byte count into a block count
(define (byte-blocks b) (quotient (+ b block-size -1) block-size))

(define (bytes-last-block b) (+ (remainder (- b 1) block-size) 1))

(define (time-stamp time-b date-b)
  (let* ((minutes   (bitwise-bit-field time-b 9 15))
         (hours     (- (bitwise-bit-field time-b 4 9) 1))
         (has-time? (and (< minutes 60) (> hours -1) (< hours 24))))
    (with-handlers
      ((exn:fail:contract? (lambda (e) ((raise-user-error "Bad date bytes")))))
      ;((exn:fail:contract? (lambda (e) ((current-date)))))
      (make-date 0
                 ; the time part is pretty non-standard, I think
                 ; where it appears not to be used, treat as midnight
                 (if has-time? minutes 0)
                 (if has-time? hours 0)
                 (bitwise-bit-field date-b 4 9)
                 (bitwise-bit-field date-b 0 4)
                 (bitwise-bit-field date-b 9 16)
                 0
                 0
                 #f
                 0))))

(define (int16->file-kind kind-int)
  (let ((kind-b (bitwise-bit-field kind-int 0 4)))
    (if (< kind-b (vector-length file-kind))
        (vector-ref file-kind kind-b)
        'Unkn)))

(define (file-kind->int16 fk) (or (vector-memq fk file-kind) 0))

(define (fbytes->vi in-port start)
  (file-position in-port start)
  (let ((byte-str (read-bytes 22 in-port)))
    (with-handlers
      ((exn:fail:user? (lambda (e) (raise-corruption-exception))))
      (make-vi (bytes->int16 byte-str 0)
               (bytes->int16 byte-str 2)
               (bytes->int16 byte-str 4)
               (pstring->string byte-str 6 7)
               (bytes->int16 byte-str 14)
               (bytes->int16 byte-str 16)
               (bytes->int16 byte-str 18)
               (bytes->int16 byte-str 20)))))

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

(define (fbytes->fi in-port start)
  (file-position in-port start)
  (let ((byte-str (read-bytes dir-entry-size in-port)))
    (with-handlers
      ((exn:fail:user? (lambda (e) (raise-corruption-exception))))
      (make-fi (bytes->int16 byte-str 0)
               (bytes->int16 byte-str 2)
               (int16->file-kind (bytes->int16 byte-str 4))
               (pstring->string byte-str 6 15)
               (bytes->int16 byte-str 22)
               (time-stamp (bytes->int16 byte-str 4)
                           (bytes->int16 byte-str 24))))))

(define (fi->bytes file-info)
  (let ((last-access (fi-last-access file-info)))
    (bytes-append (int16->bytes (fi-first-block file-info))
                  (int16->bytes (fi-last-block file-info))
                  (int16->bytes
                    (bitwise-ior 
                      (file-kind->int16 (fi-file-kind file-info))
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

(define (fi-blocks-used file-info)
  (- (fi-last-block file-info) (fi-first-block file-info)))

(define (last? lst) (and (not (null? lst)) (null? (cdr lst))))

; really need to look into file locking possibilities...
(define (register-container-file container-file)
  (if (file-exists? container-file)
      (let ((file-modified (file-or-directory-modify-seconds container-file)))
        (lambda (op)
          (cond ((not (file-exists? container-file))
                 (raise-user-error (format "File ~a has been removed!"
                                           container-file)))
                ((not (= (file-or-directory-modify-seconds container-file)
                         file-modified))
                 (raise-user-error (format "File ~a has been modified."
                                           container-file)))
                ((eq? op 'open-input)
                 (open-input-file container-file #:mode 'binary))
                ((eq? op 'file-info)
                 (make-fi 0
                          (byte-blocks (file-size container-file))
                          'Vol
                          container-file
                          (bytes-last-block (file-size container-file))
                          (seconds->date file-modified)))
                (else (raise-user-error 'register-container-file
                                               "Invalid operation ~a."
                                               op)))))
      (raise-user-error "File not found: ~a." container-file)))

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
      (when (not (valid-volume? vol-info dir)) (raise-corruption-exception))
      (lambda (op . arg)
        (cond
          ((eq? op 'list)
           (list (make-fi block-offset
                          (+ block-offset (vi-eov-block vol-info))
                          (fi-file-kind file-info)
                          (vi-volume-name vol-info)
                          block-size
                          (fi-last-access file-info))
                 (filter-map (lambda (f) (f 'list)) dir)))
          ((eq? op 'vol-name) (vi-volume-name vol-info))
          ((eq? op 'outer-used) (fi-blocks-used file-info))
          ((eq? op 'inner-used)
           (foldl + (vi-last-block vol-info) (map (lambda (f) (f op)) dir)))
          ((eq? op 'outer-free)
           (if (string=? (car arg) (vi-volume-name vol-info))
               (if (last? arg)
                   (- (vi-eov-block vol-info)
                      (foldl + 
                             (vi-last-block vol-info)
                             (map (lambda (f) (f 'outer-used)) dir)))
                   (ormap (lambda (f) (apply f op (cdr arg))) dir))
               #f))
          ((eq? op 'inner-free)
           (if (string=? (car arg) (vi-volume-name vol-info))
               (if (last? arg)
                   (- (vi-eov-block vol-info)
                      (foldl + 
                             (vi-last-block vol-info)
                             (map (lambda (f) (f 'inner-used)) dir)))
                   (ormap (lambda (f) (apply f op (cdr arg))) dir))
               #f))
          ((eq? op 'bin-image)
           ; bin-image is just proof of concept, it won't be staying...
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
          ((eq? op 'crunch)
           (let ((vol-image (vol->bytes vol-info dir op)))
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
          ((update-operation? op)
           (if (string=? (car arg) (vi-volume-name vol-info))
               ; it's on this path, we have to descend it
               (if (and (eq? op 'delete-vol) (last? arg))
                   (if (eq? (fi-file-kind file-info) 'Vol)
                       (raise-user-error "Cannot delete top level volume")
                       (values #f #f))
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
                                          (fi-last-access file-info))))))
               ; it's not on this path,
               ; we only need to take the entire svol image as is
               (let ((in-port (container-file 'open-input)))
                 (begin0
                   (values (file->bytes in-port block-offset file-info)
                           file-info)
                   (close-input-port in-port)))))
          ((eq? op 'file-exists?)
           (and (> (length arg) 1)
                (string=? (car arg) (vi-volume-name vol-info))
                (ormap (lambda (f) (apply f op (cdr arg))) dir)))
          ((eq? op 'vol-exists?)
           (and (not (null? arg))
                (string=? (car arg) (vi-volume-name vol-info))
                (or (last? arg)
                    (ormap (lambda (f) (apply f op (cdr arg))) dir))))
          ((eq? op 'vol-empty?)
           (and (not (null? arg))
                (string=? (car arg) (vi-volume-name vol-info))
                (or (and (last? arg) (zero? (vi-number-of-files vol-info)))
                    (ormap (lambda (f) (apply f op (cdr arg))) dir))))
          ((or (eq? op 'read) (eq? op 'dump))
           (if (and (> (length arg) 1)
                    (string=? (car arg) (vi-volume-name vol-info)))
               ; using ormap we get the converted text or #f
               (ormap (lambda (f) (apply f op (cdr arg))) dir)
               #f))
          ((eq? op 'file-info) file-info)
          (else (raise-user-error 'make-volume
                                  "Unknown operation: ~a." 
                                  op)))))))

(define (make-file block-offset file-info container-file)
  (lambda (op . arg)
    (cond
      ((eq? op 'list)
       (list (make-fi (+ (fi-first-block file-info) block-offset)
                      (+ (fi-last-block file-info) block-offset)
                      (fi-file-kind file-info)
                      (fi-file-name file-info)
                      (fi-last-byte file-info)
                      (fi-last-access file-info))
            '()))
      ; the two used forms are meaningless for files, we just
      ; provide a consistant interface for volume and file objects
      ((eq? op 'outer-used) (fi-blocks-used file-info))
      ((eq? op 'inner-used) (fi-blocks-used file-info))
      ((eq? op 'bin-image)
       (let ((in-port (container-file 'open-input)))
         (begin0
           (values (file->bytes in-port
                                (+ block-offset (fi-first-block file-info))
                                file-info)
                   file-info)
           (close-input-port in-port))))
      ((eq? op 'crunch)
       (let ((in-port (container-file 'open-input)))
         (begin0
           (values (file->bytes in-port
                                (+ block-offset (fi-first-block file-info))
                                file-info)
                   file-info)
           (close-input-port in-port))))
      ((update-operation? op)
       (if (and (file-update? op) (string=? (car arg) (fi-file-name file-info)))
           ; this is the file we are working on
           (cond 
             ((eq? op 'delete) (values #f #f))
             ((eq? (fi-file-kind file-info) 'Text)
              (let* ((new-image (text->file (cadr arg)))
                     (image-length (bytes-length new-image))
                     (last-bytes (bytes-last-block image-length)))
                (values (bytes-append new-image
                                      (make-bytes (- block-size last-bytes) 0))
                        (make-fi (fi-first-block file-info)
                                 (+ (byte-blocks image-length)
                                    (fi-first-block file-info))
                                 (fi-file-kind file-info)
                                 (fi-file-name file-info)
                                 last-bytes
                                 (current-date)))))
             (else (raise-user-error (format "Unable to write to file kind ~a."
                                             (fi-file-kind file-info)))))
           ; it's not this file, just produce a bin-image of it
           (let ((in-port (container-file 'open-input)))
             (begin0
               (values (file->bytes in-port
                                    (+ block-offset (fi-first-block file-info))
                                    file-info)
                       file-info)
               (close-input-port in-port)))))
      ((eq? op 'file-exists?)
       (and (last? arg)
            (string=? (car arg) (fi-file-name file-info))))
      ((eq? op 'read)
       (if (and (last? arg)
                (string=? (car arg) (fi-file-name file-info)))
           (if (eq? (fi-file-kind file-info) 'Text)
               (let ((in-port (container-file 'open-input)))
                 (begin0
                   (file->text in-port block-offset file-info)
                   (close-input-port in-port)))
               (raise-user-error (format "Unable to read file kind ~a."
                                         (fi-file-kind file-info))))
           #f))
      ((eq? op 'file-info) file-info)
      ((eq? op 'dump)
       (if (and (last? arg) (string=? (car arg) (fi-file-name file-info)))
           (let ((in-port (container-file 'open-input)))
             (begin0
               (if (eq? (fi-file-kind file-info) 'Text)
                   (file->text in-port block-offset file-info)
                   (file->hd in-port block-offset file-info))
               (close-input-port in-port)))
           #f))
      (else #f))))

; place holder for new file system objects
(define (make-new-object)
  (lambda (op . arg)
    (cond ((eq? op 'outer-used) 0)
          ((eq? op 'inner-used) 0)
          ((eq? op 'write) (values #f #f))
          ((eq? op 'delete) (values #f #f))
          ((eq? op 'delete-vol) (values #f #f))
          ((eq? op 'crunch) (values #f #f))
          ((eq? op 'file-info) (make-fi 0 0 'Spare "" 0 0))
          ((eq? op 'create-file)
           (if (= (length arg) 2)
             (if (<= (string-length (car arg)) max-filename-len)
                 (let ((new-image (text->file (cadr arg))))
                   (values (bytes-append new-image
                                         (make-bytes (- block-size
                                                        (bytes-last-block (bytes-length new-image)))
                                                     0))
                           (make-fi 0
                                    (byte-blocks (bytes-length new-image))
                                    'Text
                                    (car arg)
                                    (bytes-last-block (bytes-length new-image))
                                    (current-date))))
                 (raise-user-error (format "Filename ~a is too long." (car arg))))
             (values #f #f)))
          ((eq? op 'create-vol)
           (if (last? arg)
               (if (<= (string-length (car arg)) max-volname-len)
                   (values (bytes-append
                            (make-bytes (block-bytes volume-header-blocks) 0)
                            (vi->bytes (make-vi 0 6 0 (car arg) 6 0 0 0))
                            (make-bytes (- (block-bytes (- 6 volume-header-blocks))
                                           dir-entry-size)
                                        0))
                           (make-fi 0
                                    6
                                    'Svol
                                    (string-append (car arg) ".SVOL")
                                    block-size
                                    (current-date)))
                   (raise-user-error (format "Volume name ~a is too long." (car arg))))
               (values #f #f)))
          (else #f))))

(define (valid-volume? vol-info dir)
  (define (overlap? dir-entry1 dir-entry2)
    (let* ((file-info1 (dir-entry1 'file-info))
           (file-info2 (dir-entry2 'file-info))
           (first-block1 (fi-first-block file-info1))
           (last-block1 (fi-last-block file-info1))
           (first-block2 (fi-first-block file-info2))
           (last-block2 (fi-last-block file-info2)))
      (or (and (>= first-block1 first-block2) (< first-block1 last-block2))
          (and (>= first-block2 first-block1) (< first-block2 last-block1)))))
  (define (head-dir-overlaps-tail? dir)
    (if (< (length dir) 2)
        #f
        (ormap (lambda (x) (overlap? (car dir) x)) (cdr dir))))
  (define (dir-overlap? dir)
    (if (null? dir)
        #f
        (or (head-dir-overlaps-tail? dir) (dir-overlap? (cdr dir)))))
  (define (valid-dir-entry? dir-entry)
    (let ((file-info (dir-entry 'file-info)))
      (or (eq? (fi-file-kind file-info) 'Spare)
          (and (>= (fi-first-block file-info) (vi-last-block vol-info))
               (<= (fi-last-block file-info) (vi-eov-block vol-info))
               (>= (fi-last-block file-info) (fi-first-block file-info))
               ;(> (fi-last-byte file-info) 0)
               (<= (fi-last-byte file-info) block-size)))))
  (and (zero? (vi-first-block vol-info)) 
       (> (vi-last-block vol-info) volume-header-blocks)
       (<= (+ (byte-blocks (* (vi-number-of-files vol-info) dir-entry-size))
              volume-header-blocks)
           (vi-last-block vol-info))
       (andmap valid-dir-entry? dir)
       (not (dir-overlap? dir))))

(define (update-operation? op)
  (memq op (list 'write 'create-file 'create-vol 'delete 'delete-vol)))

(define (file-update? op) (or (eq? op 'write) (eq? op 'delete)))

(define (vol->bytes vol-info dir op . arg)
  (define (vol-iter dir block-offset dir-bytes vol-bytes cnt)
    (if (null? dir)
      (let* ((eov-for-image (+ (vi-last-block vol-info)
                               (byte-blocks (bytes-length vol-bytes))))
             (new-eov-block (if (eq? op 'crunch)
                                eov-for-image
                                (max (vi-eov-block vol-info) eov-for-image))))
        (when (> new-eov-block max-vol-blocks)
          (raise-user-error "Volume too big"))
        ; fixme: we need to ensure the file count does not exceed the space
        ;        reserved for dir entries
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
                      ; max shouldn't be needed here?
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
 
(define (file->bytes in-port start-block file-info)
  (file-position in-port (block-bytes start-block))
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

(define (file->hd in-port block-offset file-info)
  (define (integer->formatted-hex n width)  
    (define (prepend-zeroes hex-str)
      (let ((len (string-length hex-str)))
        (if (>= len width)
            hex-str
            (string-append (make-string (- width len) #\0) hex-str))))
    (prepend-zeroes (format "~x" n)))
  (define (format-line address b)
    (apply string-append
           (format "~a : " (integer->formatted-hex address 6))
           (append (map (lambda (x) (format "~a " (integer->formatted-hex x 2))) b)
                   (list "\n"))))
  (file-position in-port (block-bytes (+ block-offset
                                         (fi-first-block file-info))))
  (let loop ((address 0) (hd ""))
    (if (< address (block-bytes (fi-blocks-used file-info)))
        (loop (+ address 16)
              (string-append hd
                             (format-line address
                                          (bytes->list (read-bytes 16 in-port)))))
        hd
)))
    
(define (text->file byte-str)
  (define text-block-size 1024)
  (let ((byte-len (bytes-length byte-str)))
    (define (crlf? pos)
      (and (< pos (- byte-len 1))
           (>= pos 0)
           (= (bytes-ref byte-str pos) cr)
           (= (bytes-ref byte-str (+ pos 1)) lf)))
    (define (eol pos)
      (cond ((= pos (- byte-len 1)) pos)
            ((crlf? pos) (+ pos 1))
            ((= (bytes-ref byte-str pos) cr) pos)
            ((= (bytes-ref byte-str pos) lf) pos)
            (else (eol (+ pos 1)))))
    (define (number-of-spaces pos)
      (if (and (< pos byte-len) (= (bytes-ref byte-str pos) space))
        (+ 1 (number-of-spaces (+ pos 1)))
        0))
    (define (take-line pos)
      (let ((end-pos (eol pos))
            (indent  (min (number-of-spaces pos) 223)))
        (values (bytes-append (if (> indent 2)
                                  (bytes dle (+ indent space))
                                  (make-bytes indent space))
                              (subbytes byte-str
                                        (+ pos indent)
                                        (if (crlf? (- pos 1))
                                            (- end-pos 1)
                                            end-pos))
                              (bytes cr))
                (+ end-pos 1))))
    (define (append-line body line)
      (let ((block-bytes-remaining (- text-block-size
                                      (remainder (bytes-length body) 
                                                 text-block-size)))
            (line-length (bytes-length line)))
        (cond ((>= line-length text-block-size) ; all bets are off, split line
               (append-line (bytes-append body 
                                          (make-bytes block-bytes-remaining 0)
                                          (subbytes line 0 (- text-block-size 2))
                                          (bytes cr 0))
                            (subbytes line (- text-block-size 2))))
              ((> line-length (- block-bytes-remaining 1))
               (bytes-append body (make-bytes block-bytes-remaining 0) line))
              (else (bytes-append body line)))))
    (let convert-bytes ((pos 0) (file-text #""))
      (if (< pos byte-len)
          (let-values (((line-bytes next-pos) (take-line pos)))
            (convert-bytes next-pos (append-line file-text line-bytes)))
          (bytes-append (make-bytes (block-bytes text-file-header-blocks) 0)
                        file-text
                        (bytes 0))))))



; we use current-input-port & eof-object? eventually...
;(define (text->file byte-str)
;  (define (number-of-spaces pos)
;    (if (and (< pos (bytes-length byte-str)) (= (bytes-ref byte-str pos) space))
;        (+ 1 (number-of-spaces (+ pos 1)))
;        0))
;  (define (crlf? pos)
;    (and (< pos (- (bytes-length byte-str) 1))
;         (= (bytes-ref byte-str pos) cr)
;         (= (bytes-ref byte-str (+ pos 1)) lf)))
;  (let convert-bytes ((pos 0) (start-of-line #t) (file-text #""))
;    (if (= pos (bytes-length byte-str))
;        (bytes-append (make-bytes (block-bytes text-file-header-blocks) 0)
;                      file-text 
;                      (bytes 0))
;        (let ((pos-byte (bytes-ref byte-str pos)))
;          (cond (start-of-line
;                 (let ((indent (min (number-of-spaces pos) 223)))
;                   (if (> indent 2)
;                       (convert-bytes (+ pos indent)
;                                      #f
;                                      (bytes-append file-text
;                                                    (bytes dle (+ space 
;                                                                  indent))))
;                       (convert-bytes pos #f file-text))))
;                ((crlf? pos)
;                 (convert-bytes (+ pos 2)
;                                #t
;                                (bytes-append file-text (bytes cr))))
;                ((or (= pos-byte cr) (= pos-byte lf))
;                 (convert-bytes (+ pos 1)
;                                #t
;                                (bytes-append file-text (bytes cr))))
;                (else (convert-bytes (+ pos 1)
;                                     #f
;                                     (bytes-append file-text
;                                                   (bytes pos-byte)))))))))





; structure of listing return :

; '(file-info-object-for-vol (... list for things contained in vol ...))
; file '(file-info ())
; svol '(file-info (... list for things contained in vol ...))

; eg (source-vol ( ... ))
; ((file1.text ()) (file2.text ()) (123.svol ((file3.text ()) (file4.text ()))))

