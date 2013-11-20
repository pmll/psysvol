#lang racket

; ease an overwhelming fear of corruption

(provide (contract-out (ripple (-> string? void?))))

; ripple backup files, thus
; my.file      becomes my.file.bak1
; my.file.bak1 becomes my.file.bak2
; my.file.bak2 becomes my.file.bak3...

(define (bak-extension path-str bak-number)
  (string-append path-str
                 (if (zero? bak-number)
                     ""
                     (string-append ".bak" (number->string bak-number)))))

(define (last-bak path-str)
  (let loop ((bak-number 1))
    (if (file-exists? (bak-extension path-str bak-number))
        (loop (+ bak-number 1))
        (- bak-number 1))))

(define (ripple path-str)
  (let rip ((bak-number (last-bak path-str)))
    (rename-file-or-directory (bak-extension path-str bak-number)
                              (bak-extension path-str (+ bak-number 1)))
    (when (> bak-number 0) (rip (- bak-number 1)))))

