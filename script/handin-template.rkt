#lang racket
(require "client.rkt")
(define (call-back . args) (void))
(define server   "cs110.students.cs.ubc.ca")
(define port     7979)
(define filename "${filename}")
(define cwl      (list ${cwl}))
(define id       "${id}")
(define pwd      "${pwd}")
(define asn      "${as}")
(define h (handin-connect server port))
(submit-assignment h 
                   id 
                   pwd 
                   asn 
                   cwl 
                   (file->bytes filename #:mode 'text)
                   call-back
                   call-back
                   call-back
                   call-back)