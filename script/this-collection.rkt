#lang racket/base

(require (for-syntax racket/base))

(define-syntax (this-name-stx stx)
  (datum->syntax stx "spd-handin" stx))
