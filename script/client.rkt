#lang racket/base

(require racket/file
         racket/os
         racket/format
         racket/list
	 openssl/mzssl
         (prefix-in r: file/md5)
         "this-collection.rkt")

(provide handin-connect
         handin-disconnect
         submit-assignment)

(define-struct handin (r w))

;; errors to the user: no need for a "foo: " prefix
(define (error* fmt . args)
  (error (apply format fmt args)))

(define (write+flush port . xs)
  (for ([x (in-list xs)]) (write x port) (newline port))
  (flush-output port))

(define (close-handin-ports h)
  (close-input-port (handin-r h))
  (close-output-port (handin-w h)))

(define (wait-for-ok r who . reader)
  (let ([v (if (pair? reader) ((car reader)) (read r))])
    (unless (eq? v 'ok) (error* "~a error: ~a" who v))))  ;!!!! this is where "login error:" comes from

;; ssl connection, makes a readable error message if no connection
(define (connect-to server port)
  (define pem (in-this-collection "server-cert.pem"))
  (define ctx (ssl-make-client-context))
  (ssl-set-verify! ctx #t)
  (ssl-load-default-verify-sources! ctx)
  (ssl-load-verify-root-certificates! ctx pem)
  (with-handlers
      ([exn:fail:network?
        (lambda (e)
          (let* ([msg
                  "handin-connect: could not connect to the server (~a:~a)"]
                 [msg (format msg server port)]
                 #; ; un-comment to get the full message too
                 [msg (string-append msg " (" (exn->string e) ")")])
            (raise (make-exn:fail:network msg (exn-continuation-marks e)))))])
    (ssl-connect server port ctx)))

(define (handin-connect server port)
  (let-values ([(r w) (connect-to server port)])
    (write+flush w 'handin)
    ;; Sanity check: server sends "handin", first:
    (let ([s (read-bytes 6 r)])
      (unless (equal? #"handin" s)
        (error 'handin-connect "bad handshake from server: ~e" s)))
    ;; Tell server protocol = 'ver1:
    (write+flush w 'ver3)
    ;; One more sanity check: server recognizes protocol:
    (let ([s (read r)])
      (unless (eq? s 'ver3)
        (error 'handin-connect "bad protocol from server: ~e" s)))
    ;; Return connection:
    (make-handin r w)))

(define (handin-disconnect h)
  (write+flush (handin-w h) 'bye)
  (close-handin-ports h))

(define (submit-assignment h username passwd assignment cwls content
                           on-commit message message-final message-box)
  (let ([r (handin-r h)] [w (handin-w h)])
    (define (read/message)
      (let ([v (read r)])
        (case v
          [(message) (message (read r)) (read/message)]
          [(message-final) (message-final (read r)) (read/message)]
          [(message-box)
           (write+flush w (message-box (read r) (read r))) (read/message)]
          [else v])))
    (write+flush w
      'set 'fingerprint (fingerprint)
      'set 'username   username
      'set 'password   passwd
      'set 'cwls       cwls
      'set 'assignment assignment
      'save-submission)
    (wait-for-ok r "login") ;!!!because this isn't calling read/message server can't send 'message-final
    ;;                      ;!!!so if anything other than 'ok comes back wait-for-ok calls error*
    ;;                      ;!!! we are inside submit-assignment, called by submit-file in client-gui
    (write+flush w (bytes-length content))
    (let ([v (read r)])
      (unless (eq? v 'go) (error* "upload error: ~a" v)))
    (display "$" w)
    (display content w)
    (flush-output w)
    ;; during processing, we're waiting for 'confirm, in the meanwhile, we
    ;; can get a 'message or 'message-box to show -- after 'message we expect
    ;; a string to show using the `messenge' argument, and after 'message-box
    ;; we expect a string and a style-list to be used with `message-box' and
    ;; the resulting value written back
    (let ([v (read/message)])
      (unless (eq? 'confirm v) (error* "submit error: ~a" v)))
    (on-commit)
    (write+flush w 'check)
    (wait-for-ok r "commit" read/message)
    (close-handin-ports h)))

(define FINGERPRINT-LENGTH 10)

(define (fingerprint)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (map md5
         (take-fpl
          (list* (path->string
                  (let-values ([(foo end bar) (split-path (find-system-path 'home-dir))])
                    end))
                 (gethostname)
                 (system-type)
                 (environment-variables-names (current-environment-variables)))))))

;; ordinary take errs if lst is too short
(define (take-fpl lst [len FINGERPRINT-LENGTH])
  (cond [(or (empty? lst) (zero? len)) '()]
        [else
         (cons (car lst)
               (take-fpl (cdr lst) (sub1 len)))]))

(define (md5 s)
  (bytes->string/latin-1 (r:md5 (string->bytes/utf-8 (~a s)))))