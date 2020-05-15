#lang racket/base

; this is a patched version of net/rfc6455's server.rkt
; to correctly handle keyword arguments in ws-serve

;; Convenience interface for starting a simple web service.
;; Roughly compatible with net/websocket/server's ws-serve.

;; Copyright (c) 2013 Tony Garnock-Jones
;;
;; This module is distributed under the GNU Lesser General Public
;; License (LGPL). This means that you can link it into proprietary
;; applications, provided you follow the rules stated in the LGPL. You
;; can also modify this module; if you distribute a modified version,
;; you must distribute it under the terms of the LGPL, which in
;; particular means that you must release the source code for the
;; modified software. See http://www.gnu.org/licenses/lgpl-3.0.txt for
;; more information.

(require racket/match)
(require web-server/web-server)
(require web-server/http/request-structs)
(require web-server/http/response)
(require web-server/http/response-structs)
(require web-server/dispatchers/dispatch)
(require net/url)
(require net/rfc6455/dispatcher)
(require net/rfc6455/service-mapper)
(require net/rfc6455/conn-api)
(require (except-in net/rfc6455 ws-serve))

(provide ws-serve (all-from-out net/rfc6455))

(define (transpose xss) (apply map list xss))

(define (guard-dispatcher d)
  (lambda (conn req)
    (with-handlers [(exn:dispatcher?
		     (lambda (e)
		       (log-info "Bad WS request, ~a ~a"
				 (request-method req)
				 (url->string (request-uri req)))
		       (output-response/method
			conn
			(response 400 #"Bad WebSocket request" (current-seconds) #f '() void)
			(request-method req))))]
      (d conn req))))

(define ws-serve
  (procedure-rename
   (make-keyword-procedure
    (lambda (keys vals conn-dispatch . rest)
      (define kvs (map list keys vals))
      (define conn-headers-cell (assq '#:conn-headers kvs))
      (define conn-headers (and conn-headers-cell (cadr conn-headers-cell)))
      (define dispatcher (make-general-websockets-dispatcher conn-dispatch conn-headers))
      (match-define (list keys1 vals1) (transpose (remq conn-headers-cell kvs)))
      (keyword-apply serve
		     keys1
		     vals1
		     rest
                     #:dispatch (guard-dispatcher dispatcher))))
   'ws-serve))
