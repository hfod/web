#lang racket

(provide dispatch
         handle-error-not-found
         handle-error-crash)

(require (prefix-in  srv:  web-server/servlet)
         (prefix-in  http: web-server/http)
         (prefix-in  dis:  web-server/dispatch))

(define handler?
  (-> srv:request? srv:response?))

(define-values (dispatch _handler->url)
  (dis:dispatch-rules
    [("test") handle-test]))

;; TODO handle-subscribe-to-announce

(define/contract (handle-test req)
  handler?
  (http:response/output (λ (op) (displayln "works" op))))

(define/contract (handle-error-not-found req)
  handler?
  (srv:response/xexpr
    '(html (head (title "error 404 : not found"))
      (body "not found"))
    #:code 404))

(define/contract (handle-error-crash url exn)
  (-> any/c exn? srv:response?)
  (srv:response/xexpr
    '(html (head (title "error 500 : internal server error"))
      (body "internal server error"))
    #:code 500))
