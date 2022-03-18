#lang racket

(provide reroot)

;; TODO Maybe separate into add-root and replace-root?
(define/contract (reroot root path)
  (-> path? path? path?)
  (cond
    [(relative-path? path) (build-path root path)]
    [(absolute-path? path) (apply build-path (cons root (cdr (explode-path path))))]
    [else (assert-unreachable)]))
