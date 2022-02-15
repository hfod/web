#! /usr/bin/env racket

#lang racket

(require (prefix-in xml: xml))

(require (prefix-in view: "view.rkt"))

(define/contract (main out-dir)
  (-> path-string? void)
  (for-each
    (match-lambda
      [(cons path xexpr)
       (display-to-file (xml:xexpr->string xexpr)
                        (build-path out-dir path)
                        #:exists 'replace)])
    (view:pages)))

(module+ main
  (let ([out-dir "./dist"])
    ; TODO Add option to generate indented output, for debugging diffs.
    (command-line
      #:program (find-system-path 'run-file)
      #:once-each
      [("-o" "--out-dir")
       path "Directory to which to write page files. DEFAULT: ./dist"
       (invariant-assertion path-string? path)
       (set! out-dir (normalize-path path))])
    (main out-dir)))
