#! /usr/bin/env racket

#lang racket

(require (prefix-in view: "view.rkt"))

(define/contract (write-files files out-dir)
  (-> (listof view:File?) path-string? void)
  (for-each
    (λ (f)
       (display-to-file (view:File-content f)
                        (build-path out-dir (view:File-name f))
                        #:exists 'replace))
    files))

(define/contract (main out-dir)
  (-> path-string? void)
  (write-files (view:html-files) out-dir))

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
