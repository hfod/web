#! /usr/bin/env racket

#lang racket

(require (prefix-in view: "view.rkt"))

(define/contract (write-files files out-dir)
  (-> (listof view:File?) path-string? void)
  (for-each
    (λ (f)
       (define filepath (build-path out-dir (view:File-path f)))
       (make-parent-directory* filepath)
       (display-to-file (view:File-content f)
                        filepath
                        #:exists 'replace))
    files))

(module+ main
  (let ([program (find-system-path 'run-file)]
        [out-dir (current-directory)])
    (command-line
      #:program program
      #:once-each
      [("-o" "--out-dir")
       path "Directory to which to write output files and directories. DEFAULT: $PWD"
       (invariant-assertion path-string? path)
       (set! out-dir (normalize-path path))]
      #:args (command . args)
      (current-command-line-arguments (list->vector args))
      (let ([program (string-append (path->string program) " " command)])
        (match command
          ["web"
           ; TODO Add option to generate indented output, for debugging diffs.
           (write-files (view:web-files) out-dir)]
          ["email"
           (write-files (view:email-files) out-dir)])))))
