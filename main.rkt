#! /usr/bin/env racket

#lang racket

(require (prefix-in xml: xml))

(define/contract bootstrap-css path-string? "_lib/bs/css/bootstrap.min.css")
(define/contract bootstrap-js  path-string? "_lib/bs/js/bootstrap.bundle.min.js")
(define/contract local-css     path-string? "_lib/style.css")

(define/contract enable-tooltips
  xml:xexpr/c
  `(script ,(file->string "snip/bs-enable-tooltips.js")))

(define/contract (page main-content)
  (-> (listof xml:xexpr/c) xml:xexpr/c)
  `(html (head (title "Hack Free Or Die")
               (meta ([charset "utf-8"]))
               (meta ([name="viewport"]
                      [content "width=device-width, initial-scale=1"]))
               (meta ([name "description"]
                      [content ""]))
               (script ([src ,bootstrap-js]))
               (link ([href ,bootstrap-css]
                      [rel "stylesheet"]))
               (link ([href ,local-css]
                      [rel "stylesheet"])))
    (body ([class "d-flex h-100 text-center text-white bg-dark"])
          (div ([class "hfod-container d-flex w-100 h-100 p-3 mx-auto flex-column"])
               (header ([class "mb-auto"]))
               (main ([class "px-3"])
                     ,@main-content)
               (footer ([class "mt-auto text-white-50"])
                       (p "Inspired by "
                          (a ([href "https://hackandtell.org/"]
                              [class "text-white"])
                             "NYC Hack && Tell")))))))

(define/contract (page-home)
  (-> xml:xexpr/c)
  (define main-content
    `((h1 "Hack Free Or Die")
      (p ([class "lead"])
         "A show and tell for and by "
         (a ([href "https://datatracker.ietf.org/doc/html/rfc1392#page-21"]
             [data-bs-toggle "tooltip"]
             [title "A person who delights in having an intimate understanding of the internal workings of a system, computers and computer networks in particular."])
            "hacker")
         "s in New Hampshire.")
      (p ([class "lead"])
         "We each take 5-minute turns to present something we hacked together (or apart) and then another 5 minutes to answer questions.")
      (p ([class "lead"])
         "The only qualification is that the work you present has to be open source - so that we can all study and learn from it.")
      (p ([class "lead"])
         "After the presentations we hangout and discuss whatever.")
      (h3 "Next meeting")
      (p ([class "lead"])
         "Thursday, March 10th, 2022 at 18:00 in Manchester.")
      (p ([class "lead"])
         "Graciously hosted by "
         (a ([href "https://www.ravenlabsnh.com/"])
            "Raven Labs")
         ".")
      (p ([class "lead"])
         (a ([class "btn btn-lg btn-secondary fw-bold border-white bg-white"]
             [href ,(file->string "snip/join-us-button-mailto.txt")])
            "Join us"))
      ,enable-tooltips))
  (page main-content))

(define/contract (pages)
  (-> (listof (cons/c path-string? xml:xexpr/c)))
  `(("index.html" . ,(page-home))))

(define/contract (main out-dir)
  (-> path-string? void)
  (for-each
    (match-lambda
      [(cons path xexpr)
       (display-to-file (xml:xexpr->string xexpr)
                        (build-path out-dir path)
                        #:exists 'replace)])
    (pages)))

(module+ main
  (let ([out-dir "./dist"])
    (command-line
      #:program (find-system-path 'run-file)
      #:once-each
      [("-o" "--out-dir")
       path "Directory to which to write page files. DEFAULT: ./dist"
       (invariant-assertion path-string? path)
       (set! out-dir (normalize-path path))])
    (main out-dir)))
