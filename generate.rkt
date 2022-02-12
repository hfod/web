#! /usr/bin/env racket

#lang racket

(require (prefix-in xml: xml))

(require (prefix-in g: gregor)
         (prefix-in g: gregor/time))

(struct/contract Addr
                 ([building string?]
                  [street string?]
                  [room string?]
                  [town string?]
                  [state "NH"]
                  [zipcode string?]
                  [country "USA"]))

(struct/contract Host
                 ([name string?]
                  [addr Addr?]
                  [url string?]  ; TODO Use url type?
                  ))

(struct/contract Meeting
                 ([date g:date?]
                  [time g:time?]
                  [host Host?])
                 #:transparent)

(define d g:date)
(define t g:time)

(define/contract addr-raven-labs
  Addr?
  (Addr "913"
        "Elm St"
        "Suite 405"
        "Manchester"
        "NH"
        "03101"
        "USA"))

(define/contract host-raven-labs
  Host?
  (Host "Raven Labs"
        addr-raven-labs
        "https://www.ravenlabsnh.com/"))

(define/contract meetings
  (listof Meeting?)
  (sort (list (Meeting (d 2022 02 10)
                       (t 18 00)
                       host-raven-labs)
              (Meeting (d 2022 04 14)
                       (t 18 00)
                       host-raven-labs)
              (Meeting (d 2022 03 10)
                       (t 18 00)
                       host-raven-labs))
        (λ (a b) (g:date<? (Meeting-date a)
                           (Meeting-date b)))))

(define/contract next-meeting
  Meeting?
  (let* ([today (g:today)]
         [future
           (filter (λ (m) (g:date>? (Meeting-date m) today)) meetings)])
    (if (empty? future)
        #f
        (first future))))

(define/contract x-next-meeting
  xml:xexpr/c
  (match next-meeting
    [#f ""]
    [m
      (let* ([d (Meeting-date m)]
             [t (Meeting-time m)]
             [dt (g:datetime (g:->year d)
                             (g:->month d)
                             (g:->day d)
                             (g:->hours t)
                             (g:->minutes t))]
             [date (g:~t dt "EEEE, MMMM d, y")]
             [time (g:~t dt "HH:mm")]
             [h (Meeting-host m)]
             [host-town (Addr-town (Host-addr h))]
             ; TODO Link to local info page about host/location?
             [host-link `(a ([href ,(Host-url h)]) ,(Host-name h))])
        `(p ([class "lead"])
          ,date (br) ,time " at " ,host-link " in " ,host-town))]))

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
      ,x-next-meeting
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
