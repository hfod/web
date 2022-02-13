#! /usr/bin/env racket

#lang racket

(require (prefix-in url: net/url)
         (prefix-in xml: xml))

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
                  [url url:url?]
                  ))

(struct/contract Presenter
                 ([name string?]
                  [email string?]
                  [home (or/c #f url:url?)]
                  [links (listof url:url?)]))

(struct/contract Talk
                 ([presenter Presenter?]
                  [title string?]
                  [description string]
                  [source url:url?]
                  [home (or/c #f url:url?)]
                  [references (listof (cons/c string? string?))]
                  ))

(struct/contract Meeting
                 ; TODO notes?
                 ; TODO description?
                 ; TODO recap?
                 ([seq nonnegative-integer?]
                  [codename string?]
                  [date g:date?]
                  [time g:time?]
                  [host Host?]
                  [talks (listof Talk?)])
                 #:transparent)

(define (M #:seq seq
           #:codename codename
           #:date date
           #:time time
           #:host host
           #:talks talks)
  (Meeting seq codename date time host talks))

(define host-raven-labs
  (Host "Raven Labs"
        (Addr "913"
              "Elm St"
              "Suite 405"
              "Manchester"
              "NH"
              "03101"
              "USA")
        (url:string->url "https://www.ravenlabsnh.com")))

(define host-manch-maker-space
  (Host "Manchester Makerspace"
        (Addr "36"
              "Old Granite St"
              ""
              "Manchester"
              "NH"
              "0301"
              "USA")
        (url:string->url "https://manchestermakerspace.org")))

(define/contract meetings
  (listof Meeting?)
  (let ([d g:date]
        [t g:time])
    (list (M #:seq 0
             #:codename "Ground Zero"
             #:date (d 2022 01 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:talks '())

          (M #:seq 1
             #:codename "Genesis"
             #:date (d 2022 02 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:talks '())  ; TODO Add talks.

          (M #:seq 2
             #:codename "TBD"
             #:date (d 2022 03 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:talks '())
          )))

(define/contract (meetings-filter-by-date compares?)
  (-> (-> g:date? g:date? boolean?) (listof Meeting?))
  (let ([today (g:today)])
    (filter (λ (m) (compares? (Meeting-date m)
                              today))
            meetings)))

(define/contract meetings-past
  (listof Meeting?)
  (meetings-filter-by-date g:date<?))

(define/contract meetings-future
  (listof Meeting?)
  (meetings-filter-by-date g:date>?))

(define/contract next-meeting
  Meeting?
  (match meetings-future
    ['() #f]
    [ms
      (first (sort ms (λ (m1 m2) (g:date<? (Meeting-date m1)
                                           (Meeting-date m2)))))]))

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
             [host-link `(a ([href ,(url:url->string (Host-url h))]) ,(Host-name h))])
        `(p ([class "lead"])
          ; TODO Google maps link
          ,date (br) ,time " at " ,host-link " in " ,host-town))]))

(define/contract bootstrap-css path-string? "_lib/bs/css/bootstrap.min.css")
(define/contract bootstrap-js  path-string? "_lib/bs/js/bootstrap.bundle.min.js")
(define/contract local-css     path-string? "_lib/style.css")

(define/contract (include file)
  (-> path-string? string?)
  (file->string (build-path "inc" file)))

(define/contract enable-tooltips
  xml:xexpr/c
  `(script ,(include "bs-enable-tooltips.js")))

(define/contract (page title content)
  (-> string? (listof xml:xexpr/c) xml:xexpr/c)
  (define nav-links
    (map (λ (pair)
            (match-define (cons name file) pair)
            (define attributes
              (if (string=? title name)
                  '([class "nav-link active"]
                    [aria-current "page"])
                  '([class "nav-link"])))
            `(a (,@(cons `(href ,file) attributes))
              ,name))
         '(["home" . "index.html"]
           ["log"  . "log.html"])))
  `(html (head (title ,title " @ Hack Free Or Die")
               (meta ([name "generator"]
                      [content "https://racket-lang.org/"]))
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
               (header ([class "mb-auto"])
                       ; TODO Generate nav smarter.
                       (div
                         ; TODO Do we want this logoish thing here?
                         ;(h3 ([class "float-md-start mb-0"]) "hack && tell")
                         (nav ([class "nav nav-masthead justify-content-center float-md-end"])
                              ,@nav-links)))
               (main ([class "px-3"])
                     ,@content)
               (footer ([class "mt-auto text-white-50"])
                       (p "Inspired by "
                          (a ([href "https://hackandtell.org/"]
                              [class "text-white"])
                             "NYC Hack && Tell")))))))

(define/contract (page-log)
  (-> xml:xexpr/c)
  (define title "log")
  (page title
        `((h1 ,title)
          (table ([class "table table-dark table-striped table-hover"])
                 (thead
                   (tr (th ([scope "col"]) "#")
                       (th ([scope "col"]) "date")
                       (th ([scope "col"]) "codename")
                       (th ([scope "col"]) "host")))
                 (tbody
                   ,@(map (λ (m)
                             (define h (Meeting-host m))
                             `(tr
                               (th ([scope "row"]) ,(number->string (Meeting-seq m)))
                               (td ,(g:~t (Meeting-date m) "yyyy MMM dd"))
                               (td ,(Meeting-codename m))
                               (td (a ([href ,(url:url->string (Host-url h))]) ,(Host-name h)))))
                          (sort meetings-past
                                (λ (a b) (> (Meeting-seq a)
                                            (Meeting-seq b))))))
                 ))))

(define/contract (page-home)
  (-> xml:xexpr/c)
  (define content
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
             [href ,(include "join-us-button-mailto.txt")])
            "Join us"))
      ,enable-tooltips))
  (page "home" content))

(define/contract (pages)
  (-> (listof (cons/c path-string? xml:xexpr/c)))
  `(["index.html" . ,(page-home)]
    ["log.html"   . ,(page-log)]))

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
    ; TODO Add option to generate indented output, for debugging diffs.
    (command-line
      #:program (find-system-path 'run-file)
      #:once-each
      [("-o" "--out-dir")
       path "Directory to which to write page files. DEFAULT: ./dist"
       (invariant-assertion path-string? path)
       (set! out-dir (normalize-path path))])
    (main out-dir)))
