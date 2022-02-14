#! /usr/bin/env racket

#lang racket

(require (prefix-in url: net/url)
         (prefix-in xml: xml))

(require (prefix-in g: gregor)
         (prefix-in g: gregor/time))

(define u url:string->url)

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
                  [website (or/c #f url:url?)]
                  [affiliated-links (listof url:url?)]))

(struct/contract Ref
                 ([name string?]
                  [url url:url?]))

(struct/contract Talk
                 ([presenter Presenter?]
                  [title string?]
                  [description string?]
                  [sources (listof url:url?)] ; XXX We really should not allow this to be empty.
                  [website (or/c #f url:url?)]
                  [references (listof Ref?)]
                  [photos (listof path?)]
                  ))

(struct/contract Meeting
                 ([seq nonnegative-integer?]
                  [codename string?]
                  [date g:date?]
                  [time g:time?]
                  [host Host?]
                  [talks (listof Talk?)]
                  [recap string?])
                 #:transparent)

;; TODO Can we automate making these keyworded constructors with a macro?

(define (P #:name name
           #:email email
           #:website website
           #:affiliated-links links)
  (Presenter name (string-downcase email) website links))

(define (T #:presenter presenter
           #:title title
           #:description description
           #:sources sources
           #:website website
           #:references references
           #:photos photos)
  (Talk presenter title description sources website references photos))

(define (M #:seq seq
           #:codename codename
           #:date date
           #:time time
           #:host host
           #:talks talks
           #:recap recap)
  (Meeting seq codename date time host talks recap))

(define/contract url-raven-labs url:url? (u "ravenlabsnh.com"))

(define presenter-kyle-robertson
  (P #:name "Kyle Robertson"
     #:email "kyle.wesley@me.com"
     #:website #f
     #:affiliated-links (list (u "https://github.com/kwrobert")
                              url-raven-labs)))

(define presenter-jeff-nelson
  (P #:name "Jeff Nelson"
     #:email "jeff@ravenlabsnh.com"
     #:website #f
     #:affiliated-links (list url-raven-labs)))

(define presenter-zach-taylor
  (P #:name "Zach Taylor"
     #:email "zach@taylorzr.com"
     #:website #f
     #:affiliated-links (list (u "https://www.reddit.com/user/taylorzr"))))

(define presenter-siraaj-khandkar
  (P #:name "Siraaj Khandkar"
     #:email "siraaj@khandkar.net"
     #:website (u "https://xandkar.net")
     #:affiliated-links (list (u "https://github.com/xandkar"))))

(define presenter-bob-peret
  (P #:name "Bob Peret"
     #:email ""
     #:website #f
     #:affiliated-links (list url-raven-labs)))

(define presenter-kyle-roucis
  (P #:name "Kyle Roucis"
     #:email "kyle@kyleroucis.com"
     #:website (u "https://www.kyleroucis.com")
     #:affiliated-links (list (u "https://github.com/kroucis"))))

(define presenter-grant-peret
  (P #:name "Grant Peret"
     #:email "grant@ravenlabsnh.com"
     #:website #f
     #:affiliated-links (list url-raven-labs)))

(define host-raven-labs
  (Host "Raven Labs"
        (Addr "913"
              "Elm St"
              "Suite 405"
              "Manchester"
              "NH"
              "03101"
              "USA")
        (u "https://www.ravenlabsnh.com")))

(define host-manch-maker-space
  (Host "Manchester Makerspace"
        (Addr "36"
              "Old Granite St"
              ""
              "Manchester"
              "NH"
              "0301"
              "USA")
        (u "https://manchestermakerspace.org")))

(define/contract meetings
  (listof Meeting?)
  (let ([d g:date]
        [t g:time])
    (list (M #:seq 0
             #:codename "Ground Zero"
             #:date (d 2022 01 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:recap ""
             #:talks '())

          (M #:seq 1
             #:codename "Genesis"
             #:date (d 2022 02 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:recap ""
             #:talks
             ; TODO Talks:
             ; - [x] Kyle Robertson: optimization
             ; - [x] Jeff Nelson: BTC/Lightening POS on Raspberry Pi
             ; - [x] Zach Taylor: DIY keyboard on a cardboard
             ; - [x] Siraaj Khandkar: pista
             ; - [ ] Bob Peret: interns making mirrors
             ; - [x] Kyle Roucis: Lobjan
             ; - [ ] Grant Peret: Cat Alley sign story
             (list (T #:presenter presenter-kyle-robertson
                      #:title "Mathematical Programming and Optimization with Python and Pyomo"
                      #:description "A quick 5 minute introduction to using Python and the Pyomo library to set up and solve combinatorial optimization problems by demonstrating the solution of an example optimal scheduling problem."
                      #:sources (list (u "https://github.com/kwrobert/pyomo-presentation"))
                      #:website #f
                      #:references
                      '()
                      #:photos '())

                   (T #:presenter presenter-jeff-nelson
                      #:title "RaspiBLitz w/ pay server"
                      #:description "Raspberry pi setup running raspiblitz with other services like pay server and exlplorers."
                      #:sources (list (u "https://github.com/rootzoll/raspiblitz"))
                      #:website #f
                      #:references
                      '() ; TODO Links to all component sources.
                      #:photos '())

                   (T #:presenter presenter-zach-taylor
                      #:title "DIY mechanical split keyboard from cardboard!"
                      #:description "A demo of the current experiment and an overviewing of the many leading up prototyping experiments with cardboard and handwiring."
                      #:sources '() ; TODO Need source links.
                      #:website #f
                      #:references
                      '() ; TODO Need some links to component sources.
                      #:photos '())

                   (T #:presenter presenter-siraaj-khandkar
                      #:title "pista: a hacker's status bar"
                      #:description "Piped status: the ii of status bars! Asynchronously reads lines from N FIFOs and routes to corresponding N slots on the bar. After a TTL without updates, a slot is cleared."
                      #:sources (list (u "https://github.com/xandkar/pista"))
                      #:website #f
                      #:references
                      (list
                        (Ref "dwm" (u "https://dwm.suckless.org/"))
                        (Ref "ii" (u "https://tools.suckless.org/ii/"))
                        (Ref "status experiments" (u "https://github.com/xandkar/khatus/")))
                      #:photos '())

                   ; TODO Get details from Bob.
                   ;(T #:presenter presenter-bob-peret
                   ;   #:title ""
                   ;   #:description ""
                   ;   #:sources (list (u ""))
                   ;   #:website #f
                   ;   #:references
                   ;   '()
                   ;   #:photos '())

                   (T #:presenter presenter-kyle-roucis
                      #:title "Lojban: the logical language for nerds"
                      #:description "Lojban is an \"open source\" logical language built on predicate logic. Its grammar is unambiguous, logically constructed, and simple to learn. It has about 1300 root words from which sentences and compound works can be created. It’s a fun little toy language with 300-500 active learners across the globe. Lojban is so simple and easy, I have taught a number of people who were able to parse and understand complete sentences in just 1 hour."
                      #:sources (list (u "https://gist.githubusercontent.com/kroucis/c1587dc09b5b9b33c880/raw/b792965f9eb17f1247ae96dd349119d67f03f4a0/lo%2520nu%2520tumfakli%27u"))
                      #:website (u "Lojban.org")
                      #:references
                      (list (Ref "book"       (u "https://lojban.org/publications/cll/cll_v1.1_book.pdf"))
                            (Ref "dictionary" (u "https://la-lojban.github.io/sutysisku/lojban/index.html")))
                      #:photos '())

                   ; TODO Get details from Grant.
                   ;(T #:presenter presenter-grant-peret
                   ;   #:title ""
                   ;   #:description ""
                   ;   #:sources (list (u ""))
                   ;   #:website #f
                   ;   #:references
                   ;   '()
                   ;   #:photos '())
                   ))

          (M #:seq 2
             #:codename "TBD"
             #:date (d 2022 03 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:recap ""
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

(define/contract (page #:nav-section nav-section
                       #:title title
                       content)
  (-> #:nav-section string?
      #:title string?
      (listof xml:xexpr/c)
      xml:xexpr/c)
  (define nav-links
    (map (λ (pair)
            (match-define (cons name file) pair)
            (define attributes
              (if (string=? nav-section name)
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
  (page #:nav-section title
        #:title title
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
                               (td (a ([href ,(format "meeting-~a.html" (Meeting-seq m))]) ,(Meeting-codename m)))
                               (td (a ([href ,(url:url->string (Host-url h))]) ,(Host-name h)))))
                          (sort meetings-past
                                (λ (a b) (> (Meeting-seq a)
                                            (Meeting-seq b))))))))))

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
  (define title "home")
  (page #:nav-section title #:title title content))

(define/contract (talk->card t)
  (-> Talk? xml:xexpr/c)
  (define p (Talk-presenter t))
  `(div ([class "card h-100 bg-dark text-light"])
    ;(img ([class "card-img-top"]
    ;      [src ""]
    ;      [alt ""]))
    (div ([class "card-header"])
         (h5 ([class "card-title text-center"]) ,(Talk-title t)))
    (div ([class "card-body"])
         (p  ([class "card-title text-center"])
            "by "
            ,(if (Presenter-website p)
                 `(a ([href ,(url:url->string (Presenter-website p))]) ,(Presenter-name p))
                 (Presenter-name p))
            ; TODO Render email addr text as image:
            ;      (define email (Presenter-email p))
            ;      (define filename (string-append email ".png"))
            ;      (send (pict:pict->bitmap (pict:text email)) save-file filename 'png)
            ;      ; TODO Tweak colors to match site theme.
            )
         ; XXX "lead" seems semantically not ideal here, but seems to work OK.
         (p  ([class "card-text text-start lead"])
            ,(Talk-description t))
         ,(if (empty? (Talk-sources t))
              ""
              `(p  ([class "card-text text-start"])
                (strong "artifacts:") ; TODO Rename Talk-sources to Talk-artifacts?
                (ul ([class "text-start"])
                    ,@(map (λ (s)
                              (define url (url:url->string s))
                              `(li (a ([href ,url]) ,url)))
                           (Talk-sources t)))))
         ,(if (empty? (Talk-references t))
              ""
              `(p  ([class "card-text text-start"])
                (strong "references:")
                (ul ([class "text-start"])
                    ,@(map (λ (r)
                              `(li (a ([href ,(url:url->string (Ref-url r))]) ,(Ref-name r))))
                           (Talk-references t))))))
    (div ([class "card-footer"]) "")))

(define/contract (page-meeting m)
  (-> Meeting? xml:xexpr/c)
  (define title (format "~a: ~a" (Meeting-seq m) (Meeting-codename m)))
  (define cards (map talk->card (Meeting-talks m)))
  (define cols (map (λ (c) `(div ([class "col"]) ,c)) cards))
  (page
    #:nav-section "log"
    #:title title
    `((h1 ,title)
      (p ([class "lead"])
         ,(Meeting-recap m))
      (div ([class "row row-cols-1 row-cols-md-1 g-4"]) ,@cols))))

(define/contract (pages)
  (-> (listof (cons/c path-string? xml:xexpr/c)))
  `(["index.html" . ,(page-home)]
    ["log.html"   . ,(page-log)]
    ,@(map (λ (m) `(,(format "meeting-~a.html" (Meeting-seq m)) . ,(page-meeting m))) meetings-past)))

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
