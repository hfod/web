#lang racket

(provide Addr-town

         Host-addr
         Host-name
         Host-url

         Meeting-codename
         Meeting-date
         Meeting-host
         Meeting-recap
         Meeting-registration-url
         Meeting-seq
         Meeting-talks
         Meeting-time
         Meeting?

         Presenter-name
         Presenter-website

         Ref-name
         Ref-url

         Talk-description
         Talk-presenter
         Talk-references
         Talk-sources
         Talk-title
         Talk?

         meetings-past
         meeting-next)

(require (prefix-in url: net/url))

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
                  [recap string?]
                  [registration-url url:url?])
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
           #:recap recap
           #:registration-url reg-url)
  (Meeting seq codename date time host talks recap reg-url))

(define u url:string->url)

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

(define/contract (inc file)
  (-> path-string? string?)
  (file->string (build-path "inc" file)))

(define/contract meetings
  (listof Meeting?)
  (let ([d g:date]
        [t g:time])
    (list (M #:seq 0
             #:codename "Ground Zero"
             #:date (d 2022 01 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:registration-url (u "")
             #:recap ""
             #:talks '())

          (M #:seq 1
             #:codename "Genesis"
             #:date (d 2022 02 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:registration-url (u (inc "join-us-button-mailto.txt"))
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
                      #:description "Piped status: the ii of status bars! Asynchronously reads lines from N FIFOs and routes to corresponding N slots on the bar."
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
             #:registration-url (u "https://forms.gle/nYPmUnhkDEro9Nft8")
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

(define/contract meeting-next
  Meeting?
  (match meetings-future
    ['() #f]
    [ms
      (first (sort ms (λ (m1 m2) (g:date<? (Meeting-date m1)
                                           (Meeting-date m2)))))]))
