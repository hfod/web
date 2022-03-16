#lang racket

(provide (contract-out
           [meeting-next (or/c #f Meeting?)]
           [meetings-past (listof Meeting?)]))

(require (prefix-in url: net/url))

(require (prefix-in g: gregor)
         (prefix-in g: gregor/time))

(require "model.rkt")

;; TODO Automate making these keyworded constructors with a macro?
;;      But we do need to keep the custom from-file-to-field readers,
;;      so it isn't a straight translation.

(define (P #:name name
           #:email email
           #:website website
           #:affiliated-links links)
  (Presenter name (string-downcase email) website links))

(define (T #:presenter presenter
           #:title title
           #:description description
           #:artifacts artifacts
           #:website website
           #:references references)
  (Talk presenter title description website artifacts references))

(define (M #:seq seq
           #:codename codename
           #:date date
           #:time time
           #:host host
           #:talks talks
           #:registration-url reg-url)
  (define meeting-dir (build-path "data" "meetings" (number->string seq)))
  (define photos
    (let ([dir (build-path meeting-dir "photos")])
      (if (directory-exists? dir)
          (let* ([files
                   (filter file-exists? ; XXX Ignore (tmp) directories.
                           (map (λ (file) (build-path dir file))
                                (directory-list dir)))]
                 [photo-files
                   (filter
                     (λ (path)
                        (define ext
                          (string-downcase (bytes->string/utf-8 (path-get-extension path))))
                        (and ext (member ext '(".png"
                                               ".jpg"
                                               ".jpeg"))))
                     files)])
            (map (λ (photo-file)
                    (define caption
                      (let ([caption-file (path-replace-extension photo-file ".txt")])
                        (if (file-exists? caption-file)
                            (file->string caption-file)
                            "")))
                    (define data (file->bytes photo-file))
                    (Photo data caption))
                 photo-files))
          '())))
  (define recap
    (let ([recap-file (build-path meeting-dir "recap.md")])
      (if (file-exists? recap-file)
          (file->string recap-file)
          "")))
  (Meeting seq codename date time host talks recap photos reg-url))

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
     #:website (u "http://taylorzr.com")
     #:affiliated-links (list (u "https://www.reddit.com/user/taylorzr")
                              (u "https://github.com/taylorzr"))))

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

(define presenter-brandon-simpson
  (P #:name "Brandon Simpson"
     #:email "2boog25@gmail.com"
     #:website (u "https://bgsimpson.wixsite.com/brandon")
     #:affiliated-links (list (u "https://github.com/vermontolympian/"))))

(define presenter-bryan-TODO
  (P #:name "Bryan"
     #:email ""
     #:website #f
     #:affiliated-links '()))

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
    (list (M #:seq -1
             #:codename "Prehistory"
             #:date (d 2021 10 14)
             #:time (t 19 00)
             #:host host-raven-labs
             #:registration-url (u "https://discord.com/channels/404106811252408320/824002124899811347")
             #:talks '())

          (M #:seq 0
             #:codename "Ground Zero"
             #:date (d 2022 01 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:registration-url (u "")
             #:talks '())

          (M #:seq 1
             #:codename "Genesis Block"
             #:date (d 2022 02 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:registration-url
             (let* ([file "join-us-button-mailto.txt"]
                    ; FIXME Tangled abstractions - we're not in a view!!!
                    [path (build-path "view" "web" "inc" file)]
                    [mailto (file->string path)])
               (u mailto))
             #:talks
             ; TODO Talks:
             ; - [x] Kyle Robertson: optimization
             ; - [x] Jeff Nelson: BTC/Lightening POS on Raspberry Pi
             ; - [x] Zach Taylor: DIY keyboard on a cardboard
             ; - [x] Siraaj Khandkar: pista
             ; - [ ] Bob Peret: interns making mirrors
             ; - [x] Kyle Roucis: Lobjan
             ; - [x] Grant Peret: Cat Alley sign story
             (list (T #:presenter presenter-kyle-robertson
                      #:title "Mathematical Programming and Optimization with Python and Pyomo"
                      #:description "A quick 5 minute introduction to using Python and the Pyomo library to set up and solve combinatorial optimization problems by demonstrating the solution of an example optimal scheduling problem."
                      #:artifacts (list (Link #f (u "https://github.com/kwrobert/pyomo-presentation")))
                      #:website #f
                      #:references
                      '())

                   (T #:presenter presenter-jeff-nelson
                      #:title "RaspiBLitz w/ pay server"
                      #:description "Raspberry pi setup running raspiblitz with other services like pay server and exlplorers."
                      #:artifacts (list (Link #f (u "https://github.com/rootzoll/raspiblitz")))
                      #:website #f
                      #:references
                      '() ; TODO Links to all component artifacts.
                      )

                   (T #:presenter presenter-zach-taylor
                      #:title "DIY mechanical split keyboard from cardboard!"
                      #:description "A demo of the current experiment and an overviewing of the many leading up prototyping experiments with cardboard and handwiring."
                      #:artifacts (list (Link #f (u "https://github.com/taylorzr/qmk_firmware")))
                      #:website #f
                      #:references
                      ; TODO Need some links to components
                      (list
                        (Link #f (u "https://www.reddit.com/r/ErgoMechKeyboards/comments/shy8hz/6_column_splay_split_handwired_cardboard/"))))

                   (T #:presenter presenter-siraaj-khandkar
                      #:title "pista: a hacker's status bar"
                      #:description "Piped status: the ii of status bars! Asynchronously reads lines from N FIFOs and routes to corresponding N slots on the bar."
                      #:artifacts (list (Link #f (u "https://github.com/xandkar/pista")))
                      #:website #f
                      #:references
                      (list
                        (Link "dwm" (u "https://dwm.suckless.org/"))
                        (Link "ii" (u "https://tools.suckless.org/ii/"))
                        (Link "status experiments" (u "https://github.com/xandkar/khatus/"))))

                   ; TODO Get details from Bob.
                   ;(T #:presenter presenter-bob-peret
                   ;   #:title ""
                   ;   #:description ""
                   ;   #:artifacts (list (Link #f (u "")))
                   ;   #:website #f
                   ;   #:references
                   ;   '())

                   (T #:presenter presenter-kyle-roucis
                      #:title "Lojban: the logical language for nerds"
                      #:description "Lojban is an \"open source\" logical language built on predicate logic. Its grammar is unambiguous, logically constructed, and simple to learn. It has about 1300 root words from which sentences and compound works can be created. It’s a fun little toy language with 300-500 active learners across the globe. Lojban is so simple and easy, I have taught a number of people who were able to parse and understand complete sentences in just 1 hour."
                      #:artifacts (list (Link #f (u "https://gist.githubusercontent.com/kroucis/c1587dc09b5b9b33c880/raw/b792965f9eb17f1247ae96dd349119d67f03f4a0/lo%2520nu%2520tumfakli%27u")))
                      #:website (u "Lojban.org")
                      #:references
                      (list (Link "book"       (u "https://lojban.org/publications/cll/cll_v1.1_book.pdf"))
                            (Link "dictionary" (u "https://la-lojban.github.io/sutysisku/lojban/index.html"))))

                   (T #:presenter presenter-grant-peret
                      #:title "Cat Alley - Creation of an Entryway"
                      #:description "An overview of the aesthetic modelling, design, and loading requirements to build a cantilevered entry way sign."
                      #:artifacts (list (Link #f (u "https://github.com/RavenGrant/CatAlley")))
                      #:website #f
                      #:references
                      (list (Link "Atlas Obscura" (u "https://www.atlasobscura.com/places/cat-alley"))
                            (Link "Hidden New England" (u "https://hiddennewengland.com/2019/01/19/cat-alley-manchester-nh/"))))
                   ))

          (M #:seq 2
             #:codename "Tuna Pizza"
             #:date (d 2022 03 10)
             #:time (t 18 00)
             #:host host-raven-labs
             #:registration-url (u "https://forms.gle/nYPmUnhkDEro9Nft8")
             #:talks
             (list (T #:presenter presenter-siraaj-khandkar
                      #:title "gg - the gitter of gits"
                      #:description "A tool to locate, compare and cross-reference all your git repositories accross machines."
                      #:artifacts (list (Link #f (u "https://github.com/xandkar/gg/")))
                      #:website #f
                      #:references '())
                   ;(T #:presenter presenter-jeff-nelson
                   ;   #:title ""
                   ;   #:description ""
                   ;   #:artifacts '()
                   ;   #:website #f
                   ;   #:references '())
                   ;(T #:presenter presenter-bryan-TODO
                   ;   #:title ""
                   ;   #:description ""
                   ;   #:artifacts '()
                   ;   #:website #f
                   ;   #:references '())
                   ;(T #:presenter presenter-grant-peret
                   ;   #:title ""
                   ;   #:description ""
                   ;   #:artifacts '()
                   ;   #:website #f
                   ;   #:references '())
                   (T #:presenter presenter-brandon-simpson
                      #:title "3 DOF Robotic Arm"
                      #:description "In this project, my team and I used a 3 DOF robotic manipulator and a USB webcam to implement an automated pick and place system. Through image processing, the system was able to detect and locate objects of a specific color. Using forward and inverse position and velocity kinematics, my team and Ideveloped a program to command a robotic arm to pick and place colored spheres until there were none remaining in the workspace. This system was also capable of sorting a specific non-spherical random object and able to dynamically track an object."
                      #:artifacts `(,(Link #f (u "https://bgsimpson.wixsite.com/brandon/post/unified-robotics-iii")))
                      #:website #f
                      #:references '())
                   (T #:presenter presenter-kyle-roucis
                      #:title "KroucisVM"
                      #:description "A bytecode-driven dynamic-dispatched object-oriented so-many-hyphens virtual machine built in C and based on the Objective-C dynamic dispatch object model."
                      #:artifacts (list (Link #f (u "https://github.com/kroucis/KroucisVM")))
                      #:website #f
                      #:references '())
                   ;(T #:presenter presenter-bob-peret
                   ;   #:title ""
                   ;   #:description ""
                   ;   #:artifacts '()
                   ;   #:website #f
                   ;   #:references '())
                   ))

          (M #:seq 3
             #:codename "TBD"
             #:date (d 2022 04 07)
             #:time (t 18 00)
             #:host host-manch-maker-space
             #:registration-url (u "https://forms.gle/uwTZM4gcWc6RcQyq6")
             #:talks
             '())
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
  (or/c #f Meeting?)
  (match meetings-future
    ['() #f]
    [ms
      (first (sort ms (λ (m1 m2) (g:date<? (Meeting-date m1)
                                           (Meeting-date m2)))))]))
