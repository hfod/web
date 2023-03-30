#lang racket

(provide (contract-out
           [hosts (listof Host?)]
           [speakers (listof Speaker?)]
           [speaker->talks (-> Speaker? (listof (cons/c integer? Talk?)))]
           [speaker->meetings-organized (-> Speaker? (listof Meeting?))]
           [meeting-next (or/c #f Meeting?)]
           [meetings (listof Meeting?)]
           [meetings-future (listof Meeting?)]
           [meetings-past (listof Meeting?)]))

(require (prefix-in url: net/url))

(require (prefix-in g: gregor)
         (prefix-in g: gregor/time))

(require "model.rkt")

;; TODO Automate making these keyworded constructors with a macro?
;;      But we do need to keep the custom from-file-to-field readers,
;;      so it isn't a straight translation.

(define (S #:id id
           #:name name
           #:email email
           #:email-show? [email-show? #t]
           #:website website
           #:affiliated-links links)
  (Speaker id name (string-downcase email) email-show? website links))

(define (T #:speaker speaker
           #:title title
           #:description description
           #:artifacts artifacts
           #:website website
           #:references references)
  (Talk speaker title description website artifacts references))

(define (M #:seq seq
           #:format fmt
           #:codename codename
           #:date date
           #:time time
           #:host host
           #:organizer organizer
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
  (Meeting seq fmt codename date time host organizer talks recap photos reg-url))

(define u url:string->url)

(define/contract url-raven-labs url:url? (u "https://ravenlabsnh.com"))
(define/contract url-manchester-maker-space url:url? (u "https://manchestermakerspace.org"))

(define (tbl #:key key . xs)
  (define seen (make-hash))
  (foldl
    (λ (x t)
       (define k (key x))
       (hash-update! seen k add1 0)
       (invariant-assertion (<=/c 1) (hash-ref seen k))
       (hash-set t k x))
    (hash)
    xs))

(define speaker-id-ian-mac                  "ian-mac")
(define speaker-id-max-sklar                "max-sklar")
(define speaker-id-jonathan-hales           "jonathan-hales")
(define speaker-id-perry-e-metzger          "perry-e-metzger")
(define speaker-id-bill-the-shoelace-hater  "bill-the-shoelace-hater")
(define speaker-id-kyle-robertson           "kyle-robertson")
(define speaker-id-jeff-nelson              "jeff-nelson")
(define speaker-id-zach-taylor              "zach-taylor")
(define speaker-id-siraaj-khandkar          "siraaj-khandkar")
(define speaker-id-bob-peret                "bob-peret")
(define speaker-id-kyle-roucis              "kyle-roucis")
(define speaker-id-grant-peret              "grant-peret")
(define speaker-id-brandon-simpson          "brandon-simpson")
(define speaker-id-brian-gray               "brian-gray")
(define speaker-id-kevin-kadow              "kevin-kadow")
(define speaker-id-thaddeus-hughes          "thaddeus-hughes")
(define speaker-id-jake                     "jake")  ; Discord: ta11ey#6015
(define speaker-id-nick-FreakyNobleGas      "nick-FreakyNobleGas")
(define speaker-id-lam                      "lam")
(define speaker-id-jim-roucis               "jim-roucis")
(define speaker-id-jared                    "jared")
(define speaker-id-tigran-khandkar          "tigran-khandkar")
(define speaker-id-jack-jutzi               "jack-jutzi")
(define speaker-id-lisa                     "lisa")
(define speaker-id-daniel-krol              "daniel-krol")
(define speaker-id-fare                     "fare")
(define speaker-id-meg                      "meg-truesdale")
(define speaker-id-wayne                    "wayne-kenney")

(define host-id-raven-labs            "raven-labs")
(define host-id-red-oak-hanover       "red-oak-hanover")
(define host-id-manchester-makerspace "manchester-makerspace")

; TODO Rename to "member", "person" or anything else more applicably general.
(define speaker
  (let ([speakers
          (tbl #:key Speaker-id
               ;(S #:id speaker-id-
               ;   #:name ""
               ;   #:email ""
               ;   #:email-show? #f
               ;   #:website #f
               ;   #:affiliated-links '())

               (S #:id speaker-id-ian-mac
                  #:name "Ian"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())
               (S #:id speaker-id-max-sklar
                  #:name "Max Sklar"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links (list (u "https://twitter.com/maxsklar")
                                           (u "https://www.localmaxradio.com/")))
               (S #:id speaker-id-jonathan-hales
                  #:name "Jonathan Hales"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links (list (u "https://twitter.com/jayjayHales")
                                           (u "https://github.com/jkhales")))

               (S #:id speaker-id-perry-e-metzger
                  #:name "Perry E. Metzger"
                  #:email "perry@piermont.com"
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links (list (u "https://twitter.com/perrymetzger")
                                           (u "https://github.com/pmetzger")))

               (S #:id speaker-id-kyle-robertson
                  #:name "Kyle Robertson"
                  #:email "kyle.wesley@me.com"
                  #:website #f
                  #:affiliated-links (list (u "https://github.com/kwrobert")
                                           url-raven-labs))

               (S #:id speaker-id-jeff-nelson
                  #:name "Jeff Nelson"
                  #:email "jeff@ravenlabsnh.com"
                  #:website #f
                  #:affiliated-links (list (u "https://github.com/jnelson614")
                                           url-raven-labs))

               (S #:id speaker-id-zach-taylor
                  #:name "Zach Taylor"
                  #:email "zach@taylorzr.com"
                  #:website (u "http://taylorzr.com")
                  #:affiliated-links (list (u "https://www.reddit.com/user/taylorzr")
                                           (u "https://github.com/taylorzr")))

               (S #:id speaker-id-siraaj-khandkar
                  #:name "Siraaj Khandkar"
                  #:email "siraaj@khandkar.net"
                  #:website (u "https://xandkar.net")
                  #:affiliated-links (list (u "https://github.com/xandkar")))

               (S #:id speaker-id-bob-peret
                  #:name "Bob Peret"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links (list url-raven-labs))

               (S #:id speaker-id-kyle-roucis
                  #:name "Kyle Roucis"
                  #:email "kyle@kyleroucis.com"
                  #:website (u "https://www.kyleroucis.com")
                  #:affiliated-links (list (u "https://github.com/kroucis")))

               (S #:id speaker-id-grant-peret
                  #:name "Grant Peret"
                  #:email "grant@ravenlabsnh.com"
                  #:website #f
                  #:affiliated-links (list url-raven-labs))

               (S #:id speaker-id-brandon-simpson
                  #:name "Brandon Simpson"
                  #:email "2boog25@gmail.com"
                  #:email-show? #f
                  #:website (u "https://bgsimpson.wixsite.com/brandon")
                  #:affiliated-links (list (u "https://github.com/vermontolympian/")))

               (S #:id speaker-id-brian-gray
                  #:name "Brian Gray"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-kevin-kadow
                  #:name "Kevin"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links (list url-manchester-maker-space))

               (S #:id speaker-id-thaddeus-hughes
                  #:name "Thaddeus Hughes"
                  #:email "hughes.thad@gmail.com"
                  #:email-show? #f
                  #:website (u "http://thadhughes.xyz")
                  #:affiliated-links
                  (list (u "https://github.com/Thaddeus-Maximus")
                        url-manchester-maker-space))

               (S #:id speaker-id-jake
                  #:name "Jake"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-bill-the-shoelace-hater
                  #:name "Bill"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-nick-FreakyNobleGas
                  #:name "Nick"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-lam
                  #:name "Lam"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-jim-roucis
                  #:name "Jim Roucis"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-jared
                  #:name "Jared"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-tigran-khandkar
                  #:name "Tigran Khandkar"
                  #:email ""
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-daniel-krol
                  #:name "Daniel Krol"
                  #:email ""
                  #:email-show? #f
                  #:website (u "https://danielkrol.com/")
                  #:affiliated-links (list (u "https://github.com/orblivion")
                                           (u "https://gitlab.com/orblivion")
                                           (u "https://www.linkedin.com/in/danielkrol/")))

               (S #:id speaker-id-jack-jutzi
                  #:name "Jack Jutzi"
                  #:email "jsjutzi@gmail.com"
                  #:website #f
                  #:affiliated-links (list (u "https://github.com/jsjutzi")))

               (S #:id speaker-id-lisa
                  #:name "Lisa"
                  #:email ""
                  #:email-show? #f
                  #:website #f
                  #:affiliated-links '())

               (S #:id speaker-id-fare
                  #:name "François-René \"Faré\" Rideau"
                  #:email "fahree@gmail.com"
                  #:email-show? #t
                  #:website (u "https://ngnghm.github.io/")
                  #:affiliated-links (list (u "https://github.com/fare")
                                           (u "https://twitter.com/Ngnghm")))

               (S #:id speaker-id-meg
                  #:name "Meg Truesdale"
                  #:email "megtruesdale@gmail.com"
                  #:email-show? #t
                  #:website #f
                  #:affiliated-links (list (u "https://github.com/megtruesdale")))

               (S #:id speaker-id-wayne
                  #:name "Wayne Kenney"
                  #:email ""
                  #:email-show? #f
                  #:website (u "https://wayne.cool/")
                  #:affiliated-links (list (u "https://github.com/LogicPy")))

               )])
    (λ (id) (hash-ref speakers id))))

(define host
  (let ([hosts
          (tbl #:key Host-id
               (Host host-id-raven-labs
                     "Raven Labs"
                     (Addr "913"
                           "Elm St"
                           "Suite 405"
                           "Manchester"
                           "NH"
                           "03101"
                           "USA"
                           (u "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d2918.5021479285165!2d-71.46491045831543!3d42.98875989907271!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x89e24fece398a01f%3A0xd63afeaabdeeb64d!2sRaven%20Laboratory%2C%20LLC!5e0!3m2!1sen!2sus!4v1647814700969!5m2!1sen!2sus"))
                     url-raven-labs
                     (speaker speaker-id-grant-peret))
               (Host host-id-red-oak-hanover
                     "Red Oak Coworking Offices"
                     (Addr "66"
                           "Hanover St"
                           "Suite 200"
                           "Manchester"
                           "NH"
                           "03101"
                           "USA"
                           (u "https://www.google.com/maps/embed?pb=!1m14!1m8!1m3!1d1933.9391187294636!2d-71.46238682768785!3d42.98960320228865!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x89e24f2480bf4587%3A0xec5c87bd4c8bc022!2sRed%20Oak%20Coworking%20Offices!5e0!3m2!1sen!2sus!4v1677192211322!5m2!1sen!2sus"))
                     (u "https://redoakcoworking.com")
                     (speaker speaker-id-siraaj-khandkar))
               (Host host-id-manchester-makerspace
                     "Manchester Makerspace"
                     (Addr "36"
                           "Old Granite St"
                           ""
                           "Manchester"
                           "NH"
                           "0301"
                           "USA"
                           (u "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d2918.609093458723!2d-71.46651978372435!3d42.98650747914976!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x89e24ed838cd5e57%3A0x511bc564be09b62b!2sManchester%20Makerspace!5e0!3m2!1sen!2sus!4v1647814467210!5m2!1sen!2sus"))
                     url-manchester-maker-space
                     (speaker speaker-id-kevin-kadow)))])
    (λ (id) (hash-ref hosts id))))

(define/contract (inc file)
  (-> path-string? string?)
  (file->string (build-path "inc" file)))

(define/contract meetings
  (listof Meeting?)
  (let ([d g:date]
        [t g:time])
    (list
      (M #:seq -1
         #:format 'meet-and-greet
         #:codename "Prehistory"
         #:date (d 2021 10 14)
         #:time (t 19 00)
         #:host (host host-id-raven-labs)
         #:organizer (speaker speaker-id-grant-peret)
         #:registration-url (u "https://discord.com/channels/404106811252408320/824002124899811347")
         #:talks '())

      (M #:seq 0
         #:format 'meet-and-greet
         #:codename "Ground Zero"
         #:date (d 2022 01 10)
         #:time (t 18 00)
         #:host (host host-id-raven-labs)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "")
         #:talks '())

      (M #:seq 1
         #:format 'show-and-tell
         #:codename "Genesis Block"
         #:date (d 2022 02 10)
         #:time (t 18 00)
         #:host (host host-id-raven-labs)
         #:organizer (speaker speaker-id-siraaj-khandkar)
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
         (list (T #:speaker (speaker speaker-id-kyle-robertson)
                  #:title "Mathematical Programming and Optimization with Python and Pyomo"
                  #:description "A quick 5 minute introduction to using Python and the Pyomo library to set up and solve combinatorial optimization problems by demonstrating the solution of an example optimal scheduling problem."
                  #:artifacts (list (Link #f (u "https://github.com/kwrobert/pyomo-presentation")))
                  #:website #f
                  #:references
                  '())

               (T #:speaker (speaker speaker-id-jeff-nelson)
                  #:title "RaspiBLitz w/ pay server"
                  #:description "Raspberry pi setup running raspiblitz with other services like pay server and exlplorers."
                  #:artifacts (list (Link #f (u "https://github.com/rootzoll/raspiblitz")))
                  #:website #f
                  #:references
                  '() ; TODO Links to all component artifacts.
                  )

               (T #:speaker (speaker speaker-id-zach-taylor)
                  #:title "DIY mechanical split keyboard from cardboard!"
                  #:description "A demo of the current experiment and an overviewing of the many leading up prototyping experiments with cardboard and handwiring."
                  #:artifacts (list (Link #f (u "https://github.com/taylorzr/qmk_firmware")))
                  #:website #f
                  #:references
                  ; TODO Need some links to components
                  (list
                    (Link #f (u "https://www.reddit.com/r/ErgoMechKeyboards/comments/shy8hz/6_column_splay_split_handwired_cardboard/"))))

               (T #:speaker (speaker speaker-id-siraaj-khandkar)
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
               (T #:speaker (speaker speaker-id-bob-peret)
                  #:title ""
                  #:description ""
                  #:artifacts '()
                  #:website #f
                  #:references
                  '())

               (T #:speaker (speaker speaker-id-kyle-roucis)
                  #:title "Lojban: the logical language for nerds"
                  #:description "Lojban is an \"open source\" logical language built on predicate logic. Its grammar is unambiguous, logically constructed, and simple to learn. It has about 1300 root words from which sentences and compound works can be created. It’s a fun little toy language with 300-500 active learners across the globe. Lojban is so simple and easy, I have taught a number of people who were able to parse and understand complete sentences in just 1 hour."
                  #:artifacts (list (Link #f (u "https://gist.githubusercontent.com/kroucis/c1587dc09b5b9b33c880/raw/b792965f9eb17f1247ae96dd349119d67f03f4a0/lo%2520nu%2520tumfakli%27u")))
                  #:website #f
                  #:references
                  (list (Link #f           (u "Lojban.org"))
                        (Link "book"       (u "https://lojban.org/publications/cll/cll_v1.1_book.pdf"))
                        (Link "dictionary" (u "https://la-lojban.github.io/sutysisku/lojban/index.html"))))

               (T #:speaker (speaker speaker-id-grant-peret)
                  #:title "Cat Alley: Creation of an Entryway"
                  #:description "An overview of the aesthetic modelling, design, and loading requirements to build a cantilevered entry way sign."
                  #:artifacts (list (Link #f (u "https://github.com/RavenGrant/CatAlley")))
                  #:website #f
                  #:references
                  (list (Link "Atlas Obscura" (u "https://www.atlasobscura.com/places/cat-alley"))
                        (Link "Hidden New England" (u "https://hiddennewengland.com/2019/01/19/cat-alley-manchester-nh/"))))
               ))

      (M #:seq 2
         #:format 'show-and-tell
         #:codename "Tuna Pizza"
         #:date (d 2022 03 10)
         #:time (t 18 00)
         #:host (host host-id-raven-labs)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "https://forms.gle/nYPmUnhkDEro9Nft8")
         #:talks
         (list (T #:speaker (speaker speaker-id-siraaj-khandkar)
                  #:title "gg: the gitter of gits"
                  #:description "A tool to locate, compare and cross-reference all your git repositories accross machines."
                  #:artifacts (list (Link #f (u "https://github.com/xandkar/gg/")))
                  #:website #f
                  #:references '())
               (T #:speaker (speaker speaker-id-jeff-nelson)
                  #:title "Pi Bramble"
                  #:description "A cluster or Rasberry Pi's managed by kubernetes to deploy a drupal website."
                  #:artifacts '()
                  #:website #f
                  #:references
                  (list
                    (Link #f (u "https://github.com/geerlingguy/raspberry-pi-dramble"))
                    (Link #f (u "https://www.pidramble.com/"))))
               (T #:speaker (speaker speaker-id-brian-gray)
                  #:title ""
                  #:description ""
                  #:artifacts '()
                  #:website #f
                  #:references '())
               (T #:speaker (speaker speaker-id-grant-peret)
                  #:title ""
                  #:description ""
                  #:artifacts '()
                  #:website #f
                  #:references '())
               (T #:speaker (speaker speaker-id-brandon-simpson)
                  #:title "3 DOF Robotic Arm"
                  #:description "In this project, my team and I used a 3 DOF robotic manipulator and a USB webcam to implement an automated pick and place system. Through image processing, the system was able to detect and locate objects of a specific color. Using forward and inverse position and velocity kinematics, my team and Ideveloped a program to command a robotic arm to pick and place colored spheres until there were none remaining in the workspace. This system was also capable of sorting a specific non-spherical random object and able to dynamically track an object."
                  #:artifacts (list (Link #f (u "https://bgsimpson.wixsite.com/brandon/post/unified-robotics-iii")))
                  #:website #f
                  #:references '())
               (T #:speaker (speaker speaker-id-kyle-roucis)
                  #:title "KroucisVM"
                  #:description "A bytecode-driven dynamic-dispatched object-oriented so-many-hyphens virtual machine built in C and based on the Objective-C dynamic dispatch object model."
                  #:artifacts (list (Link #f (u "https://github.com/kroucis/KroucisVM")))
                  #:website #f
                  #:references '())
               (T #:speaker (speaker speaker-id-bob-peret)
                  #:title ""
                  #:description ""
                  #:artifacts '()
                  #:website #f
                  #:references '())
               ))

      (M #:seq 3
         #:format 'show-and-tell
         #:codename "The Sign"
         #:date (d 2022 04 07)
         #:time (t 18 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "https://forms.gle/uwTZM4gcWc6RcQyq6")
         #:talks
         (list
           (T #:speaker (speaker speaker-id-siraaj-khandkar)
              #:title "probe-me - a self port scanning service"
              #:description "A generalized prototype for a Helium hotspot monitoring service, which started as an exercise of writing an HTTP server in Racket from scratch."
              #:artifacts (list (Link #f (u "https://github.com/xandkar/probe-me")))
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-kyle-robertson)
              #:title "GPT-2: An outdated language generation model"
              #:description "A fun hack using the GPT-2 natural language generation neural network to automate generating marketing content for my wife's job"
              #:artifacts (list (Link #f (u "https://github.com/kwrobert/scarlett")))
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-thaddeus-hughes)
              #:title "EveryCalc"
              #:description "Online analysis tools for common tasks"
              #:artifacts (list (Link #f (u "https://github.com/Thaddeus-Maximus/everycalc")))
              #:website (u "http://everycalc.thadhughes.xyz/")
              #:references '())

           (T #:speaker (speaker speaker-id-kevin-kadow)
              #:title ""
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-jake)
              #:title "Walkie-textie over LoRa"
              #:description "Have you ever gone sky-diving with your friends, only to be disappointed that you can't text them mid-air? Jake's got you covered!"
              #:artifacts '()
              #:website #f
              #:references (list (Link #f (u "https://meshtastic.org/"))
                                 (Link #f (u "https://en.wikipedia.org/wiki/LoRa"))))

           (T #:speaker (speaker speaker-id-grant-peret)
              #:title "Surgical battery-repair"
              #:description "Live dangerously - replace your own battery cells!"
              #:artifacts '()
              #:website #f
              #:references '())
           ))

      (M #:seq 4
         #:format 'problem-share
         #:codename "Three Musketeers"
         #:date (d 2022 05 12)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "https://forms.gle/kU5bYdTi1im8bdy4A")
         #:talks
         (list
           (T #:speaker (speaker speaker-id-kevin-kadow)
              #:title "Resetting a disposable safety device."
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-kyle-roucis)
              #:title "Online game multiplayer matching engine."
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-siraaj-khandkar)
              #:title "Parallelizing blockchain deserialization."
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           ))

      (M #:seq 5
         #:format 'show-and-tell
         #:codename "Autolace"
         #:date (d 2022 06 09)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "https://forms.gle/TweJ2J23A7KGkRFCA")
         #:talks
         (list
           (T #:speaker (speaker speaker-id-kyle-roucis)
              #:title "A game written in experimental style of C - functional!"
              #:description "Every structure update, instead of mutation would create a new copy of the structure. It worked surprisingly well!"
              #:artifacts (list (Link #f (u "https://github.com/kroucis/stupid-pong")))
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-kyle-robertson)
              #:title "Data center efficiency optimization."
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-bill-the-shoelace-hater)
              #:title "Autolace"
              #:description "Bill loves shoes with laces, but hates tying them, so he designed and 3d-printed a solution - see photos!"
              #:artifacts '()
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-siraaj-khandkar)
              #:title "tt: a more-unixy twtxt client"
              #:description "twtxt is a dead-simple, peer-to-peer micro-blogging network/protocol. tt is a client which supports multiple peer lists and crawling to discover new peers."
              #:artifacts (list (Link #f (u "https://github.com/xandkar/tt")))
              #:website #f
              #:references (list (Link "tt-discovered peer directory" (u "https://xandkar.net/twtxt/peers/"))
                                 (Link "tt-collected nick usage stats" (u "https://xandkar.net/twtxt/nicks/"))
                                 (Link "the original twtxt client" (u "https://github.com/buckket/twtxt"))
                                 ))
           ))

      (M #:seq 6
         #:format 'show-and-tell
         #:codename "7"
         #:date (d 2022 07 07)
         #:time (t 19 00)
         #:host (host host-id-raven-labs)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "https://forms.gle/HsnNpgkpRUdKSXvk9")
         #:talks
         (list
           (T #:speaker (speaker speaker-id-jeff-nelson)
              #:title "Challanges of an energy-efficient, mobile location tracker"
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-grant-peret)
              #:title "Challanges of an energy-efficient, mobile location tracker"
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-perry-e-metzger)
              #:title "Radio hacking"
              #:description ""
              #:artifacts '()
              #:website #f
              #:references (list (Link #f (u "https://www.usenix.org/legacy/event/sec11/tech/full_papers/Clark.pdf"))))
           (T #:speaker (speaker speaker-id-jonathan-hales)
              #:title "Machine Learning for Drug Discovery"
              #:description ""
              #:artifacts '()
              #:website #f
              #:references (list (Link #f (u "https://en.wikipedia.org/wiki/Simplified_molecular-input_line-entry_system"))
                                 (Link #f (u "https://postera.ai/"))))
           (T #:speaker (speaker speaker-id-ian-mac)
              #:title "Combinatorial optimization with quantum computers"
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-max-sklar)
              #:title "Beta-distribution Analysis"
              #:description ""
              #:artifacts (list (Link #f (u "https://github.com/maxsklar/BayesPy/blob/master/DirichletLogisticRegression/findDirichletLogisticModel.py")))
              #:website #f
              #:references '())
           (T #:speaker (speaker speaker-id-thaddeus-hughes)
              #:title "Maslow CNC Router"
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())
           ))

      (M #:seq 7
         #:format 'show-and-tell
         #:codename "Swerve"
         #:date (d 2022 08 04)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "")
         #:talks
         '())

      (M #:seq 8
         #:format 'show-and-tell
         #:codename "Think and Swim"
         #:date (d 2022 09 01)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "")
         #:talks
         (list
           (T #:speaker (speaker speaker-id-kyle-roucis)
              #:title "Porting Swift code to Android"
              #:description
              "How I adapted a game written in Swift to run on Android with no chnages to the game engine."
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-siraaj-khandkar)
              #:title "Solana programming model"
              #:description
              "Intro to Solana programming model and demo of some minimal smart contracts."
              #:artifacts
              (list
                (Link #f (u "https://github.com/xandkar/solana-hello-world"))
                (Link #f (u "https://github.com/xandkar/solana-ping-pong"))
                (Link #f (u "https://github.com/xandkar/solana-echo-dyn"))
                (Link #f (u "https://github.com/xandkar/solana-echo-fixed")))
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-nick-FreakyNobleGas)
              #:title "File synchronization"
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-lam)
              #:title ""
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-jim-roucis)
              #:title "thinkorswim plugins"
              #:description "Building custom plugins for thinkorswim in ThinkScript to analyze stock market trends."
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-jared)
              #:title ""
              #:description ""
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-kyle-robertson)
              #:title "Data pipeline experiments"
              #:description "Trying to talk about building a data platform purely on Kubernetes but ended up chatting about my home lab network topology and routing setup I hacked together to play around with data platform ideas."
              #:artifacts '()
              #:website #f
              #:references '())
           ))

      (M #:seq 9
         #:format 'meet-and-greet
         #:codename "Modem"
         #:date (d 2022 10 06)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "")
         #:talks
         '())

      (M #:seq 10
         #:format 'meet-and-greet
         #:codename "Sake"
         #:date (d 2022 11 03)
         #:time (t 19 00)
         ; FIXME We were actually in Kisaki, not Makerspace,
         ;       but does that count as proper hosting?
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "")
         #:talks
         '())

      (M #:seq 11
         #:format 'meet-and-greet
         #:codename "AoC Kickoff"
         #:date (d 2022 12 01)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "")
         #:talks
         '())

      (M #:seq 12
         #:format 'show-and-tell
         #:codename "Siraaj Needs a Break"
         #:date (d 2023 02 02)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-kyle-roucis)
         #:registration-url (u "")
         #:talks
         (list
           (T #:speaker (speaker speaker-id-tigran-khandkar)
              #:title "Item and Effect Creation in Minecraft"
              #:description "Custom super-powerful items in Minecraft using command scripts and scoreboards."
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-kevin-kadow)
              #:title "Manchester Makerspace's Custom Doorbell"
              #:description "A networked and remote doorbell and surveillance system for the Manchester Makerspace."
              #:artifacts '()
              #:website #f
              #:references (list (Link #f (u "https://en.wikipedia.org/wiki/ESP32"))))

           (T #:speaker (speaker speaker-id-lisa)
              #:title "PiHole and self-hosted DNS"
              #:description "Using PiHole to control DNS resolution, monitoring ads and tracking requests from apps as well as potentially blacklisting sites and providing fine-grained parental controls."
              #:artifacts '()
              #:website #f
              #:references (list (Link #f (u "https://pi-hole.net/"))))

           (T #:speaker (speaker speaker-id-kyle-roucis)
              #:title "Unreal's Blueprint visual scripting"
              #:description "Visual scripting's sordid history and Epic's bizarre but seemingly successful creation and adoption of its Blueprints visual scripting system."
              #:artifacts '()
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-jack-jutzi)
              #:title "Rust Web Service from Scratch"
              #:description "A learning email newsletter and web service written from scratch in the Rust programming language."
              #:artifacts (list (Link #f (u "https://github.com/jsjutzi/rust-zero-backend")))
              #:website #f
              #:references '())

           (T #:speaker (speaker speaker-id-daniel-krol)
              #:title "Easy Self-hosted Maps."
              #:description "Using Sandstorm to host your own Google Maps replacement with collaborative trip planning and more!"
              #:artifacts (list (Link #f (u "https://github.com/orblivion/sandstorm-share-a-map")))
              #:website #f
              #:references (list (Link #f (u "https://sandstorm.io/"))))
           ))

      (M #:seq 13
         #:format 'show-and-tell
         #:codename "Arecibo"
         #:date (d 2023 03 02)
         #:time (t 19 00)
         #:host (host host-id-manchester-makerspace)
         #:organizer (speaker speaker-id-kyle-roucis)
         #:registration-url (u "")
         #:talks (list
                   (T #:speaker (speaker speaker-id-kyle-roucis)
                      #:title "Quick game with Unreal Blueprints"
                      #:description "Kyle's latest 1-day game creation using Unreal Engine's Blueprint Visual Scripting system."
                      #:artifacts '()
                      #:website #f
                      #:references (list (Link #f (u "https://en.wikipedia.org/wiki/Unreal_Engine"))
                                         (Link #f (u "https://docs.unrealengine.com/5.0/en-US/blueprints-visual-scripting-in-unreal-engine/"))))

                   (T #:speaker (speaker speaker-id-meg)
                      #:title "3D 2048 game with a twist"
                      #:description "Meg realized that plain old 2048 is boring, so the natural solution is to make it 3d and only movable by a ball. Implemented in C# and Unity game engine."
                      #:artifacts (list (Link #f (u "https://github.com/megtruesdale/2048Game")))
                      #:website #f
                      #:references (list (Link #f (u "https://en.wikipedia.org/wiki/2048_(video_game)"))
                                         (Link #f (u "https://en.wikipedia.org/wiki/Unity_(game_engine)"))))

                   (T #:speaker (speaker speaker-id-wayne)
                      #:title "Remote shell via Twitter and aNotepad"
                      #:description "Wayne was fed up with corporate firewalls blocking port 22 and preventing good old ssh, so he decided to Hack The Planet and send commands via Twitter (a local bot listens for tweets and executes them as commands, forwarding the output to an internet pasting service), but then Twitter closed their API, so ... he wrote his own Twitter clone!"
                      #:artifacts (list (Link #f (u "https://github.com/LogicPy/Python/tree/master/TORB")))
                      #:website #f
                      #:references (list (Link #f (u "https://anotepad.com/"))
                                         (Link #f (u "https://en.wikipedia.org/wiki/Hackers_(film)"))))

                   (T #:speaker (speaker speaker-id-siraaj-khandkar)
                      #:title "Epidemic simulation"
                      #:description "Siraaj was as excited and bored as anyone else during the 2020 COVID-19 quarantine, but also curious and hungry for experimentation, so set-out to write an epidemic simulator (and learn C for good measure). Sadly only the cellular automata version was implemented and agent mobility is in its 3rd year as a rusting TODO item."
                      #:artifacts (list (Link #f (u "https://github.com/xandkar/epidemic-sim")))
                      #:website #f
                      #:references (list (Link #f (u "https://en.wikipedia.org/wiki/Forest-fire_model"))))
                   ))

      (M #:seq 14
         #:format 'talk
         #:codename "INVITED TALK - The Essence of OOP"
         #:date (d 2023 03 13)
         #:time (t 18 30)
         #:host (host host-id-red-oak-hanover)
         #:organizer (speaker speaker-id-siraaj-khandkar)
         #:registration-url (u "https://www.meetup.com/hack-free-or-die/events/291843757")
         #:talks (list (T #:speaker (speaker speaker-id-fare)
                          #:title "The essence of OOP: Prototype OO in two functions"
                          #:description "Francois-Rene Rideau (a.k.a. [`@ngnghm`](https://twitter.com/ngnghm), [`@phanaero`](https://twitter.com/phanaero)) will share his deep insights into the essence of OOP: Prototype OO in two functions.

François-René \"Faré\" Rideau has been making programming languages and distributed systems usable for 25 years. Alumnus of the École Normale Supérieure, Former Senior Engineer at ITA Software, he also worked at Google and Bridgewater Associates. While working in the industry, he notably maintained and rewrote ASDF, the build system at the heart of the Common Lisp open source community; he also kept publishing academic papers and speaking at programming language conferences; early in his career, he even proved in Coq the correctness of a (centralized) payment protocol. Eventually, his interests in economics and software security converged with his experience in open source software and formal methods and he started working on Layer 2 solutions for the Blockchain. Since January 2018, he has made plenty of mistakes as co-founder of startups."
                          #:artifacts '()
                          #:website #f
                          #:references '())))

      )))

(define hosts
  (map host
       (remove-duplicates (map (compose Host-id Meeting-host)
                               meetings))))

(define speakers
  (map speaker
       (remove-duplicates (map (compose Speaker-id Talk-speaker)
                               (append* (map Meeting-talks meetings))))))

;; TODO Something nicer than (meeting-id talk) pair? TalkInfo? MeetingInfo?
(define/contract speaker->talks
  (-> Speaker? (listof (cons/c integer? Talk?)))
  (let ([s2t (foldl
               (λ (mt s2t)
                  (define t (cdr mt))
                  (define sid (Speaker-id (Talk-speaker t)))
                  (hash-update s2t sid (λ (mts) (cons mt mts)) '()))
               (hash)
               (append* (map (λ (m)
                                (map (λ (t) (cons (Meeting-seq m) t))
                                     (Meeting-talks m)))
                             meetings)))])
    (λ (s) (hash-ref s2t (Speaker-id s)))))  ; XXX not found => not a speaker

(define/contract speaker->meetings-organized
  (-> Speaker? (listof Meeting?))
  (let ([speaker2meetings-organized
          (foldl
            (λ (m speaker2meetings-organized)
               (hash-update speaker2meetings-organized
                            (Speaker-id (Meeting-organizer m))
                            (λ (meetings)
                               (cons m meetings))
                            '()))
            (hash)
            meetings)])
    (λ (s)
       (hash-ref speaker2meetings-organized (Speaker-id s) '()))))

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
  (meetings-filter-by-date g:date>=?))

(define/contract meeting-next
  (or/c #f Meeting?)
  (match meetings-future
    ['() #f]
    [ms
      (first (sort ms (λ (m1 m2) (g:date<? (Meeting-date m1)
                                           (Meeting-date m2)))))]))
