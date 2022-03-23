#lang racket

(provide (struct-out File)
         (contract-out
           [web-files   (-> (listof File?))]
           [email-files (-> (listof File?))]))

(require (prefix-in draw: racket/draw)
         (prefix-in pic: pict)
         (prefix-in sha: file/sha1)
         (prefix-in url: net/url)
         (prefix-in xml: xml))

(require (prefix-in g: gregor)
         (prefix-in md: markdown)
         (prefix-in txt: text-block/text))

(require (prefix-in data:  "data.rkt")
         "model.rkt")

(struct/contract File
                 ([path path?]
                  [content (or/c bytes? string?)])) ; TODO Restrict to bytes only?

(struct/contract Page
                 ([id string?]
                  [path path-string?]
                  [title string?]
                  [content (listof xml:xexpr/c)]
                  [deps (listof File?)]))

(define (P #:id id
           #:path path
           #:title title
           #:content content
           #:deps deps)
  (Page id path title content deps))

(define/contract (obj->path data)
  (-> bytes? path?)
  (define digest (sha:bytes->hex-string (sha256-bytes data)))
  (build-path "/" "_obj" (substring digest 0 2) digest))

(define/contract (obj->file data)
  (-> bytes? File?)
  (File (obj->path data) data))

(define/contract (lib file)
  (-> path-string? File?)
  (obj->file (file->bytes (build-path "view" "web" "lib" file))))

(define/contract file-bootstrap-css File? (lib "bs/css/bootstrap.min.css"))
(define/contract file-bootstrap-js  File? (lib "bs/js/bootstrap.bundle.min.js"))
(define/contract file-local-css     File? (lib "style.css"))

(define/contract path-bootstrap-css path-string? (path->string (File-path file-bootstrap-css)))
(define/contract path-bootstrap-js  path-string? (path->string (File-path file-bootstrap-js)))
(define/contract path-local-css     path-string? (path->string (File-path file-local-css)))
(define/contract path-home          path-string? "/")
(define/contract path-hosts         path-string? "/hosts")
(define/contract path-meetings      path-string? "/meetings")
(define/contract path-speakers      path-string? "/speakers")
(define/contract (path-meeting id)
  (-> integer? path-string?)
  (path->string (build-path path-meetings (~a id))))
(define/contract (path-host h)
  (-> Host? path-string?)
  (path->string (build-path path-hosts (Host-id h))))
(define/contract (path-speaker s)
  (-> Speaker? path-string?)
  (path->string (build-path path-speakers (Speaker-id s))))

(define/contract (inc file)
  (-> path-string? string?)
  (define path (build-path "view" "web" "inc" file))
  (string-join (file->lines path) "")) ; XXX Discarding newlines.

(define/contract (page-speaker s)
  (-> Speaker? Page?)
  (define (u->l u) (Link #f u))
  (define email-file
    (if (Speaker-email-show? s)
        (email->file (Speaker-email s))
        #f))
  (define title (Speaker-name s))
  (P #:id title
     #:path (path-speaker s)
     #:title title
     #:deps (if email-file `(,email-file) '())
     #:content
     `((h1 ,title)
       ,(if (Speaker-website s)
            (let ([u (url:url->string (Speaker-website s))])
              `(a ([href ,u]) ,u))
            "")
       ,(if email-file
            `(h5 ;([class "card-title text-center"])
              (img ([src ,(path->string (File-path email-file))])))
            "")
       ,(if (empty? (Speaker-affiliated-links s))
            ""
            `(p ([class "text-start"])
              (strong "links:")
              (ul ([class "text-start"])
                  ,@(links->list-items (map u->l (Speaker-affiliated-links s))))))
       (p ([class "text-start"])
          (strong "talks:")
          (ul ([class "text-start"])
              ,@(map (λ (meet-id-talk)
                        (match-define (cons mid t) meet-id-talk)
                        `(li (a ([href ,(path-meeting mid)]) ,(Talk-title t))))
                     (data:speaker->talks s))))
       ; TODO Maybe talks should have their own pages?
       )))

(define/contract (page-host h)
  (-> Host? Page?)
  (define a (Host-addr h))
  (define title (Host-name h))
  (define url (url:url->string (Host-url h)))
  (P #:id title
     #:path (path-host h)
     #:title title
     #:deps '()
     #:content
     `((h1 ,title)
       (a ([href ,url]) ,url)
       (p ,(Addr-building a)
          " "
          ,(Addr-street a)
          (br)
          ,@(if (not (string=? "" (Addr-room a))) `(,(Addr-room a) (br)) '(""))
          ,(Addr-town a)
          ", "
          ,(Addr-state a)
          " "
          ,(Addr-zipcode a))
       (p (iframe ([width "600"]
                   [height "450"]
                   [style "border:0"]
                   [loading "lazy"]
                   [allowfullscreen ""]
                   [src ,(url:url->string (Addr-google-maps-embed-url a))])))
       ; TODO List meetings held at this host.
       )))

; TODO page-plan with future meetings list: date/time, location, registration, etc.

(define/contract (page-meetings)
  (-> Page?)
  (define title "meetings")
  (P #:id title
     #:path path-meetings
     #:title title
     #:deps '()
     #:content
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
                            (td (a ([href ,(path-meeting (Meeting-seq m))]) ,(Meeting-codename m)))
                            (td (a ([href ,(url:url->string (Host-url h))]) ,(Host-name h)))))
                       (sort data:meetings-past
                             (λ (a b) (> (Meeting-seq a)
                                         (Meeting-seq b))))))))))

(define/contract (page-home)
  (-> Page?)
  (define next-meeting
    (match data:meeting-next
      [#f
        '((p ([class "lead"])
             "TBD"))]
      [m
        (let* ([date (g:~t (Meeting-date m) "EEEE, MMMM d, y")]
               [time (g:~t (Meeting-time m) "HH:mm")]
               [h (Meeting-host m)]
               [host-town (Addr-town (Host-addr h))]
               ; TODO Link to local info page about host/location?
               [host-link `(a ([href ,(path-host h)]) ,(Host-name h))])
          `((p ([class "lead"])
               ; TODO Google maps link
               ,date (br) ,time " at " ,host-link " in " ,host-town)
            (p ([class "lead"])
               (a ([class "btn btn-lg btn-secondary fw-bold border-white bg-white"]
                   [href ,(url:url->string (Meeting-registration-url m))])
                  "RSVP"))))]))
  (define id "home")
  (P #:id id
     #:path path-home
     #:title id
     #:deps '()
     #:content
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
          "If it is your first time - you " (em "have") " to present.")
       (p ([class "lead"])
          "The only qualification is that the work you present has to be open source - so that we can all study and learn from it.")
       (p ([class "lead"])
          "After the presentations we hangout and discuss whatever.")
       (p ([class "lead"])
          (a ([class "btn btn-lg btn-secondary fw-bold"]
              [href "https://discord.gg/NsEwkfCHgv"])
             "Discord"))
       (h3 "Next meeting")
       ,@next-meeting
       (script ,(inc "bs-enable-tooltips.js")))))

(define/contract (link->anchor l)
  (-> Link? xml:xexpr/c)
  (define name (Link-name l))
  (define url (url:url->string (Link-url l)))
  (let ([name (if name name url)])
    `(a ([href ,url]) ,name)))

(define/contract (links->list-items links)
  (-> (listof Link?) (listof xml:xexpr/c))
  (map (λ (l) `(li ,(link->anchor l))) links))

(define/contract (email->file e)
  (-> string? File?)
  ; TODO Tie the email font color to a theme variable somehow.
  (define color (make-object draw:color% 248 249 250))
  (define font 'modern)
  (define style (cons color font))
  (define size 13)
  (define content
    (with-output-to-bytes
      (thunk (send (pic:pict->bitmap (pic:text e style size))
                   save-file
                   (current-output-port)
                   'png))))
  (obj->file content))

;; TODO Pages for people - a.k.a. profiles.
;; TODO Pages for projects.
;; TODO Pages for hosts, with address, map and contact info.

(define/contract (page-meeting m)
  (-> Meeting? Page?)
  (define/contract (talk->card t)
    (-> Talk? xml:xexpr/c)
    (define s (Talk-speaker t))
    `(div ([class "card h-100 bg-dark text-light"])
      ;(img ([class "card-img-top"]
      ;      [src ""]
      ;      [alt ""]))
      (div ([class "card-header"])
           (h5 ([class "card-title text-center"])
               (a ([href ,(path-speaker  s)]) ,(Speaker-name s))))
      (div ([class "card-body"])
           (h5 ([class "card-title text-center"])
               ,(Talk-title t))
           (p ([class "card-text text-start"])
              ,(Talk-description t))
           ,(if (empty? (Talk-artifacts t))
                ""
                `(p ([class "card-text text-start"])
                  (strong "artifacts:")
                  (ul ([class "text-start"])
                      ,@(links->list-items (Talk-artifacts t)))))
           ,(if (empty? (Talk-references t))
                ""
                `(p ([class "card-text text-start"])
                  (strong "references:")
                  (ul ([class "text-start"])
                      ,@(links->list-items (Talk-references t))))))
      ;(div ([class "card-footer"]) "")
      ))
  (define photos (Meeting-photos m))
  (define h (Meeting-host m))
  (define content
    `((h1 ,(Meeting-codename m))
      (h6 ,(g:~t (Meeting-date m) "EEEE, MMMM d, y"))
      (h6 ,(g:~t (Meeting-time m) "HH:mm")
          " at "
          (a ([href ,(path-host h)]) ,(Host-name h))
          " in "
          ,(Addr-town (Host-addr h)))

      ,(if (empty? photos)
           ""
           `(div ([id "carouselExampleControls"]
                  [class "carousel slide"]
                  [data-bs-ride "carousel"])
             (div ([class "carousel-indicators"])
                  ,@(for/list ([i (in-naturals)]
                               [p photos])
                              `(button ([type "button"]
                                        [data-bs-target "#carouselExampleControls"]
                                        [data-bs-slide-to ,(number->string i)]
                                        [aria-label ,(format "Slide ~a" i)]
                                        ,@(if (= i 0)
                                              `([class "active"]
                                                [aria-current "true"])
                                              '())))))
             (div ([class "carousel-inner"])
                  ,@(for/list ([i (in-naturals)]
                               [p photos])
                              `(div ([class ,(if (= i 0)
                                                 "carousel-item active"
                                                 "carousel-item")])
                                (img ([src ,(path->string (File-path (obj->file (Photo-data p))))]
                                      [class "d-block w-100"]))
                                (div ([class "carousel-caption d-none d-md-block"])
                                     ,(Photo-caption p)))))
             (button ([class "carousel-control-prev"]
                      [type "button"]
                      [data-bs-target "#carouselExampleControls"]
                      [data-bs-slide "prev"])
                     (span ([class "carousel-control-prev-icon"]
                            [aria-hidden "true"]))
                     (span ([class "visually-hidden"])
                           "Previous"))
             (button ([class "carousel-control-next"]
                      [type "button"]
                      [data-bs-target "#carouselExampleControls"]
                      [data-bs-slide "next"])
                     (span ([class "carousel-control-next-icon"]
                            [aria-hidden "true"]))
                     (span ([class "visually-hidden"])
                           "Next"))))

      (div ([class "lead"])
           ,@(xexpr-insert-classes
               '((blockquote . "blockquote"))
               (md:parse-markdown (Meeting-recap m))))
      (div ([class "row row-cols-1 row-cols-md-1 g-4"])
           ,@(map (λ (c) `(div ([class "col"]) ,c))
                  (map talk->card (Meeting-talks m))))))
  (P #:id (format "meeting-~a" (number->string (Meeting-seq m)))
     #:path (path-meeting (Meeting-seq m))
     #:title (format "Meeting ~a: ~a" (Meeting-seq m) (Meeting-codename m))
     #:deps (map obj->file (map Photo-data photos))
     #:content content))

(define/contract (xexpr-insert-classes symb-class-pairs xs)
  (-> (listof (cons/c symbol? string?)) (listof xml:xexpr/c) (listof xml:xexpr/c))
  (foldl xexpr-insert-class xs symb-class-pairs))

;; TODO Generalize mapping x-expressions.
(define/contract (xexpr-insert-class pair xs)
  (-> (cons/c symbol? string?) (listof xml:xexpr/c) (listof xml:xexpr/c))
  (define attribute? (list/c symbol? string?))
  (define attributes? (listof attribute?))
  (match-define (cons sym class) pair)
  (define (insert x)
    (match x
      [(list* s as xs) #:when (and (symbol? s)
                                   (attributes? as))
       (let ([as (if (equal? s sym)
                     (cons `(class ,class) as)
                     as)]
             [xs (map insert xs)])
         (list* s as xs))]
      [x #:when (string? x) x]
      [x #:when (symbol? x) x]
      [x #:when (xml:cdata? x) x]
      [x #:when (xml:comment? x) x]
      [x #:when (xml:p-i? x) x]))
  (map insert xs))

(define/contract (assemble #:nav nav
                           #:title title
                           #:content content)
  (-> #:nav xml:xexpr/c
      #:title string?
      #:content (listof xml:xexpr/c)
      xml:xexpr/c)
  `(html (head (title ,title " @ Hack Free Or Die")
               (meta ([name "generator"]
                      [content "https://racket-lang.org/"]))
               (meta ([charset "utf-8"]))
               (meta ([name="viewport"]
                      [content "width=device-width, initial-scale=1"]))
               (meta ([name "description"]
                      [content ""]))
               (script ([src ,path-bootstrap-js]))
               (link ([href ,path-bootstrap-css]
                      [rel "stylesheet"]))
               (link ([href ,path-local-css]
                      [rel "stylesheet"])))
    (body ([class "d-flex h-100 text-center text-white bg-dark"])
          (div ([class "hfod-container d-flex w-100 h-100 p-3 mx-auto flex-column"])
               (header ([class "mb-auto"])
                       ; TODO Generate nav smarter.
                       (div
                         ; TODO Do we want this logoish thing here?
                         ;(h3 ([class "float-md-start mb-0"]) "hack && tell")
                         ,nav))
               (main ([class "px-3"])
                     ,@content)
               (footer ([class "mt-auto text-white-50"])
                       (p "Inspired by "
                          (a ([href "https://hackandtell.org/"]
                              [class "text-white"])
                             "NYC Hack && Tell")))))))

(define nav
  `(nav ([class "navbar navbar-expand-lg navbar-dark bg-dark float-md-end"])
    (div ([class "container-fluid"])

         (a ([class "navbar-brand"]
             [href ,path-home])
            "home")

         (button ([class "navbar-toggler"]
                  [type "button"]
                  [data-bs-toggle "collapse"]
                  [data-bs-target "#navbarNavDarkDropdown"]
                  [aria-controls "navbarNavDarkDropdown"]
                  [aria-expanded "false"]
                  [aria-label "Toggle navigation"])
                 (span ([class "navbar-toggler-icon"])))

         (div ([class "collapse navbar-collapse"]
               [id "navbarNavDarkDropdown"])

              (ul ([class "navbar-nav"])

                  ; Meetings
                  (li ([class "nav-item dropdown"])
                      (a ([class "nav-link dropdown-toggle"]
                          [href ,path-meetings]
                          [id "navbarDarkDropdownMenuLink"]
                          [role "button"]
                          [data-bs-toggle "dropdown"]
                          [aria-expanded "false"])
                         "meetings")

                      (ul ([class "dropdown-menu dropdown-menu-dark"]
                           [aria-labelledby "navbarDarkDropdownMenuLink"])
                          ,@(map (λ (m)
                                    `(li (a ([class "dropdown-item"]
                                             [href ,(path-meeting (Meeting-seq m))])
                                            ,(let ([date (g:~t (Meeting-date m)
                                                               "yyyy-MMM-dd")]
                                                   [name (Meeting-codename m)])
                                               (format "~a : ~a" date name)))))
                                 (sort data:meetings-past
                                       (λ (a b) (> (Meeting-seq a)
                                                   (Meeting-seq b)))))))

                  ; Hosts
                  (li ([class "nav-item dropdown"])
                      (a ([class "nav-link dropdown-toggle"]
                          [href "#"]
                          [id "navbarDarkDropdownMenuLink"]
                          [role "button"]
                          [data-bs-toggle "dropdown"]
                          [aria-expanded "false"])
                         "hosts")

                      (ul ([class "dropdown-menu dropdown-menu-dark"]
                           [aria-labelledby "navbarDarkDropdownMenuLink"])
                          ,@(map (λ (h)
                                    `(li (a ([class "dropdown-item"]
                                             [href ,(path-host h)])
                                            ,(Host-name h))))
                                 (sort data:hosts
                                       (λ (a b) (string>? (Host-name a)
                                                          (Host-name b)))))))

                  ;; Speakers
                  ;(li ([class "nav-item dropdown"])
                  ;    (a ([class "nav-link dropdown-toggle"]
                  ;        [href "#"]
                  ;        [id "navbarDarkDropdownMenuLink"]
                  ;        [role "button"]
                  ;        [data-bs-toggle "dropdown"]
                  ;        [aria-expanded "false"])
                  ;       "speakers")

                  ;    (ul ([class "dropdown-menu dropdown-menu-dark"]
                  ;         [aria-labelledby "navbarDarkDropdownMenuLink"])
                  ;        ,@(map (λ (s)
                  ;                  `(li (a ([class "dropdown-item"]
                  ;                           [href ,(path-speaker s)])
                  ;                          ,(Speaker-name s))))
                  ;               (sort data:speakers
                  ;                     (λ (a b) (string<? (Speaker-name a)
                  ;                                        (Speaker-name b)))))))
                  )))))

(define/contract (web-files)
  (-> (listof File?))
  (append*
    ; TODO Refactor such that list of runtime lib files are expressed as deps of assemble.
    (cons (list file-bootstrap-css
                file-bootstrap-js
                file-local-css)
          (map (λ (p)
                  (define page-file
                    (File (build-path (Page-path p) "index.html")
                          (xml:xexpr->string (assemble #:nav nav
                                                       #:title (Page-title p)
                                                       #:content (Page-content p)))))
                  (define dep-files (Page-deps p))
                  (cons page-file dep-files))
               (append (list (page-home))
                       (list (page-meetings))
                       (map page-meeting data:meetings-past)
                       (map page-host data:hosts)
                       (map page-speaker data:speakers)
                       )))))

;; TODO email-meeting-invite
;; TODO email-meeting-announce
;; TODO email-meeting-remind

(define/contract (email-meeting-recap m)
  (-> Meeting? File?)
  (define h (Meeting-host m))
  (define talks (Meeting-talks m))
  (define (talk->pres-name-email t)
    (define s (Talk-speaker t))
    (format "~a <~a>"
            (Speaker-name s)
            (Speaker-email s)))
  (define recipients
    ; TODO Add attendees in addition to speakers, but they must be modeled first.
    (string-join (map talk->pres-name-email talks) ","))
  (define headers
    (string-join
      (list (format "To: ~a" recipients)
            (format "Subject: [hfod] Meeting ~a recap" (Meeting-seq m)))
      "\n"))
  (define title-paragraph
    (string-join
      `(,(format "# Meeting ~a: ~a"
                 (Meeting-seq m)
                 (Meeting-codename m))
        ,(format "Held on ~a, ~a at ~a in ~a"
                 (g:~t (Meeting-date m) "EEEE, MMMM d, y")
                 (g:~t (Meeting-time m) "HH:mm")
                 (Host-name h)
                 (Addr-town (Host-addr h)))
        ,(url:url->string (Host-url h)))
      "\n"))
  (define paragraphs
    (list*
      title-paragraph
      "## Recap"
      (Meeting-recap m)
      "## Presentations"
      (map (λ (t)
              (string-join
                (list* (format "### ~a" (Talk-title t))
                       (format "by ~a" (talk->pres-name-email t))
                       (txt:text->lines (Talk-description t) 72))
                "\n"))
           talks)))
  (define body
    (string-join paragraphs "\n\n"))
  (File (string->path (format "meeting-recap-~a.eml" (Meeting-seq m)))
        (format "~a~n~n~a" headers body)))

(define/contract (email-files)
  (-> (listof File?))
  (map email-meeting-recap data:meetings-past))
