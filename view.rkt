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
                 ([path path-string?]
                  [content (or/c bytes? string?)])) ; TODO Restrict to bytes only?

(struct/contract Page
                 ([id string?]
                  [title string?]
                  [content (listof xml:xexpr/c)]
                  [deps (listof File?)]))

(define (P #:id id
           #:title title
           #:content content
           #:deps deps)
  (Page id title content deps))

(define/contract (obj->path data)
  (-> bytes? path?)
  (define digest (sha:bytes->hex-string (sha256-bytes data)))
  (build-path "_obj/" (substring digest 0 2) digest))

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

(define/contract (inc file)
  (-> path-string? string?)
  (define path (build-path "view" "web" "inc" file))
  (string-join (file->lines path) "")) ; XXX Discarding newlines.

; TODO page-plan with future meetings list: date/time, location, registration, etc.

(define/contract (page-log)
  (-> Page?)
  (define title "log")
  (P #:id title
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
                            (td (a ([href ,(format "meeting-~a.html" (Meeting-seq m))]) ,(Meeting-codename m)))
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
               [host-link `(a ([href ,(url:url->string (Host-url h))]) ,(Host-name h))])
          `((p ([class "lead"])
               ; TODO Google maps link
               ,date (br) ,time " at " ,host-link " in " ,host-town)
            (p ([class "lead"])
               (a ([class "btn btn-lg btn-secondary fw-bold border-white bg-white"]
                   [href ,(url:url->string (Meeting-registration-url m))])
                  "Register"))))]))
  (define id "home")
  (P #:id id
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
      (thunk (send (pic:pict->bitmap (pic:text (format "<~a>" e) style size))
                   save-file
                   (current-output-port)
                   'png))))
  (obj->file content))

(define/contract (page-meeting m)
  (-> Meeting? Page?)
  (define/contract email-addr-image-files
    (listof File?)
    '())
  ; TODO Refactor to remove mutation of email-addr-image-files?
  (define (email->path e)
    (define f (email->file e))
    (set! email-addr-image-files (cons f email-addr-image-files))
    (path->string (File-path f)))
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
              " " (img ([src ,(email->path (Presenter-email p))])))
           ; XXX "lead" seems semantically not ideal here, but seems to work OK.
           (p  ([class "card-text text-start lead"])
              ,(Talk-description t))
           ,(if (empty? (Talk-artifacts t))
                ""
                `(p  ([class "card-text text-start"])
                  (strong "artifacts:")
                  (ul ([class "text-start"])
                      ,@(links->list-items (Talk-artifacts t)))))
           ,(if (empty? (Talk-references t))
                ""
                `(p  ([class "card-text text-start"])
                  (strong "references:")
                  (ul ([class "text-start"])
                      ,@(links->list-items (Talk-references t))))))
      ;(div ([class "card-footer"]) "")
      ))
  (define photo-files (map obj->file (Meeting-photos m)))
  (define h (Meeting-host m))
  (define content
    `((h1 ,(Meeting-codename m))
      (h6 ,(g:~t (Meeting-date m) "EEEE, MMMM d, y"))
      (h6 ,(g:~t (Meeting-time m) "HH:mm")
          " at "
          (a ([href ,(url:url->string (Host-url h))]) ,(Host-name h))
          " in "
          ,(Addr-town (Host-addr h)))

      ,(if (empty? photo-files)
           ""
           `(div ([id "carouselExampleControls"]
                  [class "carousel slide"]
                  [data-bs-ride "carousel"])
             (div ([class "carousel-inner"])
                  ,@(for/list ([i (in-naturals)]
                               [p photo-files])
                              `(div ([class ,(if (= i 0)
                                                 "carousel-item active"
                                                 "carousel-item")])
                                (img ([src ,(path->string (File-path p))]
                                      [class "d-block w-100"])))))
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

      (div ()
           ; TODO Do something more aesthetic with the blockquote presentation.
           ,@(xexpr-insert-class 'blockquote "blockquote" (md:parse-markdown (Meeting-recap m))))
      (div ([class "row row-cols-1 row-cols-md-1 g-4"])
           ,@(map (λ (c) `(div ([class "col"]) ,c))
                  (map talk->card (Meeting-talks m))))))
  ; XXX content mutates email-addr-image-files, so must be called before using it.
  (P #:id (format "meeting-~a" (number->string (Meeting-seq m)))
     #:title (format "Meeting ~a: ~a" (Meeting-seq m) (Meeting-codename m))
     #:deps (append email-addr-image-files
                    photo-files)
     #:content content))

;; TODO Generalize mapping x-expressions.
(define/contract (xexpr-insert-class sym class xs)
  (-> symbol? string? (listof xml:xexpr/c) (listof xml:xexpr/c))
  (define attribute? (list/c symbol? string?))
  (define attributes? (listof attribute?))
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
             [href "index.html"])
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
                  (li ([class "nav-item dropdown"])
                      (a ([class "nav-link dropdown-toggle"]
                          [href "#"]
                          [id "navbarDarkDropdownMenuLink"]
                          [role "button"]
                          [data-bs-toggle "dropdown"]
                          [aria-expanded "false"])
                         "log")

                      (ul ([class "dropdown-menu dropdown-menu-dark"]
                           [aria-labelledby "navbarDarkDropdownMenuLink"])
                          ,@(map (λ (m)
                                    `(li (a ([class "dropdown-item"]
                                             [href ,(format "meeting-~a.html" (Meeting-seq m))])
                                            ,(format "~a: ~a" (Meeting-seq m) (Meeting-codename m)))))
                                 (sort data:meetings-past
                                       (λ (a b) (> (Meeting-seq a)
                                                   (Meeting-seq b))))))))))))

(define (page-id->filename id)
  (define name (match id
                 ["home" "index"]
                 [_ id]))
  (format "~a.html" name))

(define/contract (web-files)
  (-> (listof File?))
  (append*
    ; TODO Refactor such that list of runtime lib files are expressed as deps of assemble.
    (cons (list file-bootstrap-css
                file-bootstrap-js
                file-local-css)
          (map (λ (p)
                  (define page-file
                    (File (page-id->filename (Page-id p))
                          (xml:xexpr->string (assemble #:nav nav
                                                       #:title (Page-title p)
                                                       #:content (Page-content p)))))
                  (define dep-files (Page-deps p))
                  (cons page-file dep-files))
               (list* (page-home)
                      (page-log)
                      (map page-meeting data:meetings-past))))))

;; TODO email-meeting-invite
;; TODO email-meeting-announce
;; TODO email-meeting-remind

(define/contract (email-meeting-recap m)
  (-> Meeting? File?)
  (define h (Meeting-host m))
  (define talks (Meeting-talks m))
  (define (talk->pres-name-email t)
    (define p (Talk-presenter t))
    (format "~a <~a>"
            (Presenter-name p)
            (Presenter-email p)))
  (define recipients
    ; TODO Add attendees in addition to presenters, but they must be modeled first.
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
  (File (format "meeting-recap-~a.eml" (Meeting-seq m))
        (format "~a~n~n~a" headers body)))

(define/contract (email-files)
  (-> (listof File?))
  (map email-meeting-recap data:meetings-past))
