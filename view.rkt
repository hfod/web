#lang racket

(provide web-files
         File?
         File-path
         File-content)

(require (prefix-in pic: pict)
         (prefix-in sha: file/sha1)
         (prefix-in url: net/url)
         (prefix-in xml: xml))

(require (prefix-in g: gregor))

(require (prefix-in model: "model.rkt"))

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

(define/contract path-bootstrap-css path-string? "_lib/bs/css/bootstrap.min.css")
(define/contract path-bootstrap-js  path-string? "_lib/bs/js/bootstrap.bundle.min.js")
(define/contract path-local-css     path-string? "_lib/style.css")
(define/contract path-images        path-string? "_data/img")    ; Non-photo images
(define/contract path-photos        path-string? "_data/photos")

(define/contract (inc file)
  (-> path-string? string?)
  (string-join (file->lines (build-path "inc" file)) "")) ; XXX Discarding newlines.

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
                          (define h (model:Meeting-host m))
                          `(tr
                            (th ([scope "row"]) ,(number->string (model:Meeting-seq m)))
                            (td ,(g:~t (model:Meeting-date m) "yyyy MMM dd"))
                            (td (a ([href ,(format "meeting-~a.html" (model:Meeting-seq m))]) ,(model:Meeting-codename m)))
                            (td (a ([href ,(url:url->string (model:Host-url h))]) ,(model:Host-name h)))))
                       (sort model:meetings-past
                             (λ (a b) (> (model:Meeting-seq a)
                                         (model:Meeting-seq b))))))))))

(define/contract (page-home)
  (-> Page?)
  (define next-meeting
    (match model:meeting-next
      [#f ""] ; TODO A message that nothing is scheduled.
      [m
        (let* ([date (g:~t (model:Meeting-date m) "EEEE, MMMM d, y")]
               [time (g:~t (model:Meeting-time m) "HH:mm")]
               [h (model:Meeting-host m)]
               [host-town (model:Addr-town (model:Host-addr h))]
               ; TODO Link to local info page about host/location?
               [host-link `(a ([href ,(url:url->string (model:Host-url h))]) ,(model:Host-name h))])
          `((p ([class "lead"])
               ; TODO Google maps link
               ,date (br) ,time " at " ,host-link " in " ,host-town)
            (p ([class "lead"])
               (a ([class "btn btn-lg btn-secondary fw-bold border-white bg-white"]
                   [href ,(url:url->string (model:Meeting-registration-url m))])
                  "Join us"))))]))
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
       (h3 "Next meeting")
       ,@next-meeting
       (script ,(inc "bs-enable-tooltips.js")))))

(define/contract (link->anchor l)
  (-> model:Link? xml:xexpr/c)
  (define name (model:Link-name l))
  (define url (url:url->string (model:Link-url l)))
  (let ([name (if name name url)])
    `(a ([href ,url]) ,name)))

(define/contract (links->list-items links)
  (-> (listof model:Link?) (listof xml:xexpr/c))
  (map (λ (l) `(li ,(link->anchor l))) links))

(define/contract (email->file e)
  (-> string? File?)
  (define content
    (with-output-to-bytes
      (thunk (send (pic:pict->bitmap (pic:text e))
                   save-file
                   (current-output-port)
                   'png))))
  (define digest (sha:bytes->hex-string (sha256-bytes content)))
  (define name (string-append digest ".png"))
  (define path (build-path path-images name))
  (File path content))

(define/contract (page-meeting m)
  (-> model:Meeting? Page?)
  (define/contract email-addr-image-files
    (listof File?)
    (map (λ (t) (email->file (model:Presenter-email (model:Talk-presenter t))))
         (model:Meeting-talks m)))
  (define/contract (talk->card t)
    (-> model:Talk? xml:xexpr/c)
    (define p (model:Talk-presenter t))
    `(div ([class "card h-100 bg-dark text-light"])
      ;(img ([class "card-img-top"]
      ;      [src ""]
      ;      [alt ""]))
      (div ([class "card-header"])
           (h5 ([class "card-title text-center"]) ,(model:Talk-title t)))
      (div ([class "card-body"])
           (p  ([class "card-title text-center"])
              "by "
              ,(if (model:Presenter-website p)
                   `(a ([href ,(url:url->string (model:Presenter-website p))]) ,(model:Presenter-name p))
                   (model:Presenter-name p))
              ; TODO Tweak email colors to match site theme.
              ; TODO Insert email images.
              )
           ; XXX "lead" seems semantically not ideal here, but seems to work OK.
           (p  ([class "card-text text-start lead"])
              ,(model:Talk-description t))
           ,(if (empty? (model:Talk-artifacts t))
                ""
                `(p  ([class "card-text text-start"])
                  (strong "artifacts:")
                  (ul ([class "text-start"])
                      ,@(links->list-items (model:Talk-artifacts t)))))
           ,(if (empty? (model:Talk-references t))
                ""
                `(p  ([class "card-text text-start"])
                  (strong "references:")
                  (ul ([class "text-start"])
                      ,@(links->list-items (model:Talk-references t))))))
      ;(div ([class "card-footer"]) "")
      ))
  (define photo-paths
    (map
      (λ (filename)
         (build-path path-photos
                     "meetings"
                     (number->string (model:Meeting-seq m))
                     filename))
      (model:Meeting-photos m)))
  (P #:id (format "meeting-~a" (number->string (model:Meeting-seq m)))
     #:title (format "Meeting ~a: ~a" (model:Meeting-seq m) (model:Meeting-codename m))
     #:deps email-addr-image-files
     #:content
     `((h1 ,(model:Meeting-codename m))
       (h6 ,(g:~t (model:Meeting-date m) "EEEE, MMMM d, y"))

       ,(if (empty? photo-paths)
            ""
            `(div ([id "carouselExampleControls"]
                   [class "carousel slide"]
                   [data-bs-ride "carousel"])
              (div ([class "carousel-inner"])
                   ,@(for/list ([i (in-naturals)]
                                [p photo-paths])
                               `(div ([class ,(if (= i 0)
                                                  "carousel-item active"
                                                  "carousel-item")])
                                 (img ([src ,(path->string p)]
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

       (p ([class "lead"])
          ,(model:Meeting-recap m))
       (div ([class "row row-cols-1 row-cols-md-1 g-4"])
            ,@(map (λ (c) `(div ([class "col"]) ,c))
                   (map talk->card (model:Meeting-talks m)))))))

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
                                             [href ,(format "meeting-~a.html" (model:Meeting-seq m))])
                                            ,(format "~a: ~a" (model:Meeting-seq m) (model:Meeting-codename m)))))
                                 (sort model:meetings-past
                                       (λ (a b) (> (model:Meeting-seq a)
                                                   (model:Meeting-seq b))))))))))))

(define (page-id->filename id)
  (define name (match id
                 ["home" "index"]
                 [_ id]))
  (format "~a.html" name))

(define/contract (web-files)
  (-> (listof File?))
  (append*
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
                (map page-meeting model:meetings-past)))))
