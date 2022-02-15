#lang racket

(provide pages)

(require (prefix-in url: net/url)
         (prefix-in xml: xml))

(require (prefix-in g: gregor))

(require (prefix-in data: "data.rkt"))

(define/contract x-next-meeting
  xml:xexpr/c
  (match data:next-meeting
    [#f ""]
    [m
      (let* ([d (data:Meeting-date m)]
             [t (data:Meeting-time m)]
             [dt (g:datetime (g:->year d)
                             (g:->month d)
                             (g:->day d)
                             (g:->hours t)
                             (g:->minutes t))]
             [date (g:~t dt "EEEE, MMMM d, y")]
             [time (g:~t dt "HH:mm")]
             [h (data:Meeting-host m)]
             [host-town (data:Addr-town (data:Host-addr h))]
             ; TODO Link to local info page about host/location?
             [host-link `(a ([href ,(url:url->string (data:Host-url h))]) ,(data:Host-name h))])
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
                             (define h (data:Meeting-host m))
                             `(tr
                               (th ([scope "row"]) ,(number->string (data:Meeting-seq m)))
                               (td ,(g:~t (data:Meeting-date m) "yyyy MMM dd"))
                               (td (a ([href ,(format "meeting-~a.html" (data:Meeting-seq m))]) ,(data:Meeting-codename m)))
                               (td (a ([href ,(url:url->string (data:Host-url h))]) ,(data:Host-name h)))))
                          (sort data:meetings-past
                                (λ (a b) (> (data:Meeting-seq a)
                                            (data:Meeting-seq b))))))))))

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
         "If it is your first time - you " (em "have") " to present.")
      (p ([class "lead"])
         "The only qualification is that the work you present has to be open source - so that we can all study and learn from it.")
      (p ([class "lead"])
         "After the presentations we hangout and discuss whatever.")
      (h3 "Next meeting")
      ,x-next-meeting
      (p ([class "lead"])
         (a ([class "btn btn-lg btn-secondary fw-bold border-white bg-white"]
             [href "https://forms.gle/6hx8ujd56rtAuWwZ8"])
            "Join us"))
      ,enable-tooltips))
  (define title "home")
  (page #:nav-section title #:title title content))

(define/contract (talk->card t)
  (-> data:Talk? xml:xexpr/c)
  (define p (data:Talk-presenter t))
  `(div ([class "card h-100 bg-dark text-light"])
    ;(img ([class "card-img-top"]
    ;      [src ""]
    ;      [alt ""]))
    (div ([class "card-header"])
         (h5 ([class "card-title text-center"]) ,(data:Talk-title t)))
    (div ([class "card-body"])
         (p  ([class "card-title text-center"])
            "by "
            ,(if (data:Presenter-website p)
                 `(a ([href ,(url:url->string (data:Presenter-website p))]) ,(data:Presenter-name p))
                 (data:Presenter-name p))
            ; TODO Render email addr text as image:
            ;      (define email (data:Presenter-email p))
            ;      (define filename (string-append email ".png"))
            ;      (send (pict:pict->bitmap (pict:text email)) save-file filename 'png)
            ;      ; TODO Tweak colors to match site theme.
            )
         ; XXX "lead" seems semantically not ideal here, but seems to work OK.
         (p  ([class "card-text text-start lead"])
            ,(data:Talk-description t))
         ,(if (empty? (data:Talk-sources t))
              ""
              `(p  ([class "card-text text-start"])
                (strong "artifacts:") ; TODO Rename Talk-sources to Talk-artifacts?
                (ul ([class "text-start"])
                    ,@(map (λ (s)
                              (define url (url:url->string s))
                              `(li (a ([href ,url]) ,url)))
                           (data:Talk-sources t)))))
         ,(if (empty? (data:Talk-references t))
              ""
              `(p  ([class "card-text text-start"])
                (strong "references:")
                (ul ([class "text-start"])
                    ,@(map (λ (r)
                              `(li (a ([href ,(url:url->string (data:Ref-url r))]) ,(data:Ref-name r))))
                           (data:Talk-references t))))))
    ;(div ([class "card-footer"]) "")
    ))

(define/contract (page-meeting m)
  (-> data:Meeting? xml:xexpr/c)
  (define title (format "~a: ~a" (data:Meeting-seq m) (data:Meeting-codename m)))
  (define cards (map talk->card (data:Meeting-talks m)))
  (define cols (map (λ (c) `(div ([class "col"]) ,c)) cards))
  (page
    #:nav-section "log"
    #:title title
    `((h1 ,title)
      (p ([class "lead"])
         ,(data:Meeting-recap m))
      (div ([class "row row-cols-1 row-cols-md-1 g-4"]) ,@cols))))

(define/contract (pages)
  (-> (listof (cons/c path-string? xml:xexpr/c)))
  `(["index.html" . ,(page-home)]
    ["log.html"   . ,(page-log)]
    ,@(map (λ (m) `(,(format "meeting-~a.html" (data:Meeting-seq m)) . ,(page-meeting m))) data:meetings-past)))
