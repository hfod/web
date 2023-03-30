#lang racket

(provide (struct-out Addr)
         (struct-out Host)
         (struct-out Meeting)
         MeetingFormat?
         (struct-out Photo)
         (struct-out Speaker)
         (struct-out Link)
         (struct-out Talk))

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
                  [country "USA"]
                  ; XXX Generating these URLs requires an API key
                  ;     and potentially costs, but making them manually is free.
                  [google-maps-embed-url url:url?]))

; TODO Rename to a more general "member"? else?
(struct/contract Speaker
                 ([id string?]
                  [name string?]
                  [email string?]
                  [email-show? boolean?]
                  [website (or/c #f url:url?)]
                  [affiliated-links (listof url:url?)]))

(struct/contract Host
                 ([id string?]
                  [name string?]
                  [addr Addr?]
                  [url url:url?]
                  [contact Speaker?]))
; TODO Replace Speaker? copies with references by Speaker-id.

(struct/contract Link
                 ([name (or/c #f string?)]
                  [url url:url?]))

(struct/contract Photo
                 ([data bytes?]
                  [caption string?]))

(struct/contract Talk
                 ([speaker Speaker?]
                  [title string?]
                  [description string?]
                  [website (or/c #f url:url?)]
                  [artifacts (listof Link?)] ; XXX We really should not allow this to be empty.
                  [references (listof Link?)]))

(define MeetingFormat? (or/c 'meet-and-greet
                             'show-and-tell
                             'problem-share
                             'talk))

(struct/contract Meeting
                 ; TODO Model attendees.
                 ([seq integer?]
                  [format MeetingFormat?]
                  [codename string?]
                  [date g:date?]
                  [time g:time?]
                  [host Host?]
                  [organizer Speaker?] ; HBIC of arrangements, promotion and logging.
                  [talks (listof Talk?)]
                  [recap string?]
                  [photos (listof Photo?)]
                  [registration-url url:url?]))
