#lang slideshow

(require slideshow/play
         slideshow/code
         slideshow/latex
         (for-syntax syntax/stx))

; Library
(define (medium-text txt)
  (text txt (current-main-font) 50))

(define (large-text txt)
  (text txt (current-main-font) 62))

(define (massive-text txt)
  (text txt (current-main-font) 120))

(define (medium-$$ txt)
  (scale ($$ txt) 1.5))

(define (large-$$ txt)
  (scale ($$ txt) 2))

(define (massive-$$ txt)
  (scale ($$ txt) 3))

(define (title-slide . data)
  (play-n
   #:skip-last? #t
   (animate-slide
    'next
    'alts
    `(,data ()))))



(define (pretty-slide #:title [title ""] . data)
  (play-n
   #:skip-first? #t
   #:skip-last? #t
   #:title title
   (animate-slide
    'next
    'alts
    `(,data ()))))


(define (header-slide #:title [title ""] #:header [header ""] . data)
  (play-n
   #:title title
   #:skip-first? #t
   #:skip-last? #t
   (λ (n1 n2 n3)
     (fade-pict
      n3 (fade-pict 
          n1 (t "")
          (fade-around-pict
           n2 header (λ (x)
                       (apply vc-append `(0 ,x ,@data)))))
      (t "")))))

(define-syntax (picture-slide stx)
  (syntax-case stx ()
    [(k #:title title first-pic pic ...)
     ; =>
     #'(picture-slide* title first-pic pic ...)]
    
    [(k first-pic pic ...)
     ; =>
     #'(picture-slide* "" first-pic pic ...)]))

(define-syntax (picture-slide* stx)
  (define (build-transitions pic id acc)
    (cond [(stx-null? pic) acc]
          [(stx-null? (stx-cdr pic))
           ; =>
           #`(fade-pict #,(stx-car id) #,acc
                        (scale #,(stx-car pic) #,(stx-car id)))]
          
          [else
           ; =>
           (build-transitions (stx-cdr pic) (stx-cdr id)
                              #`(fade-pict #,(stx-car id) #,acc
                                           (scale #,(stx-car pic) (+ #,(stx-car id) #,(stx-car (stx-cdr id))))))]))
  (syntax-case stx ()
    [(k title first-pic pic ...)
     ; =>
     (with-syntax ([(first-id) (generate-temporaries #'(first-pic))]
                   [(id ...) (generate-temporaries #'(pic ...))]
                   [(last-id) (generate-temporaries #'(1))])
       (with-syntax ([body (build-transitions #'(pic ...) #'(id ...)
                                              #`(cellophane (scale first-pic (+ 1 #,(stx-car #'(id ...))))
                                                            first-id))])
         #'(play-n
            #:skip-first? #t
            #:skip-last? #t
            #:title title
            (λ (first-id id ... last-id)
              (cellophane body (- 1 last-id))))))]))

(define-syntax (section stx)
  (syntax-case stx ()
    [(k #:title section-title slides ...)
     ; =>
     (with-syntax ([pretty-slide (datum->syntax #'k 'pretty-slide)]
                   [header-slide (datum->syntax #'k 'header-slide)]
                   [picture-slide (datum->syntax #'k 'picture-slide)])
       #'(let ()
           (define (pretty-slide #:title [title #f] . data)
             (unless title
               (set! title section-title))
             (play-n
              #:skip-first? #t
              #:skip-last? #t
              #:title title
              (animate-slide
               'next
               'alts
               `(,data ()))))
           
           (define (header-slide #:title [title #f] #:header [header ""] . data)
             (unless title
               (set! title section-title))
             (play-n
              #:skip-first? #t
              #:skip-last? #t
              #:title title
              (λ (n1 n2 n3)
                (fade-pict n3
                           (fade-pict n1
                                      (t "")
                                      (fade-around-pict n2 header (λ (x)
                                                                    (apply vc-append `(0 ,x ,@data)))))
                (t "")))))
                
           (define-syntax (picture-slide stx)
             (syntax-case stx ()
               [(k #:title title first-pic pic (... ...))
                ; =>
                #'(picture-slide* title first-pic pic (... ...))]
               
               [(k first-pic pic (... ...))
                ; =>
                #'(picture-slide* section-title first-pic pic (... ...))]))
           
           slides ...))]))

; Slideshow
(title-slide
 (colorize (large-text "Machine Learning Tools") "black")
 (colorize (medium-text "Leif Andersen") "blue")
 (colorize (t "University of Utah") "red"))

(pretty-slide
 #:title "Learning Tools"
 (item "LibAlf")
 'next
 (item "LearnLib")
 'next
 (item "RALT"))

(header-slide
 #:header (medium-text "Comparison of Tools")
 (scale (bitmap "learntools.png") 1))

(pretty-slide
 (large-text "Two Type of Learning"))

(pretty-slide
 #:title "Two Types of Learning"
 (item "Online Learning")
 'next
 (item "Offline Learning"))

;(play-n
; #:skip-first? #t
; #:skip-last? #t
; #:title "Learning Algorithms"
; (λ (n1 n2 n3)
;   (cellophane (fade-pict n2 (cellophane (scale (bitmap "Algorithms1.png") (+ 2 (* 2 n2))) n1)
;                          (scale (bitmap "Algorithms2.png") (* 2 n2))) (- 1 n3))))

(picture-slide
 #:title "Learning Algorithms"
 (scale (bitmap "Algorithms1.png") 2)
 (scale (bitmap "Algorithms2.png") 2))

(pretty-slide
 (massive-text "Online Learning"))

(section
 #:title "Online Learning"
 
 (pretty-slide
  (large-text "Oracle knows language"))
 
 (pretty-slide
  (large-text "Learner chooses questions"))
 
 (pretty-slide
  (item "Learner assumes trivial DFA.")
  'next
  (item "Learner asks Oracle if strings are in the language.")
  'next
  (item "Learner guesses DFA.")
  'next
  (item "If Correct: Finished.")
  'next
  (item "If Incorrect: Repeate process with new knowledge."))
 
 (header-slide
  #:header (medium-text "Example:")
  (large-$$ "L=\\{x|\\mathit{gcd}(x,3) = 3\\}"))
 
; (play-n
;  #:skip-first? #t
;  #:skip-last? #t
;  (λ (n1 n2 n3)
;    (cellophane (fade-pict n2 (cellophane (scale (bitmap "div3_0.png") (+ 2 (* 2 n2))) n1)
;                           (scale (bitmap "div3_1.png") (* 2 n2))) (- 1 n3))))

 (picture-slide
  (scale (bitmap "div3_0.png") 2)
  (scale (bitmap "div3_1.png") 2))
 
 (header-slide
  #:header (medium-text "Example:")
  (large-$$ "L=\\{10\\}"))

; (play-n
;  #:skip-first? #t
;  #:skip-last? #t
;  (λ (n1 n2 n3 n4)
;    (cellophane (fade-pict n3
;                           (fade-pict n2 (cellophane (scale (bitmap "set10_0.png") (+ 2 (* 2 n2))) n1)
;                                         (scale (bitmap "set10_1.png") (+ (* 2 n2))))
;                           (scale (bitmap "set10_2.png") n3)) (- 1 n4))))
 
 (picture-slide
  (scale (bitmap "set10_0.png") 2)
  (scale (bitmap "set10_1.png") 2)
  (bitmap "set10_2.png"))
 
 (header-slide
  #:header (medium-text "Example:")
  (large-$$ "L_5Two_1"))

 (picture-slide
  (scale (bitmap "two1five0_0.png") 2)
  (bitmap "two1five0_1.png")
  (bitmap "two1five0_2.png")
  (bitmap "two1five0_3.png")
  (bitmap "two1five0_4.png")))
 
; (play-n
;  #:skip-first? #t
;  #:skip-last? #t
;  (λ (n1 n2 n3 n4 n5 n6)
;    (cellophane 
;     (fade-pict n5 
;                (fade-pict n4 
;                           (fade-pict n3 
;                                      (fade-pict n2 
;                                                 (cellophane (scale (bitmap "two1five0_0.png") (+ 2 (* 2 n2))) n1)
;                                                 (scale (bitmap "two1five0_1.png") (+ n2)))
;                                      (scale (bitmap "two1five0_2.png") (+ n3 n4)))
;                           (scale (bitmap "two1five0_3.png") (+ n4 n5)))
;                (scale (bitmap "two1five0_4.png") n5))
;     (- 1 n6)))))

(pretty-slide
 (massive-text "Offline Learning"))

(section
 #:title "Offline Learning"
 
 (pretty-slide
  (massive-text "No Oracle"))
 
 (pretty-slide
  (medium-text "Learner handed strings known to be in")
  (medium-text "and not in the language"))
 
 (pretty-slide
  (item "Learner assumes trivial DFA.")
  'next
  (item "Learner Tests each string in given set.")
  'next
  (item "If it matches, DFA is not changed.")
  'next
  (item "If it conflicts assumption, it modifies DFA to fit new data.")
  'next
  (item "Learner gives minimal DFA that fits criteria.")
  'next
  (item "If known to be incorrect, Learner requests additional data set.")
  'next
  (item "Otherwise Complete."))
 
 (header-slide
  #:header (medium-text "Example")
  (massive-text "Live Demo")))

(pretty-slide
 (massive-text "Use in URV"))
 
(header-slide
 #:header (medium-text "More Information")
 (medium-text "Learning Algorithms and Formal Verification")
 (medium-text "P. Madhusudan"))

(pretty-slide
 (massive-text "Questions?"))