;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname yes) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))

(define-struct WS (person lobstacles time lokey background counter))
; Person is a person structure
; Lobstacles is a list of obstacle structures
; Time is a number
; Lokey is a list of keys
; Background is the number which represents what background should be in place

(define-struct person (x y dx power))
; x is a number for the x position of the person
; y is a number for the y postition of the person
; dx is the speed in the x direction of the person
; power is the string that represents the power that the person has

(define-struct obstacle (x y dx dy type))
; x is a number for the x position of the obstacle
; y is a number for the y position of the obstacle
; dx is the speed in the x direction
; dy is the speed in the y direction


(define BACKGROUND1 .)
(define BACKGROUND2 . )
(define BACKGROUND3 .)
(define PERSON (scale .07 .))

(define ROCK .)
(define LASER .)
(define TIME .)
(define initial-Worldstate (make-WS (make-person 400 100 7 "none") empty 0 empty 1 0))

; Updates the worldstate's lobstacles with the lobstacles in the function
; Worldstate, List of Obstacles -> Worldstate
(define (updatelobstacles ws lobstacles)
  (make-WS (WS-person ws) lobstacles (WS-time ws) (WS-lokey ws) (WS-background ws) (WS-counter ws)))

; Updates the lokey in the ws to lokey
; Worldstate, Lokey -> Worldstate
(define (update-lokey ws lokey)
  (make-WS (WS-person ws) (WS-lobstacles ws) (WS-time ws) lokey (WS-background ws) (WS-counter ws)))


; Worldstate, Key -> Worldstate
; Adds key to the lokey of worldstate
(define (addkey ws key)
  (make-WS (WS-person ws) (WS-lobstacles ws) (WS-time ws) (cons key (WS-lokey ws)) (WS-background ws) (WS-counter ws)))

; Creates a new obstacle
; Worldstate, Obstacle -> Worldstate
(define (create-obstacle ws obstacle)
  (make-WS (WS-person ws) (cons obstacle (WS-lobstacles ws)) (WS-time ws) (WS-lokey ws) (WS-background ws) (WS-counter ws)))

; Adds new time to the old time
; Worldstate, Number -> Worldstate
(define (WS-new-time ws new-time)
  (make-WS (WS-person ws) (WS-lobstacles ws) new-time (WS-lokey ws) (WS-background ws) (WS-counter ws)))

; Adds x to the x value of the person
; Worldstate -> Worldstate
(define (changepersonx ws x)
  (make-WS (make-person (+ (person-x (WS-person ws)) x) (person-y (WS-person ws)) (person-dx (WS-person ws)) (person-power (WS-person ws))) (WS-lobstacles ws) (WS-time ws) (WS-lokey ws) (WS-background ws) (WS-counter ws)))

; Draws the worldstate
; Worldstate -> Image
(define (render ws)
  (place-image
   (text (string-append "Time: " (number->string (floor (WS-time ws)))) 20 "white")
   100 20
   (place-image
    PERSON
    (person-x (WS-person ws)) (person-y (WS-person ws))
    (drawlobstacles ws))))

; Draws the items in the list of obstacles in the ws
; Worldstate -> Image
(define (drawlobstacles ws)
  (foldl (lambda (li acc)
           (place-image
            (cond
              [(string=? (obstacle-type li) "rock") ROCK]
              [(or (string=? (obstacle-type li) "ammolaser") (string=? (obstacle-type li) "attack-laser") (string=? (obstacle-type li) "laser")) LASER]
              [else TIME])
            (obstacle-x li)
            (obstacle-y li) acc))
         (cond
           [(< (WS-background ws) 2) BACKGROUND1]
           [(< (WS-background ws) 4) BACKGROUND2]
           [else BACKGROUND3])
         (WS-lobstacles ws)))


; Adds the pressed key to the list of keys in the worldstate
; Worldstate, Key -> Worldstate
(define (key-press-handler ws key)
  (cond
    [(or (> 200 (person-x (WS-person ws))) (< 600 (person-x (WS-person ws)))) ws] 
    [(or (key=? key "left") (key=? key "right")) (addkey ws key)]
    [(key=? key "h") (addkey ws key)]
    [else ws]))

; Removes the key from the lokey when the key is released
; Worldstate, Key -> Worldstate
(define (key-release-handler ws key)
  (cond
    [(or (key=? key "left") (key=? key "right")) (make-WS (WS-person ws) (WS-lobstacles ws) (WS-time ws) (remove-key key (WS-lokey ws)) (WS-background ws) (WS-counter ws))]
    [(and (key=? key " ") (not (empty? (filter (λ (ob) (string=? (obstacle-type ob) "ammolaser")) (WS-lobstacles ws))))) (addkey ws key)]
    [(and (key=? key "t") (not (empty? (filter (λ (ob) (string=? (obstacle-type ob) "ammotime")) (WS-lobstacles ws))))) (addkey ws key)]
    [else ws]))
    

; Removes a key from a list of keys
; lokey, key -> lokey
(define (remove-key key lokey)
  (filter (λ (li) (not (string=? li key))) lokey))

; Updates the worldstate every tick
; Worldstate -> Worldstate
(define (ticker ws)
  (counter-adder (time-person-collision (rock-laser-collision (ammohandler (change-background (clean-obstacles (spawn-obstacles (timer (moveobstacles (moveperson ws)))))))))))




; Adds 1/60 to the counter if it is not 0 and goes back to 0 if the counter reaches 5
; Worldstate -> Worldstate
(define (counter-adder ws)
  (cond
    [(= (WS-counter ws) 0) ws]
    [(> (WS-counter ws) 5) (update-counter ws (- (WS-counter ws)))]
    [else (update-counter ws (/ 1 60))]))

; Adds n to the counter in the worldstate ws
; Worldstate -> Worldstate
(define (update-counter ws n)
  (make-WS (WS-person ws) (WS-lobstacles ws) (WS-time ws) (WS-lokey ws) (WS-background ws) (+ (WS-counter ws) n)))


; Detects when the laser has hit a rock and gets rid of both.
; Worldstate -> Worldstate
(define (rock-laser-collision ws)
  (cond
    [(empty? (filter (λ (ob) (string=? (obstacle-type ob) "attack-laser")) (WS-lobstacles ws))) ws]
    [else
     (local
       [(define list-wo-attacklaser (filter (λ (ob) (not (string=? (obstacle-type ob) "attack-laser"))) (WS-lobstacles ws)))
        (define attack-laser (first (filter (λ (ob) (string=? (obstacle-type ob) "attack-laser")) (WS-lobstacles ws))))]
       (cond
         [(not (empty? (filter (λ (ob) (< (distance (obstacle-x ob) (obstacle-y ob) (obstacle-x attack-laser) (obstacle-y attack-laser)) 15)) list-wo-attacklaser)))
          (updatelobstacles ws (filter (λ (ob) (> (distance (obstacle-x ob) (obstacle-y ob) (obstacle-x attack-laser) (obstacle-y attack-laser)) 15)) list-wo-attacklaser))]
         [else ws]))])) 


; Detects when the person has hit the time obstacle
; Worldstate -> Worldstate
(define (time-person-collision ws)
  (cond
    [(predict-collision ws "time") (local
                                     [(define colliding-time
                                        (first (filter (λ (ob) (< (distance (person-x (WS-person ws)) (person-y (WS-person ws)) (obstacle-x ob) (obstacle-y ob)) 15)) (WS-lobstacles ws))))]
                                     (make-WS (WS-person ws)
                                              (cons (make-obstacle 150 250 0 0 "ammotime")
                                                    (filter (λ (ob)
                                                              (not (and (= (obstacle-x ob) (obstacle-x colliding-time)) (= (obstacle-y ob) (obstacle-y colliding-time)))))
                                                            (WS-lobstacles ws))) 
                                              (WS-time ws)
                                              (WS-lokey ws)
                                              (WS-background ws)
                                              (WS-counter ws)))]
    [else ws]))
                                  

       
                             
                                                                                                                           
                                                                                                                         

; On collision with a laser the person gains the laser to use against an obstacle, if he presses space he loses the ammo
; Worldstate -> Worldstate
(define (ammohandler ws)
  (cond
    [(predict-collision ws "laser") (updatelobstacles ws (cons (make-obstacle 150 200 0 0 "ammolaser") (filter (λ (ob) (> (distance (person-x (WS-person ws)) (person-y (WS-person ws)) (obstacle-x ob) (obstacle-y ob)) 15)) (WS-lobstacles ws))))]
    [(keyinlist " " (WS-lokey ws))
     (cond
       [(not (empty? (filter (λ (ob) (string=? (obstacle-type ob) "ammolaser")) (WS-lobstacles ws))))
        (make-WS
         (WS-person ws)
         (cons (make-obstacle (person-x (WS-person ws)) (person-y (WS-person ws)) 0 -10 "attack-laser")
               (filter
                (λ (ob)
                  (not (string=? (obstacle-type ob) "ammolaser")))
                (WS-lobstacles ws)))
         (WS-time ws) (remove-key " " (WS-lokey ws)) (WS-background ws) (WS-counter ws))]
       [else (update-lokey ws (remove-key " " (WS-lokey ws)))])]
    [(keyinlist "t" (WS-lokey ws))
     (make-WS (WS-person ws)
              (filter (λ (ob) (not (string=? (obstacle-type ob) "ammotime"))) (WS-lobstacles ws))
              (WS-time ws)
              (remove-key "t" (WS-lokey ws))
              (WS-background ws)
              (/ 1 60))]
    [else ws]))
  

; Worldstate -> Worldstate
; Adds to the value of the background by one and will range from 1 to 120
(define (change-background ws)
  (cond
    [(>= (WS-background ws) 6) (make-WS (WS-person ws) (WS-lobstacles ws) (WS-time ws) (WS-lokey ws) 1 (WS-counter ws))]
    [else (make-WS (WS-person ws) (WS-lobstacles ws) (WS-time ws) (WS-lokey ws) (+ (WS-background ws) 1) (WS-counter ws))]))

; Gets rid of the obstacles that are no longer appearing on the screen
; Worldstate -> Worldstate
(define (clean-obstacles ws)
  (make-WS (WS-person ws) (filter (lambda (ob) (> (obstacle-y ob) 0)) (WS-lobstacles ws)) (WS-time ws) (WS-lokey ws) (WS-background ws) (WS-counter ws)))

; Adds obstacles to the obstacle list at random
; Worldstate -> Worldstate
(define (spawn-obstacles ws)
  (cond
    [(< (floor (WS-time ws)) 10) (difficulty 30 7 ws)]
    [(< (floor (WS-time ws)) 15) (difficulty 25 8 ws)]
    [(< (floor (WS-time ws)) 20) (difficulty 20 9 ws)]
    [(< (floor (WS-time ws)) 25) (difficulty 15 10 ws)]
    [(< (floor (WS-time ws)) 30) (difficulty 10 11 ws)]
    [(< (floor (WS-time ws)) 35) (difficulty 8 13 ws)] 
    [else (difficulty 5 15 ws)]))
    

; Number, Number, Worldstate -> Worldstate
; Spawns an obstacle at random with a 1 / n chance at a random speed from speed - 4 to speed
(define (difficulty n speed ws)
  (cond
    [(if (not (= (WS-counter ws) 0)) (= (random 400) 1) (= (random n) 1)) (create-obstacle ws (make-obstacle (random 210 590) 800 0 (random (- speed 4) speed) "rock"))]
    [(= (random 500) 1) (create-obstacle ws (make-obstacle (random 210 590) 800 0 6 "laser"))]
    [(= (random 200) 1) (create-obstacle ws (make-obstacle (random 210 590) 800 0 6 "time"))]
    [(keyinlist "h"(WS-lokey ws)) (if (= (random 25) 1) (create-obstacle ws (make-obstacle (random 210 590) 800 0 6 "time")) ws)]
    [else ws]))

; Adds 1 to the time every 28 ticks
; Worldstate -> Worldstate
(define (timer ws) (WS-new-time ws (+ (WS-time ws) (/ 1 60))))

; Moves the obstacles based on their dy
; Worldstate -> Worldstate
(define (moveobstacles ws)
  (cond
    [(not (= (WS-counter ws) 0)) (updatelobstacles ws
                                                   (map (λ (ob)
                                                          (cond
                                                            [(string=? (obstacle-type ob) "rock")
                                                             (make-obstacle (obstacle-x ob) (- (obstacle-y ob) 2) (obstacle-dx ob) (obstacle-dy ob) (obstacle-type ob))]
                                                            [else (make-obstacle
                                                                   (obstacle-x ob)
                                                                   (- (obstacle-y ob) (obstacle-dy ob))
                                                                   (obstacle-dx ob) (obstacle-dy ob) (obstacle-type ob))]))
                                                        (WS-lobstacles ws)))]
    [else (updatelobstacles ws (map
                                (λ (ob)
                                  (make-obstacle
                                   (obstacle-x ob)
                                   (- (obstacle-y ob) (obstacle-dy ob))
                                   (obstacle-dx ob)
                                   (obstacle-dy ob)
                                   (obstacle-type ob)))
                                (WS-lobstacles ws)))]))
                                
  

; Moves the person in the worldstate depending on the list of keys lokey
; Worldstate -> Worldstate
(define (moveperson ws)
  (cond
    [(and (> (person-x (WS-person ws)) 210) (keyinlist "left" (WS-lokey ws))) (changepersonx ws (- (person-dx (WS-person ws))))]
    [(and (< (person-x (WS-person ws)) 590) (keyinlist "right" (WS-lokey ws))) (changepersonx ws (person-dx (WS-person ws)))]
    [else ws]))

; Checks if a key is in the list of keys lokey
; Lokey -> Boolean
(define (keyinlist key lokey)
  (cond
    [(empty? (filter (λ (x) (string=? key x)) lokey)) #f]
    [else #t]))

; Checks the distance between 2 points based on 2 x values and 2 y values
; Number, Number, Number, Number -> Number
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

; Worldstate, String -> Boolean
; Predicts whether any obstacle of the specified type will pass the person
(define (predict-collision ws type)
  (cond
    [(empty? (filter (λ (ob) (and (string=? (obstacle-type ob) type) (< (distance (person-x (WS-person ws)) (person-y (WS-person ws)) (obstacle-x ob) (obstacle-y ob)) 15))) (WS-lobstacles ws))) #f]
    [else #t]))

; Checks certain condtitions to end the game
; Worldstate -> Boolean
(define (end-game? ws)
  (predict-collision ws "rock"))
                                
(big-bang initial-Worldstate
  (stop-when end-game?) 
  (on-key key-press-handler)
  (on-release key-release-handler)
  (to-draw render)
  (on-tick ticker (/ 1 60)))