;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))




;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; Interp. A list of invader objects

(define LOI1 empty)
(define LOI2 (cons I1 (cons I2 empty)))

(define (fn-for-loinvader loinvader)
  (cond [(empty? loinvader) (...)]   ;; Base case - empty list
        [else (... (first loinvader) ;; Invader
                   (fn-for-loinvader (rest loinvader)))])) ;; Natural recursion


;; ListOfMissiles is one of:
;; - empty
;; - (cons Missile ListOfMissile)
;; Interp. A list of Missile objects

(define LOM1 empty)
(define LOM2 (cons M1 (cons M2 empty)))

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (first lom) ;; Missile
                   (fn-for-lom (rest lom)))])) ;; Natural Recursion



;; ---------------------------------------------------------------------------------------------
;; FUNCTIONS
;; ---------------------------------------------------------------------------------------------

;; Game -> Game
;; start the world with (main G0)

(define (main s)
  (big-bang s                  ;; Game
    (on-tick update-positions) ;; Game -> Game
    (to-draw render-all)       ;; Game -> Image
    (on-key handle-key)        ;; Game KeyEvent -> Game
    (stop-when invader-landed? ;; Game -> Boolean
               game-over)))


;; ---------------------------------------------------------------------------------------------
;; UPDATE FUNCTIONS



;; Game -> Game
;; Update the position of all game objects and determine if an additional invader should spawn
;; !!! Add tests for updating position 

;(define (update_positions s) s) ;; Stub

(define (update-positions s)
  (make-game (spawn-invader? (invader-hit-detection (update-invaders (game-invaders s)) (update-missiles (game-missiles s))))
             (missile-hit-detection (update-invaders (game-invaders s)) (clean-missile-list (update-missiles (game-missiles s))))
             (update-tank (game-tank s))))



;; ListOfInvaders -> ListOfInvaders
;; Updates the position every invader in a list of invaders
(check-expect (update-invaders empty) empty)
(check-expect (update-invaders (cons (make-invader 150 200 1.5) empty))
              (cons (update-invader (make-invader 150 200 1.5))
                    empty))
(check-expect (update-invaders (cons (make-invader 150 200 1.5) (cons (make-invader 200 300 -1.5) empty)))
              (cons (update-invader (make-invader 150 200 1.5))
                    (cons (update-invader (make-invader 200 300 -1.5))
                          empty)))

;(define (update-invaders loinvader) loinvader) ;; Stub

(define (update-invaders loinvader)
  (cond [(empty? loinvader) empty]
        [else (cons (update-invader (first loinvader))
                    (update-invaders (rest loinvader)))]))
  


;; Invader -> Invader
;; Update the position of an individual invader
(check-expect (update-invader (make-invader 150 200 INVADER-X-SPEED))
              (make-invader (+ 150 INVADER-X-SPEED)
                            (+ 200 INVADER-Y-SPEED)
                            INVADER-X-SPEED))
(check-expect (update-invader (make-invader 150 200 (- INVADER-X-SPEED)))
              (make-invader (+ 150 (- INVADER-X-SPEED))
                            (+ 200 INVADER-Y-SPEED)
                            (- INVADER-X-SPEED)))
(check-expect (update-invader (make-invader (+ 1 INVADER-WIDTH/2) 200 -1.5)) ; Hit left wall
              (make-invader (- (image-width INVADER) (+ 1 INVADER-WIDTH/2 -1.5))
                            (+ 200 INVADER-Y-SPEED)
                            (- -1.5)))
(check-expect (update-invader (make-invader (- WIDTH (+ INVADER-WIDTH/2 1)) 200 1.5)) ; Hit right wall
              (make-invader (- (- (* WIDTH 2) (image-width INVADER)) (+ (- WIDTH (+ INVADER-WIDTH/2 1)) 1.5))
                            (+ 200 INVADER-Y-SPEED)
                            (- 1.5)))
(check-expect (update-invader (make-invader 100 (- HEIGHT 1) -1.5)) ; Land moving left
              (make-invader (+ 100 -1.5)
                            HEIGHT
                            -1.5))
(check-expect (update-invader (make-invader 100 (- HEIGHT 1) 1.5)) ; Land moving right
              (make-invader (+ 100 1.5)
                            HEIGHT
                            1.5))

;(define (update-invader i) i) ;; Stub

(define (update-invader i)
  (cond [(>= (+ (invader-y i) INVADER-Y-SPEED) HEIGHT)  ; Landing case
         (make-invader (+ (invader-x i) (invader-dx i))
                       HEIGHT
                       (invader-dx i))]
        [(<= (+ (invader-x i) (invader-dx i)) INVADER-WIDTH/2)  ; Hit left wall
         (make-invader (- (image-width INVADER) (+ (invader-x i) (invader-dx i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-dx i)))]
        [(>= (+ (invader-x i) (invader-dx i)) (- WIDTH INVADER-WIDTH/2))  ; Hit right wall
         (make-invader (- (- (* WIDTH 2) (image-width INVADER)) (+ (invader-x i) (invader-dx i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-dx i)))]
        [else ; Base case
         (make-invader (+ (invader-x i) (invader-dx i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))



;; ListOfInvaders -> ListOfInvaders
;; determine if an invader should be spawned, if so adds it to the list
;; !!!
(define (add-invader? loi) loi)


;; ListOfMissiles -> ListOfMissiles
;; Update the position of every missile in the list of missiles
;; !!!
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (cons (make-missile 100 20)
                                     (cons (make-missile 40 300)
                                           empty)))
              (cons (update-missile (make-missile 100 20))
                    (cons (update-missile (make-missile 40 300))
                          empty)))

;(define (update-missiles lom) lom) ; Stub

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (update-missile (first lom))
                    (update-missiles (rest lom)))]))



;; Missile -> Missile
;; Update the position of a single missile based on MISSILE-SPEED
(check-expect (update-missile (make-missile 100 20))
              (make-missile 100
                            (- 20 MISSILE-SPEED)))
(check-expect (update-missile (make-missile 40 300))
              (make-missile 40
                            (- 300 MISSILE-SPEED)))

;(define (update-missile m) m) ; Stub

(define (update-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; Moves the tank in its current direction, stop when hitting the edge of the screen
;; !!!
(check-expect (update-tank (make-tank TANK-HEIGHT/2 -1)) ; Start at left wall
              (make-tank TANK-HEIGHT/2 -1))
(check-expect (update-tank (make-tank (- WIDTH TANK-HEIGHT/2) 1)) ; Start at right wall
              (make-tank (- WIDTH TANK-HEIGHT/2) 1))
(check-expect (update-tank (make-tank (+ 1 TANK-HEIGHT/2) -1)) ; Hit left wall
              (make-tank TANK-HEIGHT/2 -1))
(check-expect (update-tank (make-tank (- (- WIDTH TANK-HEIGHT/2) 1) 1)) ; Hit right wall
              (make-tank (- WIDTH TANK-HEIGHT/2) 1))
(check-expect (update-tank (make-tank 50 -1)) ; Move left
              (make-tank (+ 50 (* -1 TANK-SPEED))
                         -1))
(check-expect (update-tank (make-tank 78 1)) ; Move right
              (make-tank (+ 78 (* 1 TANK-SPEED))
                         1))

;(define (update-tank t) t) ; Stub

(define (update-tank t)
  (cond [(<=  (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) TANK-HEIGHT/2) ; Hit left wall
         (make-tank TANK-HEIGHT/2
                    (tank-dir t))]
        [(>=  (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (- WIDTH TANK-HEIGHT/2)) ; Hit right wall
         (make-tank (- WIDTH TANK-HEIGHT/2)  
                    (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED))
                    (tank-dir t))]))



;; ListOfMissiles -> ListOfMissiles
;; Remove missiles that fly beyond the top of the screen (assume missile list sorted in order of increasing height)
(check-expect (clean-missile-list empty) empty)
(check-expect (clean-missile-list (list (make-missile 54 123)))
              (list (make-missile 54 123)))
(check-expect (clean-missile-list (list (make-missile 63 287)
                                        (make-missile 123 123)
                                        (make-missile 12 0)
                                        (make-missile 43 -10)
                                        (make-missile 213 -23)))
              (list (make-missile 63 287)
                    (make-missile 123 123)
                    (make-missile 12 0)))

;(define (clean-missile-list lom) lom) ; Stub

(define (clean-missile-list lom)
  (cond [(empty? lom) empty]
        [(< (missile-y (first lom)) 0) empty]
        [else (cons (first lom)
                    (clean-missile-list (rest lom)))]))



;; ListOfInvaders ListOfMissiles -> ListOfMissiles
;; Returns a ListOfMissiles with any missiles that have hit invaders removed
;; assume list of missiles and list of invaders sorted from bottom to top
(check-expect (missile-hit-detection empty empty) empty)
(check-expect (missile-hit-detection empty LOM2) LOM2)
(check-expect (missile-hit-detection LOI2 empty) empty)
(check-expect (missile-hit-detection LOI2 LOM2) LOM2)
(check-expect (missile-hit-detection (list (make-invader 43 452 -2.5)
                                           (make-invader 123 23 2.5))
                                     (list (make-missile 43 430)
                                           (make-missile 123 32)))
              (list (make-missile 43 430)))
(check-expect (missile-hit-detection (list (make-invader 43 452 -2.5)
                                           (make-invader 123 23 2.5))
                                     (list (make-missile 43 450)
                                           (make-missile 123 23)))
              empty)

;(define (missile-hit-detection loi lom) lom) ; Stub

(define (missile-hit-detection loi lom)
  (cond [(empty? lom) empty]                                 ; Out of missiles

        [(empty? loi) lom]                                   ; No invaders, all remaining missiles stay in LoM

        [(and (< (abs (- (missile-x (first lom))             ; HIT
                         (invader-x (first loi)))) HIT-RANGE)
              (< (abs (- (missile-y (first lom))
                         (invader-y (first loi)))) HIT-RANGE))
         (missile-hit-detection loi (rest lom))]
        
        [(< (missile-y (first lom)) (invader-y (first loi))) ; Missile lower than Invader -> check rest of Invaders
         (missile-hit-detection (rest loi) lom)]
        
        [else (cons (first lom)
                    (missile-hit-detection loi (rest lom)))])) ; Missile above Invader -> check rest of Missiles


;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Returns a ListOfInvaders with any invaders that have been hit by missiled removed
(check-expect (invader-hit-detection empty empty) empty)
(check-expect (invader-hit-detection empty LOM2) empty)
(check-expect (invader-hit-detection LOI2 empty) LOI2)
(check-expect (invader-hit-detection LOI2 LOM2) LOI2)
(check-expect (invader-hit-detection (list (make-invader 43 452 -2.5)
                                           (make-invader 123 23 2.5))
                                     (list (make-missile 43 430)
                                           (make-missile 123 32)))
              (list (make-invader 43 452 -2.5)))
(check-expect (invader-hit-detection (list (make-invader 43 452 -2.5)
                                           (make-invader 123 23 2.5))
                                     (list (make-missile 43 450)
                                           (make-missile 123 23)))
              empty)

;(define (invader-hit-detection loi lom) loi) ; Stub

(define (invader-hit-detection loi lom)
  (cond [(empty? loi) empty]   ; Out of invaders
        [(empty? lom) loi]     ; Out of missiles

        [(and (< (abs (- (missile-x (first lom))             ; HIT
                         (invader-x (first loi)))) HIT-RANGE)
              (< (abs (- (missile-y (first lom))
                         (invader-y (first loi)))) HIT-RANGE))
         (invader-hit-detection (rest loi) lom)]
        
        [(< (invader-y (first loi)) (missile-y (first lom))) ; Invader below Missile -> check rest of Missiles
         (invader-hit-detection loi (rest lom))]
        
        [else (cons (first loi)
                    (invader-hit-detection (rest loi) lom))])) ; Invader above Missile -> check rest of Invaders



;; ListOfInvaders -> ListOfInvaders
;; Using the INVADER-RATE and a random number generator to detemine if a new invader should be spawned at the top of the screen
(check-random (spawn-invader? empty) (insert-invader empty))
(check-expect (spawn-invader? LOI2) LOI2)

;(define (spawn-invader? loi) loi) ; Stub

(define (spawn-invader? loi)
  (cond [(empty? loi) (insert-invader loi)]
        [else (if (< (random INVADE-RATE) 2)
                  (insert-invader loi)
                  loi)]))

;; ListOfInvaders -> ListOfInvaders
;; Insert an invader into the list of invaders at the to the front of the list to ensure that
;; order of list of invaders is in order from bottom to top when adding invader at top of screen
(check-random (insert-invader empty) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED)))
(check-random (insert-invader LOI2) (append LOI2 (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) empty)))

;(define (insert-invader loi) loi) ; Stub

(define (insert-invader loi)
  (cond [(empty? loi) (list (make-invader (random WIDTH) 0 INVADER-X-SPEED))]
        [else (append loi
                      (list (make-invader (random WIDTH)0 INVADER-X-SPEED)))]))


;; ---------------------------------------------------------------------------------------------
;; RENDER FUNCTIONS



;; Game -> Image
;; Render all currently existing objects in the Game object on the screen
(check-expect (render-all G0)
              (render-tank T0
                           (render-missiles empty
                                            (render-invaders empty))))
(check-expect (render-all G3)
              (render-tank T1
                           (render-missiles (list M1 M2)
                                            (render-invaders (list I1 I2)))))

;(define (render_all s) BACKGROUND) ; Stub

(define (render-all s)
  (render-tank (game-tank s) (render-missiles (game-missiles s) (render-invaders (game-invaders s)))))



;; ListOfInvaders -> Image
;; Render a list of invaders placed based on their x,y coordinates. Base case is blank background
(check-expect (render-invaders empty) BACKGROUND)
(check-expect (render-invaders (cons (make-invader 100 200 -1.5) empty))
              (place-image INVADER 100 200 BACKGROUND))
(check-expect (render-invaders (list (make-invader 100 200 -1.5) (make-invader 185 29 1.5)))
              (place-image INVADER 100 200
                           (place-image INVADER 185 29 BACKGROUND)))

;(define (render-invaders loinvader) BACKGROUND) ; Stub

(define (render-invaders loinvader)
  (cond [(empty? loinvader) BACKGROUND]
        [else (render-invader-on (first loinvader)
                                 (render-invaders (rest loinvader)))]))



;; Invader, Image -> Image
;; Draws an individual invader at its x,y location on an existing image
(check-expect (render-invader-on (make-invader 100 200 1.5) BACKGROUND)
              (place-image INVADER 100 200 BACKGROUND))
(check-expect (render-invader-on (make-invader 185 29 1.5) BACKGROUND)
              (place-image INVADER 185 29 BACKGROUND))

;(define (render-invader-on invader img) img)

(define (render-invader-on invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img)) 



;; ListOfMissiles, Image -> Image
;; Draws a list of missiles on an existing image
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list (make-missile 12 200) (make-missile 175 23)) BACKGROUND)
              (place-image MISSILE 12 200
                           (place-image MISSILE 175 23 BACKGROUND)))

;(define (render-missiles lom img) img) ; Stub

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (place-image MISSILE
                           (missile-x (first lom))
                           (missile-y (first lom))
                           (render-missiles (rest lom) img))]))



;; Tank, Image -> Image
;; Draws a tank at its specified x coordinate at the bottom of the screen of an existing image
(check-expect (render-tank (make-tank 50 1) BACKGROUND)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank (make-tank TANK-HEIGHT/2 1) BACKGROUND) ; At left wall
              (place-image TANK TANK-HEIGHT/2 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank (make-tank (- WIDTH TANK-HEIGHT/2) 1) BACKGROUND) ; At right wall
              (place-image TANK (- WIDTH TANK-HEIGHT/2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t img) img) ; Stub

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))



;; ---------------------------------------------------------------------------------------------
;; KEYBOARD HANDLER FUNCTIONS



;; Game KeyEvent -> Game
;; Handle key presses for the game
;; - left arrow  -> set tank direction to left
;; - right arrow -> set tank direction to the right
;; - space bar   -> fire a missile from the tanks current location upwards
(check-expect (handle-key (make-game empty empty (make-tank 50 1)) "left") ; moving right, press left
              (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 -1)) "left") ; moving left, press left
              (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 1)) "right") ; moving right, press right
              (make-game empty empty (make-tank 50 1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 -1)) "right") ; moving left, press right
              (make-game empty empty (make-tank 50 1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 1)) "q") ; moving right, press 'q' (invalid input)
              (make-game empty empty (make-tank 50 1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 -1)) "r") ; moving left, press 'r' (invalid input)
              (make-game empty empty (make-tank 50 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 50 1)) " ") ; press space, empty missile list
              (make-game empty (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2))) (make-tank 50 1)))
(check-expect (handle-key (make-game LOI2 LOM2 (make-tank 124 -1)) " ") ; press space, missile list with entries
              (make-game LOI2 (cons (make-missile 124 (- HEIGHT TANK-HEIGHT/2)) LOM2) (make-tank 124 -1)))

;(define (handle-key s ke) s) ; Stub

(define (handle-key s ke)
  (cond [(key=? ke " ") (make-game (game-invaders s)
                                   (cons (make-missile (tank-x (game-tank s))
                                                       (- HEIGHT TANK-HEIGHT/2))
                                         (game-missiles s))
                                   (game-tank s))]
        [(key=? ke "left") (make-game (game-invaders s)
                                      (game-missiles s)
                                      (make-tank (tank-x (game-tank s))
                                                 -1))]
        [(key=? ke "right") (make-game (game-invaders s)
                                       (game-missiles s)
                                       (make-tank (tank-x (game-tank s))
                                                  1))]
        [else s]))



;; ---------------------------------------------------------------------------------------------
;; STOP GAME FUNCTIONS

;; Game -> Boolean
;; Returns True if an invader has landed, otherwise false
(check-expect (invader-landed? G0) false)
(check-expect (invader-landed? G1) false)
(check-expect (invader-landed? G2) false)
(check-expect (invader-landed? G3) true)

;(define (invader-landed? s) false) ; Stub

(define (invader-landed? s)
  (landed-helper (game-invaders s)))


;; ListOfInvader -> Boolean
;; Helper to traverse the ListOfInvaders checking for landed condition
(check-expect (landed-helper empty) false)
(check-expect (landed-helper (list (make-invader 43 54 1.5))) false)
(check-expect (landed-helper LOI2) true)

;(define (landed-helper loi) false) ; Stub

(define (landed-helper loinvader)
  (cond [(empty? loinvader) false]   ;; Base case - empty list
        [(>= (invader-y (first loinvader)) HEIGHT) true]
        [else (landed-helper (rest loinvader))]))



;; Game -> Image
;; Displays a game over screen
(check-expect (game-over G0)
              (place-image/align (text "GAME OVER" 48 "red")
                                 (/ WIDTH 2)
                                 (/ HEIGHT 2)
                                 "center"
                                 "center"
                                 BACKGROUND))

;(define (game-over s) BACKGROUND) ; Stub

(define (game-over s)
  (place-image/align (text "GAME OVER" 48 "red")
                                 (/ WIDTH 2)
                                 (/ HEIGHT 2)
                                 "center"
                                 "center"
                                 BACKGROUND))

;; ---------------------------------------------------------------------------------------------
;; Automatically run Main

(main (make-game empty empty T0))