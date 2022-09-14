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
    (on-tick update_positions) ;; Game -> Game
    (to-draw render_all)       ;; Game -> Image
    ))


;; ---------------------------------------------------------------------------------------------
;; UPDATE FUNCTIONS



;; Game -> Game
;; Update the position of all game objects and determine if an additional invader should spawn
;; !!! Add tests for updating position 

;(define (update_positions s) s) ;; Stub

(define (update_positions s)
  (make-game (update-invaders (game-invaders s))
             (update-missiles (game-missiles s))
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
(check-expect (update-invader (make-invader 1 200 -1.5)) ; Hit left wall
              (make-invader (- (+ 1 -1.5))
                            (+ 200 INVADER-Y-SPEED)
                            (- -1.5)))
(check-expect (update-invader (make-invader (- WIDTH 1) 200 1.5)) ; Hit right wall
              (make-invader (- WIDTH (- (+ (- WIDTH 1) 1.5) WIDTH))
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
        [(<= (+ (invader-x i) (invader-dx i)) 0)  ; Hit left wall
         (make-invader (- (+ (invader-x i) (invader-dx i)))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (- (invader-dx i)))]
        [(>= (+ (invader-x i) (invader-dx i)) WIDTH)  ; Hit right wall
         (make-invader (- WIDTH (- (+ (invader-x i) (invader-dx i)) WIDTH))
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
(check-expect (update-tank (make-tank 0 -1)) ; Start at left wall
              (make-tank 0 -1))
(check-expect (update-tank (make-tank WIDTH 1)) ; Start at right wall
              (make-tank WIDTH 1))
(check-expect (update-tank (make-tank 1 -1)) ; Hit left wall
              (make-tank 0 -1))
(check-expect (update-tank (make-tank (- WIDTH 1) 1)) ; Hit right wall
              (make-tank WIDTH 1))
(check-expect (update-tank (make-tank 50 -1)) ; Move left
              (make-tank (+ 50 (* -1 TANK-SPEED))
                         -1))
(check-expect (update-tank (make-tank 78 1)) ; Move right
              (make-tank (+ 78 (* 1 TANK-SPEED))
                         1))

;(define (update-tank t) t) ; Stub

(define (update-tank t)
  (cond [(<=  (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0) ; Hit left wall
         (make-tank 0
                    (tank-dir t))]
        [(>=  (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH) ; Hit right wall
         (make-tank WIDTH
                    (tank-dir t))]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED))
                    (tank-dir t))]))



;; ---------------------------------------------------------------------------------------------
;; RENDER FUNCTIONS



;; Game -> Image
;; Render all currently existing objects in the Game object on the screen
;; !!! Add tests and function definition
(define (render_all s) BACKGROUND)

;; ---------------------------------------------------------------------------------------------
;; KEYBOARD HANDLER FUNCTIONS

;; ---------------------------------------------------------------------------------------------
;; STOP GAME FUNCTIONS