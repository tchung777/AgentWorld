;My Agent
;Thomas Chung   2018    All rights reserved.
;This is my implementation for my agent for AgentWorld.
;It is a cowardly AI that runs away from predator when they are closed enough
;It is a greedy and slothful AI that eats edible vegetation directly in front of it.

; Seed random number generator
; Example code from guile user's manual
; http://www.gnu.org/software/guile/manual/html_node/Random.html
(let ((time (gettimeofday)))
      (set! *random-state*
            (seed->random-state (+ (car time)
                                   (cdr time)))))

(define (initialize-agent)
        "OK AY")

;This function is a helper function to help me re-write the parameters for nth-item
;It recursively calls itself if the counter is less than maxNum and splicies the the head of
;the list off. When the counter is the desired nth-item counter, then the head is what we want.
(define (helper alist maxNum counter) 
    (cond ((> counter maxNum) '())
          ((= counter maxNum) (car alist))
          (#t (helper (cdr alist) maxNum (+ 1 counter)))))

;This function helps kick off the helper function
(define (nth-item idx alist)
  (helper alist idx 1))

;This function returns the coordinate of an item within an agent's percept.
;Since it seems the agent's percept returns this triangular vision,
;we just shift the inputted x accordinly depending on what row the target is suppose to have.
(define (get-location percept x y)
    (nth-item (+ (+ x y) 1) (nth-item y percept)))

;This functions check to see if the location is a vegetation.
(define (isvegetation loc)
  (cond ((list? loc)
            (cond ((equal? (nth-item 1 loc) 'vegetation) #t)
                   (#t #f)
            ))
        (#t #f)
  )
)

;This function checks to see if the location contains edible vegetation.
(define (isedible loc)
  (cond ((isvegetation loc)
         (cond ((> (nth-item 3 loc) 0) #t)
               (#t #f)
         )
        )
        (#t #f)
  )
)

;This function checks to see if the loc contains a predator.
(define (ispredator loc)
  (cond ((list? loc)
            (cond ((equal? (nth-item 1 loc) 'predator) #t)
                  (#t #f)
            ))
        (#t #f)
  )
)

;This function checks to see if the location is empty.
(define (isempty loc)
  (equal? 'empty loc)
)

;This function checks to see if the location is a barrier.
(define (isbarrier loc)
  (equal? 'barrier loc)
)

; This function checks to see if 'was attacked' is part of a list.
(define (was-attacked-single event)
  (not (equal? #f (member 'attacked-by event)))
)

; This function determined if at any point a 'was attacked' event happened in the past.
(define (was-attacked event)
  (not (equal? #f (member #t (map was-attacked-single event))))
)

(define (choose-action current-energy previous-events percepts)
	(let*  (
            (rand (random 2))
            (front (get-location percepts 0 1))
           )
           ;(display previous-events)               ;This line was used for debugging.
           ;(newline)                               ;This line was used for debugging.
           ;(display percepts)                      ;This line was used for debugging.
           ;(newline)                               ;This line was used for debugging.
		   ;(display (get-location percepts 0 1))   ;This line was used for debugging.
           ;(newline)                               ;This line was used for debugging.
           (cond 
                 
                 ;If there is a predator directly in front of me, then turn around!
                 ((ispredator front) "TURN-AROUND")

                 ;If there's food in front of me, then eat!
                 ((isedible front) "EAT-PASSIVE")

                 ; If I run into an obstacle (i.e: dead vegetation, barrier, an agent)
                 ; Then I either turn left or right.
                 ((or (and (isvegetation front) (not (isedible front)) (not (isempty front))) (isbarrier front)) 
                  (cond ((>= rand 1) "TURN-RIGHT")
                        (#t "TURN-LEFT")
                  ))
                 
                 ; If I was attacked previously, then run like hell. But don't run too hard.
                 ((was-attacked previous-events) "MOVE-AGGRESSIVE-2")

                 ; Otherwise DON'T MOVE.
                 (#t "STAY")
           )
    )
)

