; Author:         Joseph Phan 
; Description:    Project-Agent-World. This file contains the logic
;                 for an agent that will try to survive in an environment that has ...
;                   * other agents (other programmed bots)
;                   * vegetation (food that agents eat to gain energy)
;                   * predators (whose goal is to attack and reduce agent energies)
;                 Note that the environment is a grid (x by y) rectangle, and the game
;                 is a turn based system. Agents have different moves to explore and exploit
;                 the environment that cost energy. An Agent's view (or percept) is a 45 degree
;                 angle. The agent's ultimate goal is to collect as much energy as possible 
;                 and survive. (keeping energy > 0)
;
; Agent Logic (implemented functionality):    
;     1. Agent got attacked.. Run!
;     2. Predator is walking to agent.. Run! 
;     3. Agent already planned moves for the future. Trust the agent's planned decisions
;     4. Agent is too low on energy execute conservative strategy to drag out death vs searching for energy (normal strategy)
;     5. Agent is next to vegetation. Eat, or Camp by it. looking out for predators, and waiting for it to bloom
;     6. Agent is blocked by another agent to vegetation, walk around him/her
;     7. Look for and walk to nearby vegetation
;     8. Explore map

; Class:          Coen 266: AI
; Time:           Spring 2018


; Seed used for random number generator
(let ((time (gettimeofday)))
      (set! *random-state*
            (seed->random-state (+ (car time)
                                   (cdr time))))
)

; Example-Percept: 
;        -5 -4 -3 -2 -1 0  1  2  3  4  5
;      5  B  B E  V4 E  E  E  V1 B  B  B
;      4     B E  E  E  E  E  E  B  B
;      3       V3 E  E  E  E  E  B
;      2          E  V2 E  E  E
;      1             E  E  E
;      0                A

; This constant is used to unit test the functions and to provide a 
; visual of an example percept
(define example-percept '((empty empty empty)
(empty (vegetation 2 45) empty empty empty)
((vegetation 3 150) empty empty empty empty empty barrier)
(barrier empty empty empty empty empty empty barrier barrier)
(barrier barrier empty (vegetation 4 200) empty empty empty
(vegetation 1 125) barrier barrier barrier))
)

; ------------- Constants -------------
; This is used when the Agent is low on energy. The agent's tactic is to go to edge
; and stay, watching out for predators
(define stay-100-turns '(
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" "STAY" 
      )
)

; This is used to determine which vegetation relative to the agent are "valid" to chase after.
; This is ultimately determined by the number of turns the agent will need to take before being
; able to eat from the vegetation (ignoring the bloom pattern) As max, the agent will take 3 moves
; NOTE: the order of this list matters, as it will be sequentially checked. so the agent goes to the 
; vegetation with the least amount of moves
(define vegetation-percept-coordinates-list '(
      ; 1 moves to get to eat-position (step)
      (0 2) (0 3) (0 4) 
      ; 2 moves to get to eat-position (step turn)
      (-1 1)  (1 1) 
      (-1 2)  (1 2)
      (-1 3)  (1 3)
      (-1 4)  (1 4)
      ; 3 moves to get to eat-position (step turn step)
      (-2 2)  (2 2)
      (-2 3)  (2 3)
      (-3 3)  (3 3)
      )
) 

; Every Turn the agent checks if a predator is getting close. If a predator is 
; in one of these blocks, it can attack in <2 turns. The assumption is that if it is
; this close. it is targetting my agent. Agent is programmed to turn and run if a predator
; is found in these locations
(define closer-predator-percept-coordinates-list '(
      (0 2)  (0 3) 
      (-1 1)  (1 1) 
      (-1 2)  (1 2)
      )
)

; This checks for predators if the agent was moving forward. The agent... shouldn't
; be moving toward predators, and this checks along the "walk path" of the agent to make sure
; the agent doesn't walk closer to a predator
; NOTE: this is conceptual - not being used in practice
(define predator-percept-coordinates-list '(
      ; 1 moves to get to eat-position (step)
      (0 2) (0 3) (0 4) 
      (-1 1)  (1 1) 
      (-1 2)  (1 2)
      (-1 3)  (1 3)
      (-1 4)  (1 4)
      (-2 2)  (2 2)
      (-2 3)  (2 3)
      )
)



; ------------- Global Variables -------------

; Used when the agent wants to save future moves (planning ahead)
(define saved-next-move '())

(define (execute-next-saved-move)
      (let ((temp (car saved-next-move)))
            (set! saved-next-move (cdr saved-next-move) )
            temp
      )
)



; -------------  Useful Helper Functions -------------
; nth-item - get's the nth item from a list
; ASSUMPTION: List is not empty
(define (nth-item n alist)
    (if (= n 1) (car alist) (nth-item (- n 1) (cdr alist))) 
)


(define (get-x-row percept yCoord)
    (nth-item yCoord percept)
)

(define (translate-xCoord-to-index xCoord yCoord)
    (+ (+ xCoord yCoord) 1)
)

; Example Input/Output with the example percept: 
;guile> (get-location example-percept 0 1)
;empty
;guile> (get-location example-percept -1 2)
;(vegetation 2 45)
;guile> (get-location example-percept 3 5)
;barrier
(define (get-location percept xCoord yCoord)
    (nth-item (translate-xCoord-to-index xCoord yCoord) (get-x-row percept yCoord))
)

; -------------  Readiness Probe -------------

; Used by the Agent World to determine the agent is compiled correctly, and ready to receive input
; (essentially a readiness probe)
(define (initialize-agent)
        "OK"
)

; -------------  Coordinate Checkers -------------
;NOTE: All coordinate checker function returns a boolean value

; Checks if the given coordinate is a vegetation
(define (is-vegetation percepts coordinate) 
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (list? coordinate-value)   
                  (equal? (car coordinate-value) 'vegetation)
            )
      )
)

; Checks if the given coordinate is a bloomed vegetation (agent will gain value from eating)
(define (is-valuable-vegetation percepts coordinate) 
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (and 
                        (list? coordinate-value)   
                        (equal? (car coordinate-value) 'vegetation)
                  )
                  (> (nth-item 3 coordinate-value) 0)
            )
      )
)

; Checks if the given coordinate is a barrier
(define (is-barrier percepts coordinate)
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (not (list? coordinate-value))
                  (equal? coordinate-value 'barrier)
            )
      )
)

; Checks if the given coordinate is empty
(define (is-empty percepts coordinate)
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (not (list? coordinate-value))
                  (equal? coordinate-value 'empty)
            )
      )
)

; Checks if the given coordinate is a predator
(define (is-predator percepts coordinate)
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (list? coordinate-value)   
                  (equal? (car coordinate-value) 'predator)
            )
      )
)

; Checks if the given coordinate is a predator
(define (is-agent percepts coordinate)
      (let ((coordinate-value (get-location percepts (car coordinate) (car (cdr coordinate))) ))
            (and 
                  (list? coordinate-value)   
                  (equal? (car coordinate-value) 'agent)
            )
      )
)

; Checks if the coordinate in front of the agent is a valuable vegetation
(define (is-valuable-vegetation-in-front percepts)
      (is-valuable-vegetation percepts '(0 1)) 
)

; Checks if the coordinate in front of the agent is a vegetation
(define (is-vegetation-in-front percepts)
      (is-vegetation percepts '(0 1)) 
)

; -------------  "Last Move" Functions  -------------
; Returns a boolean whether the agent got attacked last turn
(define (is-last-event-attacked previous-events)
      (cond
            (
                  (null? previous-events) 
                  #f
            )
            (
                  (equal? (car (car previous-events)) 'attacked-by)
                  #t
            )
            (
                  #t
                  (is-last-event-attacked (cdr previous-events))
            ) 
      )
)

; Returns a boolean whether the agent moved last turn
(define (is-last-event-move previous-events)
      (cond
            (
                  (null? previous-events) 
                  #f
            )
            (
                  (equal? (car (car previous-events)) 'moved)
                  #t
            )
            (
                  #t
                  (is-last-event-move (cdr previous-events))
            ) 
      )
)


; -------------  Vegetation Logic Functions  -------------
; The function is used to determine if the agent should eat aggressively 
;  -   -   A2  -  - 
;      A1  V  A3
;          ME
; If an Agent exists at location A1 A2 or A3, return a boolean to eat aggressively (else passive)
(define (is-agent-next-to-vegetation percepts)
      (cond 
            (     ;barrier on west of vegetation. eat aggresively
                  (is-agent percepts '(-1 1))  
                        #t
            )    
            (     ;barrier north of vegetation. eat aggresively
                  (is-agent percepts '(0 2))  
                        #t
            )    
            (     ;barrier on east of vegetation. eat aggresively
                  (is-agent percepts '(1 1))  
                        #t
            )    
            (     ; No barriers next to vegetation, safe to eat passively
                  #t
                        #f    
            )    
      )
)

; Determines the series of moves to get in front (facing) the nearest vegetation given its coordinate
(define (determine-move-to-vegetation coordinate)
      (cond 
            (
                  (= (car coordinate) 0)
                  ;(string-append "MOVE-PASSIVE-" (number->string (- (car (cdr coordinate)))))
                  (string-append "MOVE-AGGRESSIVE-" (number->string (- (car (cdr coordinate)))))

            )
            (
                  (= (car (cdr coordinate)) 1)
                  (if (< (car coordinate) 0)
                        (set! saved-next-move '("TURN-LEFT") )
                        (set! saved-next-move '("TURN-RIGHT") )
                  )
                  "MOVE-PASSIVE-1"
            )
            (
                  (= (car (cdr coordinate)) 2)
                  (if (< (car coordinate) 0)
                        (set! saved-next-move '("TURN-LEFT") )
                        (set! saved-next-move '("TURN-RIGHT") )
                  )
                  "MOVE-PASSIVE-2"
            )
            (
                  (= (car (cdr coordinate)) 3)
                  (if (< (car coordinate) 0)
                        (set! saved-next-move '("TURN-LEFT") )
                        (set! saved-next-move '("TURN-RIGHT") )
                  )
                  "MOVE-PASSIVE-3"
            )
            (
                  (= (car (cdr coordinate)) 4)
                  "MOVE-PASSIVE-3"
            )
      
      )

)

; Returns the coordinage of the nearest vegetation 
(define (get-nearby-vegetation-coordinate percepts coordinate-list )
  (cond 
      ( 
            (null? coordinate-list)
                  '()
      )
      (
            (is-vegetation percepts (car coordinate-list))
                  (car coordinate-list)
      )
      (     
            #t
                  (get-nearby-vegetation-coordinate percepts (cdr coordinate-list))
      )
  )
)

; Returns a boolean of whether vegetation is nearby
(define (is-vegetation-nearby percepts coordinate-list)
      (cond 
            ( 
                  (null? coordinate-list)
                        #f
            )
            (
                  (is-valuable-vegetation percepts (car coordinate-list))
                        #t
            )
            (     
                  #t
                        (is-vegetation-nearby percepts (cdr coordinate-list))
            )
      )
)

; Agent will eat a vegetation if it provides energy. Rationale. This agent is reactive to predators,
; and will run when one comes by (highest precedence). Also do not want to count of other agents coming by;
; "Take what you can get"
; If the vegetation is not bloomed (0 energy), keep on a lookout for predators (by turning), and checkup on the vegetation
(define (execute-eat-vegetation-strategy percepts)
      (cond 
            (
                  (is-valuable-vegetation-in-front percepts)
                  (if (is-agent-next-to-vegetation percepts)
                        "EAT-AGGRESSIVE"
                        "EAT-PASSIVE"
                  )
            )
            (
                  #t
                  (set! saved-next-move '("TURN-RIGHT" "TURN-RIGHT" "TURN-RIGHT"))
                  "TURN-RIGHT"
            )
      )
)

; Determine whether an agent is in way of a direct path to the vegetation
(define (is-agent-blocking-path-to-vegetation percepts)
      (     or
            (and (is-vegetation percepts '(0 2)) (is-agent percepts '(0 1)))
            (and (is-vegetation percepts '(0 3)) (is-agent percepts '(0 1)))
      )
)

; Adjust the walking path to a vegetation if an agent is in the way
(define (adjust-path-to-vegetation)
      (set! saved-next-move '("MOVE-PASSIVE-1" "TURN-LEFT") )
      "TURN-RIGHT"
)

; -------------  Predator Logic Functions  -------------

; Returns a boolean is predator is close (in the percept)
(define (is-predator-coming percepts coordinate-list)
      (cond 
            ( 
                  (null? coordinate-list)
                   #f
            )
            (
                  (is-predator percepts (car coordinate-list))
                  #t
            )
            (     
                  #t
                  (is-predator-coming percepts (cdr coordinate-list))
            )
      )
)

; Determine the move when a predator is coming
(define (execute-avoid-predator-strategy)
      (set! saved-next-move '("MOVE-PASSIVE-2") )
      (let ((rand (random 3)))
            (cond ((equal? rand 0) "TURN-RIGHT")
                  ((equal? rand 1) "TURN-LEFT")
                  ((equal? rand 2) "TURN-AROUND")
                  (#f "STAY")
            )
      )
)


; This function will become a buffer for all "MOVE" commands to the agent.
; it will ensure that the agent is not walking toward an agent, instead, it will 
; change the command to a "TURN" to avoid the predator
; NOTE: This is no longer in use! I deemed it too conservative. These cases are 
; already covered by the "is-predator-coming" functions
(define (adjust-move-to-predator percepts coordinate-list original-move)
      (display "is-move-next-to-predator")
      (cond 
            ( 
                  (null? coordinate-list)
                        original-move
            )
            (
                  (is-predator percepts (car coordinate-list))
                  (let ((rand (random 3)))
                        (cond ((equal? rand 0) "TURN-RIGHT")
                              ((equal? rand 1) "TURN-LEFT")
                              ((equal? rand 2) "TURN-AROUND")
                              (#f "STAY")
                        )
                  )
            )
            (     
                  #t
                  (adjust-move-to-predator percepts (cdr coordinate-list) original-move)
            )
      )
)

; This function is called when the agent just got attacked. The agent will determine whether 
; it can run away straight ahead or have to turn and run
(define (determine-run-away-strategy percepts)
      (cond  
            (  
                  (or (or (is-predator percepts '(0 1)) (is-vegetation percepts '(0 1)  )) (is-barrier percepts '(0 3)))
                  "TURN-RIGHT"
            )
            (
                  #t
                  "MOVE-PASSIVE-3"
            )
      )
)


; -------------  Exploration Functions  -------------
; Randomly walk across the map, avoiding borders 
(define (determine-random-move percepts)
      (cond 
            (
                  (is-barrier percepts '(0 2))
                  (let ((rand (random 3)))
                  (cond ((equal? rand 0) "TURN-RIGHT")
                        ((equal? rand 1) "TURN-LEFT")
                        ((equal? rand 2) "TURN-AROUND")
                        (#f "STAY")
                  )
                  )
            )
            (
                  #t
                  (let ((rand (random 6)))
                  (cond ((equal? rand 0) "TURN-RIGHT")
                        ((equal? rand 1) "TURN-LEFT")
                        ((equal? rand 2) "TURN-AROUND")
                        ((equal? rand 3) "MOVE-PASSIVE-1") 
                        ((equal? rand 4) "MOVE-PASSIVE-1") 
                        ((equal? rand 5) "MOVE-PASSIVE-1")
                        (#f "STAY")
                  )
                  )
            )
      )
)

; -------------  Agent-Conservative Functions  -------------
; The agent is run low on energy. It will try its best to walk to the edge of the map and stay there
; until it sees a predator. It will also try not to get stuck on vegetations
(define (execute-agent-conservative-strategy percepts)
      (cond 
            (
                  (is-vegetation percepts '(0 1))
                  (let ((rand (random 3)))
                  (cond ((equal? rand 0) "TURN-RIGHT")
                        ((equal? rand 1) "TURN-LEFT")
                        ((equal? rand 2) "TURN-AROUND")
                        (#f "STAY")
                  )
                  )
            )
            (
                  (is-barrier percepts '(0 1))
                  (set! saved-next-move stay-100-turns)
                  "TURN-AROUND"
            )
            (
                  #t
                  "MOVE-PASSIVE-1"
            )

      )
)


; This is the interface the agent has with the world. This function provides the precedence of the
; agent's decisions (esentially the rules of the agent). To sum it up.. 
;     1. Agent got attacked.. Run!
;     2. Predator is walking to agent.. Run! 
;     3. Agent already planned moves for the future. Trust the agent's planned decisions
;     4. Agent is too low on energy execute conservative strategy to drag out death vs searching for energy (normal strategy)
;     5. Agent is next to vegetation. Eat, or Camp by it. looking out for predators, and waiting for it to bloom
;     6. Agent is blocked by another agent to vegetation, walk around him/her
;     7. Look for and walk to nearby vegetation
;     8. Explore map
(define (choose-action current-energy previous-events percepts)
;(display "-------------------------------------------")(newline)
;(display "current-energy:           ")(display current-energy)(newline)
;(display "previous-events:          ")(display previous-events)(newline)
;(display "Percepts:                 ")(display percepts)(newline)
      (cond 
            (     ; 1. Agent got attacked.. Run!
                  (is-last-event-attacked previous-events)
                  ;(display "!!!ATTACKED \n")
                  (determine-run-away-strategy percepts)
            )

            (     ;2. Predator is walking to agent.. Run! 
                  (is-predator-coming percepts closer-predator-percept-coordinates-list)
                  (execute-avoid-predator-strategy)
            )

            (      ;3. Agent already planned moves for the future. Trust the agent's planned decisions
                  (not (null? saved-next-move))
                  ;(display "!!!Saved Move \n")
                  (execute-next-saved-move)
            )

            (     ;4. Agent is too low on energy execute conservative strategy to drag out death vs searching for energy (normal strategy)
                  (< current-energy 500)
                  ;(display "!!!LOW-ENERGY \n")
                  (execute-agent-conservative-strategy percepts)
                  
            )

            (     ;5. Agent is next to vegetation. Eat, or Camp by it. looking out for predators, and waiting for it to bloom
                  (is-vegetation-in-front percepts)
                  (execute-eat-vegetation-strategy percepts)
            )

            (     ;6. Agent is blocked by another agent to vegetation, walk around him/her
                  (is-agent-blocking-path-to-vegetation percepts)
                  (adjust-path-to-vegetation)
            )
            
            (     ;7. Look for and walk to nearby vegetation
                  (is-vegetation-nearby percepts vegetation-percept-coordinates-list)
                  ;(display "!!!FOOD \n")
                  (determine-move-to-vegetation (get-nearby-vegetation-coordinate percepts vegetation-percept-coordinates-list))
            )

            (     ;8. Explore map
                  #t 
                  ;(display "!!!RANDOM \n")
                  (determine-random-move percepts)
                  
            )     
      )
)