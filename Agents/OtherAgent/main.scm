; On entering a new environment, the function “initialize-agent” will be called 
; with no parameters. The agent will then have 5 seconds in which to initialize 
; its state to prepare to enter the new environment. Your agent must return a 
; string (although the actual value of the string will be ignored by the 
; environment).
(define (initialize-agent)
    "OK")


; Every subsequent turn will then cause a “choose-action” function call to be 
; issued to your agent. This call will have three parameters: 
;       - an integer value representing the energy level of your agent
;       - a list of the events that occurred during the last turn
;       - a list representing the discernible environment
; 
; The environment list will contain five sublists of lengths 3, 5,
; 7, 9, and 11, representing the contents of spaces up to 5 positions away from
; the agent in the direction it is facing.
(define (choose-action current-energy previous-events percepts)
        (just-run previous-events percepts))


; A few rules to walk around. Eat vegetation when we run into it, and run from
; predators when they are in front of us.
; 
; INPUT:
; OUTPUT:
(define (just-run previous-events percepts)
    (let ((rand (random 2)))  ; roll of dice for what we should do
        (cond
            ; We have something in front of us
            ((list? (cadr (car percepts)))
                (cond 
                    ((equal? (car (cadr (car percepts))) 'agent)  ; agent in front of us
                        (cond 
                            ((equal? rand 0) "TURN-RIGHT")
                            ((equal? rand 1) "TURN-LEFT")))
                    ((equal? (car (cadr (car percepts))) 'predator) "TURN-AROUND")  ; predator in front of us
                    ((equal? (car (cadr (car percepts))) 'vegetation)  ; vegetation in front of us
                        (cond 
                            ((equal? (tail (cadr (car percepts))) 0)  ; only eat if it's worth it
                                (cond 
                                    ((equal? rand 0) "TURN-RIGHT")
                                    ((equal? rand 1) "TURN-LEFT")))
                            (#t "EAT-AGGRESSIVE")))))

            ; Move away from the barrier
            ((equal? (car percepts) '(barrier barrier barrier))
                (cond 
                    ((equal? rand 0) "TURN-RIGHT")
                    ((equal? rand 1) "TURN-LEFT")))

            ; We were just attacked ... RUN
            ((was-attacked? previous-events)
                (cond
                    ((list? (cadr (car percepts)))  ; something in front of us
                        (cond 
                            ((equal? rand 0) "TURN-RIGHT")
                            ((equal? rand 1) "TURN-LEFT")))
                    ((list? (nth-item 3 (nth-item 2 percepts)))  "MOVE-AGGRESSIVE-1")  ; something 2 away from us
                    (#t "MOVE-AGGRESSIVE-2")))
            
            ; Predator is somewhere in front of us
            ((list? (nth-item 6 (nth-item 5 percepts)))  ; predator 5 away from us
                (cond 
                    ((equal? (car (nth-item 6 (nth-item 5 percepts))) 'predator) "TURN-RIGHT")
                    (#t "MOVE-PASSIVE-1")))
            ((list? (nth-item 5 (nth-item 4 percepts)))  ; predator 4 away from us
                (cond 
                    ((equal? (car (nth-item 5 (nth-item 4 percepts))) 'predator) "TURN-RIGHT")
                    (#t "MOVE-PASSIVE-1")))
            ((list? (nth-item 4 (nth-item 3 percepts)))  ; predator 3 away from us
                (cond 
                    ((equal? (car (nth-item 4 (nth-item 3 percepts))) 'predator) "TURN-RIGHT")
                    (#t "MOVE-PASSIVE-1")))
            ((list? (nth-item 3 (nth-item 2 percepts)))  ; predator 2 away from us
                (cond 
                    ((equal? (car (nth-item 3 (nth-item 2 percepts))) 'predator) "TURN-RIGHT")
                    (#t "MOVE-PASSIVE-1")))

            ; Move forward if you can
            ((equal? (cadr (car percepts)) 'empty) "MOVE-PASSIVE-1")  ; move forward if you can
            (#t "TURN-RIGHT"))))


; Returns the last element of a list
; 
; INPUT: (tail '(vegetation 1 500))
; OUTPUT: 500
(define (tail list)
    (cond
        ((null? (cdr list)) (car list))
        (#t (tail (cdr list)))))


; Returns the nth item of a list
; 
; INPUT: (nth-item 2 '(a b c d e f))
; OUTPUT: b
(define (nth-item index list)
    (cond ((equal? 1 index) (car list))  ; base case, index is equal to 0, return
        (#t (nth-item (- index 1) (cdr list)))))  ; decrement the index and get tail of list


; Checks if a single event contains whether or not we were attacked
; 
; INPUT: (was-attacked-helper '(attacked-by 1 -50))
; OUTPUT: #t
(define (was-attacked-helper single-event)
    (list? (member 'attacked-by single-event)))


; Returns true if we were just previously attacked
; 
; INPUT: (was-attacked? '((moved 1) (attacked-by 1 -50)))
; OUTPUT: #t
(define (was-attacked? previous-events)
    (list? (member #t (map was-attacked-helper previous-events))))
