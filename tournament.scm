;;
;; File
;;   tournament.scm
;;
;; Authors
;;   Thomas Pinkava
;;     Copy/Pasted stuff together
;;
;; Summary
;;   Contains our humble tournament players
;;
(module tournament lang/plt-pretty-big
(provide pinkavat-mancala-best-player1 pinkavat-mancala-best-player2)
(require "game.scm" "mancala.scm")
  

;; ALPHABETA.SCM


;;
;; Procedure
;;   make-alpha-beta-player
;;
;; Purpose
;;   Create a player using alpha-beta minimax search
;;
;; Parameters
;;   game, a game
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   play, a procedure
;;
;; Preconditions
;;   evaluation-fun takes a state of game and produces a number
;;
;; Postconditions
;;   play is a procedure that takes a state and produces a legal
;;   action for player in game
(define make-alpha-beta-player
  (lambda (game plies evaluation-fun)
    (lambda (state)
      (alpha-beta-search game state plies evaluation-fun))))

;;
;; Procedure
;;   alpha-beta-search
;;
;; Purpose
;;   Return an action using alpha-beta search
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   action, a value
;;
;; Preconditions
;;   plies >= 0
;;   evaluation-fun takes a state of game and produces a number
;;
;; Postconditions
;;   action is a valid action from the state
(define alpha-beta-search
  (lambda (game state plies evaluation-fun)
    (car (alpha-beta-max-value game state
                               -inf.0 +inf.0 
                               0 plies 
                               evaluation-fun))))


    
;;
;; Procedure
;;   alpha-beta-max-value
;;
;; Purpose
;;   Generate the best action-value pair for a state
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   alpha, a number
;;   beta, a number
;;   depth, a number
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   action-value, a pair
;;
;; Preconditions
;;   0 <= depth < plies
;;   evaluation-fun takes a state of game and produces a number
;;
;; Postconditions
;;   (car action-value) is a valid, optimal action from the state
;;      (cdr action-value) is the value of the action
;;
;; Provenance
;;   Procedure adapted from cutoff-minimax.scm, provided
;;   in the lab package by Jerod Weinman.
;;
(define alpha-beta-max-value
  (lambda (game state alpha beta depth plies evaluation-fun)
    (if (or (= depth plies)          ;; If we have reached cutoff or the end
            ((game-terminal? game) state))
        (cons null (evaluation-fun state))

        (let kernel [(successors ((game-successors-fun game) state))
                     (max-pair (cons null -inf.0))
                     (k-alpha alpha)]
          (if (null? successors)
		max-pair
                ;; Recurse while there are still moves to try
                (let* [(min-pair (alpha-beta-min-value
                                  game (cdar successors) k-alpha beta depth plies
                                  evaluation-fun))
                       (val (cdr min-pair))
                       (new-max-pair
                        (if (> val (cdr max-pair))
                            ;; This bit is still weird
                            (cons (caar successors) val)
                            max-pair))]
                  (if (>= (cdr new-max-pair) beta)
                      new-max-pair
                      (kernel (cdr successors)
                              new-max-pair
                              (max k-alpha (cdr new-max-pair))))))))))

                       

    
;;
;; Procedure
;;   alpha-beta-min-value
;;
;; Purpose
;;   Generate the worst action-value pair for a state
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   alpha, a number
;;   beta, a number
;;   depth, a number
;;   plies, a number
;;   evaluation-fun, a procedure
;;
;; Produces
;;   action-value, a pair
;;
;; Preconditions
;;   0 <= depth < plies
;;   evaluation-fun that takes a state of game and produces a number
;;
;; Postconditions
;;   (car action-value) is a valid, optimal action from the state
;;   (cdr action-value) is the value of the action
;;
;; Provenance
;;   Procedure adapted from cutoff-minimax.scm, provided
;;   in the lab package by Jerod Weinman.
;;
(define alpha-beta-min-value
  (lambda (game state alpha beta depth plies evaluation-fun)
    (if (or (= depth plies)          ;; If we have reached cutoff or the end
            ((game-terminal? game) state))
        (cons null (evaluation-fun state))

        (let kernel [(successors ((game-successors-fun game) state))
                     (min-pair (cons null +inf.0))
                     (k-beta beta)]
          (if (null? successors)
		min-pair
                ;; Recurse while there are still moves to try
                (let* [(max-pair (alpha-beta-max-value
                                  game (cdar successors) alpha k-beta
                                  (+ 1 depth) plies
                                  evaluation-fun))
                       (val (cdr max-pair))
                       (new-min-pair
                        (if (< val (cdr min-pair))
                            ;; Introduction of action
                            ;; Recursion getting weird
                            (cons (caar successors) val)
                            min-pair))]
                  (if (<= (cdr new-min-pair) alpha)
                      new-min-pair
                      (kernel (cdr successors)
                              new-min-pair
                              (min k-beta (cdr new-min-pair))))))))))



  ;; EVALUATION.SCM

  ;; sub-helper function to flip board
  ;; takes a state and returns a state from the other player's perspective
  (define flip-board
    (lambda (state)
      (let kernel [(dec state)(acc '())(n 7)]
        (if (= n 0) (append dec acc)
            (kernel (cdr dec)(append acc (list (car dec)))(- n 1))))))


  
  ;; Procedure: 
  ;;   units-in-mancala
  ;; Purpose:
  ;;   returns the number of marbles in p1's mancala
  ;; Produces:
  ;;   marbles, an integer
  ;; Parameters:
  ;;   state, a list
  ;; Preconditions:
  ;;   state is a valid mancala board
  ;; Postconditions:
  ;;   marbles is the value at index 6 of state 
  ;;
  (define units-in-mancala
    (lambda (state)
      (list-ref state 6)))

  ;; The inversion, for the opponent's value
  (define opp-units-in-mancala
    (lambda (state)
      (list-ref state 13)))


  
  ;; Procedure:
  ;;   open-spots
  ;; Purpose:
  ;;   counts the number of open spots on p1's side of the board
  ;; Produces:
  ;;   spots, an integer
  ;; Parameters:
  ;;   state, a list
  ;; Preconditions:
  ;;   state is a valid mancala board
  ;; Postconditions:
  ;;   spots is the number of indices of state between 0 and 5 inclusive
  ;;   whose values are equal to zero
  ;;
  (define open-spots
    (lambda (state)
      (let kernel [(dec state)(count 0)(n 6)]
        (if (= n 0) count
            (kernel (cdr dec)(if (= (car dec) 0) (+ count 1) count)(- n 1))))))

  ;; The inversion, for the opponent's value
  (define opp-open-spots
    (lambda (state)
      (open-spots (flip-board state))))


  ;; Procedure:
  ;;   capture-max-val
  ;; Purpose:
  ;;   compute the maximum number of marbles attainable by
  ;;   effecting a capture move in the given state
  ;; Produces:
  ;;   marbles, a positive integer
  ;; Parameters:
  ;;   state, a list
  ;; Preconditions:
  ;;   state is a valid mancala board
  ;; Postconditions:
  ;;   marbles is the maximum number of the opponent's marbles
  ;;   that may be captured by performing the best marble-capturing
  ;;   move from the provided state
  ;;
  (define capture-max-val
    (lambda (state)
      (let [(capture-value
        ;; Helper to calculate the total captured by each move
    (lambda (state n)
      (let* [(contents (list-ref state n))(target (+ contents n))]
        (if (or (> target 5)
                (not (= 0 (list-ref state target)))
                (= contents 0))
            0
            (list-ref state (- 12 target))))))]
        ;; Helper operates over every available move, maximum returned
      (let kernel [(max-val 0) (n 0)]
        (if (= 5 n)
            max-val
            (kernel (max max-val (capture-value state n)) (+ n 1)))))))
  
  ;; The inversion, for the opponent's value
  (define opp-capture-max-val
    (lambda (state)
      (capture-max-val (flip-board state))))
               

  
  ;; the core of the evaluation function, a list of previously defined
  ;; sub-heuristics and their associated weights expressed as a list of pairs
  (define mancala-eval-core
    (list
     (cons 5 units-in-mancala)
     (cons 5 capture-max-val)
     (cons -1 open-spots)
     (cons -3 opp-units-in-mancala)
     (cons -5 opp-capture-max-val)
     (cons 1 opp-open-spots)
     ))

  
  ;; Procedure
  ;;   best-mancala-eval
  ;;
  ;; Purpose
  ;;   Generates an evaluation heuristic for a mancala player
  ;;
  ;; Parameters
  ;;   player, a boolean value
  ;;
  ;; Produces
  ;;   result, a procedure
  ;;
  ;; Preconditions
  ;;   [None]
  ;;
  ;; Postconditions
  ;;   result is a heuristic function that takes a mancala state
  ;;   and returns an integer.
  ;;
  (define best-mancala-eval
    (lambda (player)
      (lambda (state)
        (* (if player 1 -1)
           (apply + 0
                  (map
                   (lambda (pair) ;; first item  in pair is weight, second is
                     ;; heuristic function 
                     (* (car pair) ((cdr pair) (cdr state))))
                   mancala-eval-core)
                  )))))



(define mancala (make-mancala-game))
  
(define pinkavat-mancala-best-player1
  (make-alpha-beta-player mancala 4 (best-mancala-eval #t)))

(define pinkavat-mancala-best-player2
  (make-alpha-beta-player mancala 4 (best-mancala-eval #f)))

  ) ; module


