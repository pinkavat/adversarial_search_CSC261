;;
;; File
;;   alphabeta.scm
;;
;; Authors
;;   Jerod Weinman
;;     Wrote all but the implementations of alpha-beta-max-value and 
;;      alpha-beta-min-value
;;   Thomas Pinkava
;;     Adapted alpha-beta-max-value and alpha-beta-min-value
;;
;; Summary
;;   Implementation of minimax search using alpha-beta pruning
;;
;; Provides
;;   (make-alpha-beta-player game plies evaluation-fun)
;;   (alpha-beta-search game state plies evaluation-fun)
;;   (alpha-beta-max-value game state alpha beta depth plies eval-fun)
;;   (alpha-beta-min-value game state alpha beta depth plies eval-fun)
(module alphabeta lang/plt-pretty-big
	(provide make-alpha-beta-player
                 alpha-beta-search
                 alpha-beta-max-value
                 alpha-beta-min-value)
	(require "game.scm")


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

) ; module
