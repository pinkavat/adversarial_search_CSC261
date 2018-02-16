;;
;; File
;;   minimax.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Provides a collection of routines implementing a MiniMax adversarial search
;;
;; Provides

;;   (make-minimax-player game utility)
;;   (minimax-search game state utility)
;;   (max-value game state utility)
;;   (min-value game state utility)
(module minimax lang/plt-pretty-big
	(provide make-minimax-player
                 minimax-search
                 max-value
                 min-value)
	(require "game.scm" "general.scm")

;; Set DO-DISPLAY to #t to turn on intermediate displays of values
;; while running a minimax search
(define DO-DISPLAY #f)

(define dprintf
  (lambda (format . args)
    (when DO-DISPLAY
      (apply printf (cons format args)))))

;;
;; Procedure
;;   make-minimax-player
;;
;; Purpose
;;   Create a player using minimax search
;;
;; Parameters
;;   game, a game
;;   utility, a procedure
;;
;; Produces
;;   play, a procedure
;;
;; Preconditions
;;   utility is a procedure that takes a state and produces a number
;;
;; Postconditions
;;   play is a procedure that takes a state and produces a legal action for game
(define make-minimax-player
  (lambda (game utility)
    (lambda (state)
      (minimax-search game state utility))))


;;
;; Procedure
;;   minimax-search
;;
;; Purpose
;;   Return an action using minimax search
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   utility, a procedure
;;
;; Produces
;;   action, a value
;;
;; Preconditions
;;   utility is a procedure that takes a state and produces a number
;;
;; Postconditions
;;   action is a valid action from the given state
;;   action obeys the MINIMAX rule of AIMA (3/e), p. 164
(define minimax-search
  (lambda (game state utility)
    (car (max-value game state utility))))



;;
;; Procedure
;;   max-value
;;
;; Purpose
;;   Generate the best action-value pair for a state
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   utility, a procedure
;;
;; Produces
;;   action-value, a pair
;;
;; Preconditions
;;   utility is procedure that produces a number for any value where
;;       ((game-terminal? game) state) is true
;;
;; Postconditions
;;   (car action-value) is a valid, optimal action from state
;;   (cdr action-value) is the value of the action
(define max-value
  (lambda (game state utility)

    (dprintf "MAX state ~a~n" state)
    
    (cond
     [((game-terminal? game) state)  ; We are given a terminal state

      ;; Some intermediate display material
      (if DO-DISPLAY ((game-display-fun game) state))
      (dprintf "MAX utility ~a~n" (utility state))
      
      ;; Because this state is terminal, there is no action to take
      ;; We return the value as the utility of the state
      (cons null (utility state))] ; action-value pair
     
     [else
      ;; In this case, we do not have a terminal state, therefore
      ;; successors ought not be empty

      ;; Loop to find find maximimum value and the action giving it
      
      (let loop ([successors ((game-successors-fun game) state)]
		 ; successors is a list of (action . state) pairs
		 [argmax null]  ; maximizing action
		 [maxval null]) ; maximal value

	(if (null? successors)   ; No more moves (successors)

	    (if (null? maxval)   ; No successors at all! Error
		(error "Game produced no successors from non-terminal state" 
		       state)
		(cons argmax maxval)) ; Otherwise, return best action-value pair

	    ; Process next successor
	    (let* ([action-value (min-value game 
					    (cdar successors) ; successor state
					    utility)]
		   [val (cdr action-value)]) ; value of the action-value pair
	      
	      (cond 
	       [(or (null? argmax)     ; haven't calculated any values yet, or
		    (> val maxval))    ; this move's value is an improvement
		(loop (cdr successors)   ; remaining possibilities
		      (caar successors)  ; new argmax action
		      val)]              ; new maxval value
	       [else
		(loop (cdr successors)   ; remaining possibilities
		      argmax             ; same argmax action
		      maxval)]))))])))   ; same maxval value

			 

;;
;; Procedure
;;   min-value
;;
;; Purpose
;;   Generate the worst action-value pair for a state
;;
;; Parameters
;;   game, a game
;;   state, a value
;;   utility, a procedure
;;
;; Produces
;;   action-value, a pair
;;
;; Preconditions
;;   utility is procedure that produces a number for any value where
;;       ((game-terminal? game) state) is true
;;
;; Postconditions
;;   (car action-value) is a valid, optimal action from the state
;;   (cdr action-value) is the value of the action
(define min-value
  (lambda (game state utility)

    (dprintf "MIN state ~a~n" state)
    
    (cond
     [((game-terminal? game) state)  ; We are given a terminal state
      
      ;; Some intermediate display material
      (if DO-DISPLAY ((game-display-fun game) state))
      (dprintf "MIN utility ~a~n" (utility state))
      
      ;; Since this state is terminal, there is no action to take
      ;; We return the value as the utility of the state
      (cons null (utility state))] ; action-value pair
     
     [else
      ;; In this case, we do not have a terminal state, therefore
      ;; successors ought not be empty
      
      ;; Loop to find find minimum value and the action giving it
      
      (let loop ([successors ((game-successors-fun game) state)]
		; successors is a list of (action . state) pairs
		 [argmin null]  ; minimizing action
		 [minval null]) ; minimal value
		 
	(if (null? successors)   ; No more moves

	    (if (null? minval)   ; No successors at all! Error
		(error "Game produced no successors from non-terminal state" 
		       state)
		(cons argmin minval)) ; Otherwise, return best action-state pair

	    (let* ([action-value (max-value game 
					    (cdar successors) ; successor state
					    utility)]
		   [val (cdr action-value)]) ; value of the action-value pair
	      
	      (cond 
	       [(or (null? argmin)     ; haven't calculated any values yet, or
		    (< val minval))    ; this move's value is an improvement
		(loop (cdr successors)   ; remaining possibilities
		      (caar successors)  ; new argmin
		      val)]              ; new minval
	       [else
		(loop (cdr successors)   ; remaining possibilities
		      argmin             ; same argmin
		      minval)]))))])))   ; same minval
) ; module
