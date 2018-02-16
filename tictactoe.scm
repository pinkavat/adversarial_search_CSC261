;;
;; File
;;   tictactoe.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Routine(s) for modeling a two player tic-tac-toe game
;;
;; Provides
;;   (make-tictactoe-game)
;;   (tictactoe-utility-fun player)
(module mancala lang/plt-pretty-big
	(provide make-tictactoe-game
                 tictactoe-utility-fun)
	(require "game.scm" "general.scm")


;; Create a game type object for Tic Tac Toe using the procedures below
(define make-tictactoe-game
  (lambda ()
    (make-game
     tictactoe-successor-fun
     tictactoe-terminal?     
     tictactoe-win?
     tictactoe-start-state
     (lambda (state) (tictactoe-display-board (cdr state))))))

;-------------------------------------------------------------------------------
; No user serviceable parts below
;-------------------------------------------------------------------------------

;; Successor function, gives a list of action-state pairs. Actions are
;; indices (0-8) to available slots and states are column-major order
;; eight-element lists of null or booleans representing the player
;; occupying the slot
(define tictactoe-successor-fun
  (let* ((add-play (lambda (board sym slot)
                     (let loop ((index 0)
                                (board board))
                       (cond
                        ((= index 9)
                         null)
                        ((= index slot)
                         (cons sym (cdr board)))
                        (else
                         (cons (car board)
                               (loop (+ 1 index) (cdr board))))))))
         (add-plays (lambda (board sym)
		      (let loop ((index 0))
			(cond
			 ((= index 9) ; No more indices to test
			  null)
			 ((null? (list-ref board index)) ; No play here yet
			  (cons (cons index ; action
				      (add-play board sym index))  ; state
				(loop (+ 1 index))))
			 (else
			  (loop (+ 1 index))))))))
    (lambda (state)
      (map (lambda (action-board) 
	     (cons (car action-board) 
		   (cons (not (car state)) 
			 (cdr action-board)) ))
           (add-plays (cdr state) (car state))))))


;; start state value, player 1's play with an empty board
(define tictactoe-start-state
  (cons #t (list null null null null null null null null null)))

;; Determine whether a particular player has one for the given state
(define tictactoe-win? 
  (lambda (player state)
    (let ((board (cdr state))
          (win? (lambda (a b c)
                  (and (equal? a player)
                       (equal? b player)
                       (equal? c player)))))
      (or 
       (win? (list-ref board 0) (list-ref board 1) (list-ref board 2))
       (win? (list-ref board 3) (list-ref board 4) (list-ref board 5))
       (win? (list-ref board 6) (list-ref board 7) (list-ref board 8))
       (win? (list-ref board 0) (list-ref board 3) (list-ref board 6))
       (win? (list-ref board 1) (list-ref board 4) (list-ref board 7))
       (win? (list-ref board 2) (list-ref board 5) (list-ref board 8))
       (win? (list-ref board 0) (list-ref board 4) (list-ref board 8))
       (win? (list-ref board 2) (list-ref board 4) (list-ref board 6))))))

;; Simple +1/-1/0 utility function functor for a specified player
(define tictactoe-utility-fun
  (lambda (player)
    (lambda (state)
      (cond 
       ((not (tictactoe-terminal? state)) ; Plays remain: value 0
        (display "WARNING! Utility function applied to non-terminal state.")
        0)
       ((tictactoe-win? player state) 
        1)
       ((tictactoe-win? (not player) state) 
        -1)
       (else ; draw
        0)))))

;; Helper function to display a board with the common X and O symbols
(define tictactoe-display-board
  (lambda (board)
    (let ((slot-disp (lambda (val)
                       (cond
                        ((null? val) "-")
                        (val         "X")
                        (else        "O")))))
      (newline)
      (display (slot-disp (list-ref board 0))) (display " ")
      (display (slot-disp (list-ref board 3))) (display " ")
      (display (slot-disp (list-ref board 6))) (display " ") (newline)
      (display (slot-disp (list-ref board 1))) (display " ")
      (display (slot-disp (list-ref board 4))) (display " ")
      (display (slot-disp (list-ref board 7))) (display " ") (newline)
      (display (slot-disp (list-ref board 2))) (display " ")
      (display (slot-disp (list-ref board 5))) (display " ")
      (display (slot-disp (list-ref board 8))) (display " ") (newline))))

;; Predicate for identifying a terminal state
(define tictactoe-terminal?
  (lambda (state)
    (or 
     (not (any? (map null? (cdr state)))) ; No empty slots
     (tictactoe-win? #t state)            ; Player 1 has won
     (tictactoe-win? #f state))))         ; Player 2 has won

) ; module
