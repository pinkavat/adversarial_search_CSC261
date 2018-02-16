;;
;; File
;;   barranca.scm
;;
;; Author
;;   Jerod Weinman
;;
;; Summary
;;   Routine(s) for modeling a two player barranca game
;;
;; Provides
;;   (make-barranca-game numbers target)
;;   (barranca-utility-fun player target)
;;
;; Barranca state: 
;;   (next-player player-1-numlist player-2-numlist available-numlist)

(module barranca lang/plt-pretty-big
	(provide make-barranca-game barranca-utility-fun)
	(require "basics.scm" "game.scm")


(define make-barranca-game
  (lambda (numbers target)
    (make-game
     (barranca-successor-fun numbers)
     barranca-terminal?
     (barranca-win? target)
     (barranca-start-state numbers)
     barranca-display-cards)))

;-------------------------------------------------------------------------------
; No user serviceable parts below ...
;-------------------------------------------------------------------------------

;; Successor function, gives a list of action-state pairs. Actions are
;; numbers available and states are lists of the form given above.

(define barranca-successor-fun
  (lambda (numbers)
    (lambda (state)
      (let* ((player (car state))
             (nextplayer (not player))
             (p1-list (cadr state))
             (p2-list (caddr state))
             (num-list (cadddr state))
             (add-play (lambda (num rem-nums)
                         (list nextplayer
                               (if player
                                   (cons num p1-list)
                                   p1-list)
                               (if (not player)
                                   (cons num p2-list)
                                   p2-list)
                               rem-nums))))
        (let loop ((rem-nums (list-ref state 3)))
          (if (null? rem-nums)
              null
              (cons 
               (cons (car rem-nums) ; Action
                     (add-play (car rem-nums)
                               (filter (right-section (compose not =) 
						      (car rem-nums))
                                       num-list)))
               (loop (cdr rem-nums)))))))))

;; Terminal test (no available numbers)
(define barranca-terminal?
  (lambda (state)
    (null? (cadddr state))))

;; Win test, measure whether player's numbers have a product with a lower
;; absolute difference than the opponent's for a given target
(define barranca-win?
  (lambda (target)
    (lambda (player state)
      (let ((diff-1 (abs (- target (apply * (cadr state)))))
	    (diff-2 (abs (- target (apply * (caddr state))))))
	(if player
	    (< diff-1 diff-2)
	    (< diff-2 diff-1))))))

;; Start state, player 1 plays, no one with numbers, and all numbers available
(define barranca-start-state
  (lambda (numbers)
    (list #t null null (map (right-section + 1) (iota numbers)))))

;; Display function: Each player's current product
(define barranca-display-cards
  (lambda (state)
    (display "Player 1: ") (display (apply * (cadr state))) (newline)
    (display "Player 2: ") (display (apply * (caddr state))) (newline)))

;; Utility function for a player given a target, +1/0/-1 for win, draw, loss
(define barranca-utility-fun
  (lambda (player target)
    (lambda (state)
      (cond 
       ((not (barranca-terminal? state)) ; Plays remain: value 0
        (display "WARNING! Utility function applied to non-terminal state.")
        0)
       (((barranca-win? target) player state)
        1)
       (((barranca-win? target) (not player) state)
        -1)
       (else ; draw
        0)))))
) ; module
