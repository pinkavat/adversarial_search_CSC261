(module mancala-player lang/plt-pretty-big
	(provide simple-mancala-eval)

;; 
;; Procedure
;;   simple-mancala-eval
;;
;; Purpose
;;   Create a procedure for evaluating a player's mancala state
;;
;; Parameters
;;   player, a boolean
;;
;; Produces
;;   eval-fun, a procedure
;;
;; Preconditions
;;   [None]
;;
;; Postconditions
;;   eval-fun takes a state and returns the number of stones in player's mancala
(define simple-mancala-eval
  (lambda (player)
    (lambda (state)
      (let ((board (cdr state)))
	(list-ref board (if player 6 13))))))

) ; module
