;; Lab: Adversarial Search
;; CSC 261 
;;
;; File
;;   driver.scm
;;
;; Summary
;;   Tests the search and evaluation functions over a variety of cases. 
;;
;; Provides
;;   [none]

(require "mancala.scm")
(require "evaluation.scm")
;; (require "alphabeta.scm") ;; not yet implemented
(require "cutoff-minimax.scm")
(require "mancala-player.scm")
(require "game.scm")
(require "general.scm")

(define mancala (make-mancala-game)) 

(define mancala-player1-eval (best-mancala-eval #t))
(define mancala-player2-eval (simple-mancala-eval #f))

(define mancala-player1 
  (make-cutoff-minimax-player mancala 3 mancala-player1-eval))
(define mancala-player2 
  (make-cutoff-minimax-player mancala 3 mancala-player2-eval)) 

(game-play mancala mancala-player1 mancala-player2)
