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
;;   Nothing; executes test cases

(require "mancala.scm")
(require "barranca.scm")
(require "evaluation.scm")
(require "alphabeta.scm")
(require "cutoff-minimax.scm")
(require "mancala-player.scm")
(require "game.scm")
(require "general.scm")

;; Mancala evaluation function testing
(define mancala (make-mancala-game)) 

;; Trial 1: pit simple evaluation against our heuristics
(define mancala-player1-eval (best-mancala-eval #t))
(define mancala-player2-eval (simple-mancala-eval #f))

(define mancala-player1 
  (make-alpha-beta-player mancala 3 mancala-player1-eval))
(define mancala-player2 
  (make-alpha-beta-player mancala 3 mancala-player2-eval))

(game-play mancala mancala-player1 mancala-player2)
(display "\n")

;; Alpha-Beta testing
(define barranca (make-barranca-game 3 4))
(define barranca-p1-eval (barranca-utility-fun #t 4))
(display
 "Alpha-Beta search of barranca (3,4): calculated optimal move: 3, Result: ")
(display (alpha-beta-search barranca (game-start-state barranca)
                            3 barranca-p1-eval))
(display "\n")

(define barranca2 (make-barranca-game 5 10))
(define barranca2-p1-eval (barranca-utility-fun #t 10))
(display
 "Alpha-Beta search of barranca (5,10): calculated optimal move: 1, Result: ")
(display (alpha-beta-search barranca2 (game-start-state barranca2)
                            5 barranca2-p1-eval))
(display "\n")


