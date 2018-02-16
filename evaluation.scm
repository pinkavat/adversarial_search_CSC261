;; Lab: Adversarial Search
;; CSC 261 
;;
;; File
;;   evaluation.scm
;;
;; Summary
;;   An evaluation function for a mancala player
;;
;; Provides
;;   (best-mancala-eval player)

(module evaluation lang/plt-pretty-big
        (provide best-mancala-eval)
        (require "mancala.scm") ; add any other modules you may need



;; the core of the evaluation function, a list of previously defined
;; heuristics and their associated weights
(define mancala-eval-core

  
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
      0)))

) ; module


