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


;; sub-helper function to flip board
(define flip-board
  (lambda (state)
    (let kernel [(dec state)(acc '())(n 7)]
      (if (= n 0) (append dec acc)
          (kernel (cdr dec)(append acc (list (car dec)))(- n 1))))))

;; helper functions to get number of stones in mancala
(define units-in-mancala
  (lambda (state)
    (list-ref state 6)))
(define opp-units-in-mancala
  (lambda (state)
    (list-ref state 13)))

;; helper functions to get the number of empty spots
(define open-spots
  (lambda (state)
    (let kernel [(dec state)(count 0)(n 6)]
      (if (= n 0) count
          (kernel (cdr dec)(if (= (car dec) 0) (+ count 1) count)(- n 1))))))
(define opp-open-spots
  (lambda (state)
    (open-spots (flip-board state))))


;; function to calculate max gain from anticipated captures 
(define capture-max-val
  (lambda (state)
    (let kernel [(max-val 0) (n 0) ]
      (if (= 5 n)
          max-val
          (kernel (max max-val (capture-value state n)) (+ n 1))))))
(define opp-capture-max-val
  (lambda (state)
    (capture-max-val (flip-board (state)))))

;; sub-helper for capture-max-val
(define capture-value
  (lambda(state n)
    (let* [(contents (list-ref state n))(target (+ contents n))]
      (if (or (> target 5)
              (not (= 0 (list-ref state target)))
              (= contents 0))
          0
          (list-ref state (- 12 target))))))
               
  
;; the core of the evaluation function, a list of previously defined
;; heuristics and their associated weights expressed as a list of pairs
(define mancala-eval-core
  (list
   (cons 3 units-in-mancala)
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
      (apply + 0
            (map
             (lambda (pair) ;; first item  in pair is weight, second is
                            ;; heuristic function 
               (* (car pair) ((cdr pair) state)))
             mancala-eval-core)
            ))))

) ; module


