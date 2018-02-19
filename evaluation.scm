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

  ) ; module


