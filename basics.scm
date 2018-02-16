;;
;; File
;;   basics.scm
;;
;; Author
;;   Jerod Weinman
;;   Noyce 3825
;;
;; Summary
;;   Collection of standard higher-order procedures and other methods for 
;;   manipulating and evaluating polynomials.
;;
;; Provides
;;   (left-section proc left)
;;   (right-section proc right)
;;   (iota n)
;;   (nest f n)
(module basics lang/plt-pretty-big
	(provide left-section
                 right-section
                 iota
                 nest)

;;
;; Procedure
;;   left-section
;;
;; Purpose
;;   Transform a binary to a unary procedure by fixing the left operand
;;
;; Parameters
;;   op, a procedure
;;   left, a value
;;
;; Produces
;;   unary, a procedure
;;
;; Preconditions
;;   op is a binary procedure
;;   left is a valid (first) parameter to op
;;
;; Postconditions
;;   unary takes one parameter so that 
;;     (unary right)  == (op left right)
(define left-section
  (lambda (op left)
    (lambda (right)
      (op left right))))

;;
;; Procedure
;;   right-section
;;
;; Purpose
;;   Transform a binary to a unary procedure by fixing the right operand
;;
;; Parameters
;;   op, a procedure
;;   right, a value
;;
;; Produces
;;   unary, a procedure
;;
;; Preconditions
;;   op is a binary procedure
;;   right is a valid (second) parameter to op
;;
;; Postconditions
;;   unary takes one parameter so that 
;;     (unary left)  == (op left right)
(define right-section
  (lambda (op right)
    (lambda (left)
      (op left right))))


;;
;; Procedure
;;   iota
;;
;; Purpose
;;   Produce a list of number up to a given value
;;
;; Parameters
;;   n, an integer
;;
;; Produces
;;   result, a list
;;
;; Preconditions
;;   n >= 0
;;
;; Postconditions
;;   result = (0 1 ... n-1)
(define iota
  (lambda (n)
    (let loop ((i (- n 1))   ; Loop from end (n-1) down to zero
               (so-far '())) ; Result so far, build as we loop
      (if (< i 0)
          so-far
          (loop (- i 1)               ; Tail recursive solution combines
                (cons i so-far))))))  ; result so far BEFORE recursive step


;;
;; Procedure
;;   nest
;;
;; Purpose
;;   Functor to compose a procedure with itself an arbitrary number of times
;;
;; Parameters
;;   f, a unary procedure
;;   n, a positive integer
;;
;; Produces
;;   nested, a unary procedure
;;
;; Preconditions
;;   Any result produced by f must be a valid argument to f.
;;
;; Postconditions
;;   nested is a procedure that applies f n times and returns the result.
(define nest
  (lambda (f n)
    (if (= n 1)
        f  ; Base case, nothing to compose. Otherwise
        (compose f (nest f (- n 1)))))) ; Compose f with itself nested n-1 times

) ; module
