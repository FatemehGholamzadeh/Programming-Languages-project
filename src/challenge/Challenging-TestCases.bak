#lang racket

;; syntax: https://docs.racket-lang.org/rackunit/api.html

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.
(require rackunit)

(define envList (list (cons "x" (num 2))
					  (cons "y" (num 3))
					  (cons "z" (num 4))
					  (cons "p" (num 5))
					  (cons "q" (num 6))
					  (cons "t" (num 7))
					  (cons "a" (num 8))
					  (cons "b" (num 9))
					  (cons "u" (num 10))
					  (cons "s" (num 11))
					  (cons "bt" (bool #t))
					  (cons "bf" (bool #f))
					  ))

(define tests
  (test-suite
   "Project Tests - Challenging Part"

   ;1
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "f" "a" (plus (var "a") (var "u"))))) 
				(set "u") 
				"compute-free-vars test #1")
					
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "f" "a" (mult (var "t") (var "u")))))
				(set "t" "u") 
				"compute-free-vars test #2")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "f" "a" (neg (mult (var "t") (var "u"))))))
				(set "t" "u") 
				"compute-free-vars test #3")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "a" (var "t"))))
				(set "t") 
				"compute-free-vars test #4")				
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "a" (num 2))))
				(set) 
				"compute-free-vars test #6")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "bt" (bool #t))))
				(set) 
				"compute-free-vars test #6-1")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "a" (iseq (plus (var "x") (var "y")) (neg (var "y"))))))
				(set "y" "x") 
				"compute-free-vars test #7")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (ifnzero (div (var "x") (var "y")) (neg (var "p")) (minus (var "s")(var "t"))))))
				(set "y" "p" "s" "t") 
				"compute-free-vars test #8")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (ifleq (neg (mult (var "x") (var "y"))) (neg (var "p")) (neg (andalso (var "bt")(var "bt"))) (orelse (var "bf")(var "bf"))))))
				(set "y" "p" "bt" "bf") 
				"compute-free-vars test #9")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "y" "x" (apply (lam "z" "p" (ifneq (neg (mult (var "x") (var "y"))) (neg (var "p")) (plus (var "s")(var "x")) (plus (var "s")(var "z")))) (var "p")))))
				(set "s" "p") 
				"compute-free-vars test #10")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam "y" "x" (lam "z" "p" (ifleq (neg (mult (var "x") (var "y"))) (neg (var "p")) (plus (var "s")(var "x")) (plus (var "s")(var "z")))))))
				(set "s") 
				"compute-free-vars test #11")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (apair (var "a")(var "b")))))
				(set "a" "b") 
				"compute-free-vars test #12")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (1st (apair (var "a")(var "x"))))))
				(set "a") 
				"compute-free-vars test #13")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (2nd (1st (apair (apair (var "a")(var "b"))(munit)))))))
				(set "a" "b") 
				"compute-free-vars test #14")	

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (ismunit (1st (apair (apair (var "a")(var "x"))(munit)))))))
				(set "a") 
				"compute-free-vars test #15")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (ismunit (var "a")))))
				(set "a") 
				"compute-free-vars test #16")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (munit))))
				(set) 
				"compute-free-vars test #17")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (with "y" (num 2) (plus (var "x")(var "y"))))))
				(set) 
				"compute-free-vars test #18")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (with "y" (num 2) (with "x" (num 3) (plus (var "x")(var "y")))))))
				(set) 
				"compute-free-vars test #19")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (with* (list (cons "y" (num 2)) (cons "p" (num 3))) (plus (mult (var "p")(var "q"))(plus (var "x")(var "y")))))))
				(set "q") 
				"compute-free-vars test #20")
				
					
(check-equal? 	(closure-env (eval-exp-c (with* (list (cons "x" (num 1)) (cons "y" (num 2))) 
				(lam "f" "y" (plus (var "x") (var "y"))))))
				(list (cons "x" (num 1)))
				"eval-exp-c test #1")
		
(check-equal? 	(closure-env (eval-exp-c (with* (list (cons "w" (num 3)) (cons "x" (num 1)) (cons "y" (num 2))) 
				(lam "f" "y" (plus (var "x") (var "y"))))))
				(list (cons "x" (num 1)))
				"eval-exp-c test #2")		
				
(check-equal? 	(closure-env (eval-exp-c (with* (list (cons "w" (num 3)) (cons "x" (num 1)) (cons "y" (num 2))) 
				(lam "f" "y" (plus (var "x") (var "y"))))))
				(list (cons "x" (num 1)))
				"eval-exp-c test #3")
				

;(check-equal? 	(closure-env (eval-exp (with* (list (cons "w" (num 3)) (cons "x" (num 1)) (cons "y" (num 2))) 
;				(lam "f" "y" (plus (var "x") (var "y"))))))
;				(list (cons "y" (num 2)) (cons "x" (num 1)) (cons "w" (num 3)))
;				"eval-exp-c test #4")	


(check-equal? 	(closure-env (eval-exp-c (lam null "x" (with "y" (num 2) (with "x" (num 3) (plus (var "x")(var "y")))))))
				'() 
				"eval-exp-c test #4")	
				
(check-equal? 	(closure-env (eval-exp-c (with* (list (cons "u" (num 3)) (cons "w" (num 2)))(lam "f" "a" (plus (var "a") (var "u"))))))
				(list (cons "u" (num 3)))
				"eval-exp-c test #5")
					
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam "f" "a" (mult (var "t") (var "u"))))))
				(list (cons "u" (num 10)) (cons "t" (num 7))) ;OR any permutation
				"eval-exp-c test #6")
					
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam "f" "a" (plus (apply (lam null "a" (neg (var "a"))) (num 2)) (var "b"))))))
				(list (cons "b" (num 9)))
				"eval-exp-c test #7")					
					
					
					
					
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam "f" "a" (neg (mult (var "t") (var "u")))))))
				(list (cons "u" (num 10)) (cons "t" (num 7))) ;OR any permutation
				"eval-exp-c test #8")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "a" (var "t")))))
				(list (cons "t" (num 7)))
				"eval-exp-c test #9")				
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "a" (num 2)))))
				'() 
				"eval-exp-c test #10")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "a" (iseq (plus (var "x") (var "y")) (neg (var "y")))))))
				(list (cons "y" (num 3)) (cons "x" (num 2))) ;OR any permutation
				"eval-exp-c test #11")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (ifnzero (mult (var "x") (var "y")) (neg (var "p")) (plus (var "s")(var "t")))))))
				(list (cons "s" (num 11)) (cons "t" (num 7))(cons "p" (num 5))(cons "y" (num 3))) ;OR any permutation
				"eval-exp-c test #12")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (ifleq (neg (mult (var "x") (var "y"))) (neg (var "p")) (plus (var "s")(var "x")) (plus (var "s")(var "z")))))))
				
				(list (cons "s" (num 11))(cons "p" (num 5)) (cons "z" (num 4))(cons "y" (num 3))) ;OR any permutation
				"eval-exp-c test #9")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam "y" "x" (apply (lam "z" "p" (ifleq (neg (mult (var "x") (var "y"))) (neg (var "p")) (plus (var "s")(var "x")) (plus (var "s")(var "z")))) (var "p"))))))
				
				(list (cons "s" (num 11))(cons "p" (num 5))) ;OR any permutation
				"eval-exp-c test #10")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam "y" "x" (lam "z" "p" (ifleq (neg (mult (var "x") (var "y"))) (neg (var "p")) (plus (var "s")(var "x")) (plus (var "s")(var "z"))))))))
				
				(list (cons "s" (num 11)))
				"eval-exp-c test #11")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (apair (var "a")(var "b"))))))
				
				(list (cons "b" (num 9))(cons "a" (num 8)) ) ;OR any permutation
				"eval-exp-c test #12")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (1st (apair (var "a")(var "x")))))))
				
				(list (cons "a" (num 8)))
				"eval-exp-c test #13")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (2nd (1st (apair (apair (var "a")(var "b"))(munit))))))))
				(list (cons "b" (num 9))(cons "a" (num 8)) ) ;OR any permutation
				"eval-exp-c test #14")	

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (ismunit (1st (apair (apair (var "a")(var "x"))(munit))))))))
				(list (cons "a" (num 8)))
				"eval-exp-c test #15")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (ismunit (var "a"))))))
				(list (cons "a" (num 8)))
				"eval-exp-c test #16")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (munit)))))
				'()
				"eval-exp-c test #17")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (with "y" (num 2) (plus (var "x")(var "y")))))))
				'()
				"eval-exp-c test #18")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (with "y" (num 2) (with "x" (num 3) (plus (var "x")(var "y"))))))))
				'()
				"eval-exp-c test #19")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (with* (list (cons "y" (num 2)) (cons "p" (num 3))) (plus (mult (var "p")(var "q"))(plus (var "x")(var "y"))))))))
				(list (cons "q" (num 6)))
				"eval-exp-c test #20")
					
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (with "f1"
                               (lam "f1" "a" (with "x" (var "a") (lam "f2" "z" (plus (var "x") (var "s")))))
                               (with "f3" (lam "f3" "f" (with "x" (num 1729) (apply (var "f") (munit)))) 
                                     (apply (var "f3") (apply (var "f1") (var "p")))))))))
				(list (cons "s" (num 11))(cons "p" (num 5)) );OR any permutation
				"eval-exp-c test #21")
	
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" (with "fnc"
       (lam "f1" "x" 
            (ifleq (ismunit (var "x")) (var "p") 
                       (num 0) 
                       (plus (1st (var "x")) (apply (var "f1") (2nd (var "x"))))))
       (apply (var "fnc") (apair (var "s") (apair (var "s") (apair (var "b") (munit))))))))))
				
				(list  (cons "s" (num 11)) (cons "b" (num 9))(cons "p" (num 5)));OR any permutation
				"eval-exp-c test #22")	
				
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" 				
     (with "range"
           (lam "range" "lo"
                (lam null "hi"
                     (ifleq (var "t") (var "hi") (munit)
                                (apair (var "lo") (apply (apply (var "range") (plus (num 1) (var "p"))) (var "hi"))))))
           (apply (apply (var "u") (num 5)) (num 8)))))))
		   (list  (cons "u" (num 10)) (cons "t" (num 7))(cons "p" (num 5)));OR any permutation
				"eval-exp-c test #23")

(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "x" 	
(apply (lam "a" "b" (ifleq (var "b") (var "u") (plus (var "b") (num 3))
                         (apply (var "a") (mult (var "t") (num 3))  ))) (var "s"))))))
			(list  (cons "s" (num 11))(cons "u" (num 10)) (cons "t" (num 7)));OR any permutation
			"eval-exp-c test #24")
			
			
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "a" (neg (var "a"))))))
				'() 
				"eval-exp-c test #25")				
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "a" (plus (ismunit (var "b")) (var "c"))))))
				(list  (cons "b" (num 9))) 
				"eval-exp-c test #26")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (lam null "x" (plus (ismunit (var "b")) (var "c")))))
				(set "b" "c") 
				"compute-free-vars test #26-2")
				
(check-equal? 	(closure-env (eval-exp-c (with* envList (lam null "a" (ifmunit (var "b") (var "p")(var "t"))))))
				(list  (cons "b" (num 9)) (cons "t" (num 7))(cons "p" (num 5)));OR any permutation
				"eval-exp-c test #27")				
				
   ))

(require rackunit/text-ui)
(require rackunit/log)
;; runs the test
;(run-tests tests)


(define result (run-tests tests))

(define out (open-output-file "grade-Challenging.txt" #:exists 'replace))
(pretty-write (- 50 result) out #:newline? #t)
(pretty-write (test-log) out #:newline? #f)
(close-output-port out)

;(define out2 (open-output-file "summary.txt" #:exists 'replace))
;(write (test-log) out2)
;(close-output-port out2)
