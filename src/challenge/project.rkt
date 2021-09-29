;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions


(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application


(struct munit    ()     #:transparent)   ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 

(struct bool (boolean) #:transparent) ;; a bool constant, e.g., (bool #t)
(struct mult     (e1 e2)               #:transparent)   ;; multiply two numbers
(struct neg      (e)                   #:transparent)   ;; neg of a number
(struct minus  (e1 e2)  #:transparent)  ;; subtract two expressions
(struct div  (e1 e2)  #:transparent)  ;; divide two expressions
(struct andalso  (e1 e2) #:transparent)   ;; logical conjunction
(struct orelse  (e1 e2) #:transparent)   ;; logical disjunction
(struct cnd  (e1 e2 e3) #:transparent)   ;; if e1 then e2 else e3 (Only one of and is evaluated)
(struct iseq  (e1 e2)  #:transparent)  ;; compare two expressions
(struct ifnzero  (e1 e2 e3)  #:transparent)  ;; if e1 not zero then e2 else e3
(struct ifleq  (e1 e2 e3 e4) #:transparent)   ;; if e1>e2 then e4 else e3 (Only one of e3 and e4 is evaluated) 
(struct with  (s e1 e2) #:transparent)   ;; s=e1 in e2
(struct apair (e1 e2) #:transparent)   ;; make a new pair
(struct 1st  (e1)  #:transparent)   ;; get first part of a pair
(struct 2nd (e1)  #:transparent)   ;; get second part of a pair

(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r
(struct letrec (s1 e1 s2 e2 e3) #:transparent) ;; in e3 replace s1 with e1 and s2 with e2




;; Problem 1

(define (racketlist->numexlist xs) (if (null? xs)
                                       (munit)
                                       (apair (car xs) (racketlist->numexlist (cdr xs)))))

(define (numexlist->racketlist xs) (if (munit? xs)
                                       '()
                                       (cons (apair-e1 xs)(numexlist->racketlist (apair-e2 xs)))))
;; Problem 2

;; lookup a variable in an environment
;; Complete this function
;;envlookup search in a racketlist
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (getvalue str rec)
  (cond [(equal? (key-s (record-k rec)) str) (key-e (record-k rec))]                          
        [(munit? (record-r rec)) (munit)]
        [#t (getvalue str (record-r rec))]))
      

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]
        
        [(num? e)
         (if (integer? (num-int e))
             e
             (error "The argument given is incompatible"))]


        [(closure? e) e]

        
        [(lam? e) (closure env e)]

        
        [(apply? e)
         (let ([fClosure (eval-under-env (apply-funexp e) env)]
               [act-p (eval-under-env (apply-actual e) env)])
           (if (or (closure? fClosure) (lam? fClosure))
               (if (lam? fClosure)
                   (let* ([lamClosure (eval-under-env fClosure env)]
                          [func (closure-f lamClosure)])
                     (if (lam? func)
                           (eval-under-env (lam-body func) (cons (cons (lam-formal func) act-p) (cons (cons (lam-nameopt func) lamClosure) (closure-env lamClosure))))
                         (error "Application must be performed on a function")
                      ))
                      (let ([func (closure-f fClosure)])
                           (if (lam? func)
                               (let* ([ext-env-tmp1 (cons (cons (lam-formal func) act-p) (closure-env fClosure))]
                                     [ext-env (cons (cons (lam-nameopt func) fClosure) ext-env-tmp1)])
                                  (eval-under-env (lam-body func) ext-env))
                               (error "Application must be performed on a function")))
                     ) 
               (error "First argument must be a closure")))]
        
        
        [(munit? e)  (munit)]
        
        [(ismunit? e)
          (let ([v (eval-under-env (ismunit-e e) env)])
            (if (munit? v) (bool #t) (bool #f)))]
        

        
        [(bool? e)
              (if (boolean? (bool-boolean e)) e (error "NUMEX ERROR IN Bool: BAD ARGUMENT IN " e))]
        
        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multipication applied to non-number")))]
        
        [(div? e) 
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (equal? (num-int v2) 0)
                   (error "Error for devision by 0")
                   (num (quotient (num-int v1)(num-int v2))))

               (error "NUMEX division applied to non-number")))]
        
        [(neg? e) 
         (let ([v1 (eval-under-env (neg-e e) env)])
           (cond
                 [(bool? v1) (if(false? (bool-boolean v1)) (bool #t) (bool #f))]

                 [(num? v1) (num (- (num-int v1)))]
                 
                 [#t (error "NUMEX negation applied to non-number-or-boolean")]))]
        
        [(andalso? e)
            (let ([v1 (eval-under-env (andalso-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #f) (bool #f)
                                  (let ([v2 (eval-under-env (andalso-e2 e) env)])
                                          (if (bool? v2) v2 (error "The second argument is not boolean"))))
                  (error "The first argument is not boolean")))]
        
        [(orelse? e)
            (let ([v1 (eval-under-env (orelse-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #t) (bool #t)
                                 (let ([v2 (eval-under-env (orelse-e2 e) env)])
                                   (if (bool? v2) v2 (error "The second argument is not boolean"))))
                  (error "The first argument is not boolean")))]
        
        [(cnd? e)
          (let ([v1 (eval-under-env (cnd-e1 e) env)])

            (if (bool? v1)
                (if(equal? (bool-boolean v1) #t) (eval-under-env (cnd-e2 e) env) (eval-under-env (cnd-e3 e) env))
                (error "The first argument is not boolean"))
            )]
        
        [(iseq? e) 
         (let ([x (eval-under-env (iseq-e1 e) env)]
               [y (eval-under-env (iseq-e2 e) env)])
         (cond [(and (num? x)(num? y))
               (if (equal? (num-int x)  (num-int y)) (bool #t) (bool #f))                    
               ]
             
               [(and (bool? x)(bool? y))
              
               (if(equal? (bool-boolean x)   (bool-boolean y)) (bool #t) (bool #f))       
               ]
               [#t (bool #f)]))
          ]
        
        [(ifleq? e)
           (let ([v1 (eval-under-env (ifleq-e1 e) env)]
                 [v2 (eval-under-env (ifleq-e2 e) env)]
                 )
             (if (and (num? v1) (num? v2)) (if (> (num-int v1) (num-int v2)) (eval-under-env (ifleq-e4 e) env) (eval-under-env (ifleq-e3 e) env)
                                            ) (error "The argument is not boolean"))
           )]
        
        [(ifnzero? e)
           (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
             (if (num? v1)
                 (if (equal? (num-int v1)  0)
                     (eval-under-env (ifnzero-e3 e) env)
                     (eval-under-env (ifnzero-e2 e) env))
                 (error "The first argument is not number")))]

        [(with? e)
            (let* ([v (eval-under-env (with-e1 e) env)]
            [ext-env (cons (cons (with-s e) v) env)])
            (eval-under-env (with-e2 e) ext-env))]
        
        [(apair? e)
            (let ([v1 (eval-under-env (apair-e1 e) env)]
              [v2 (eval-under-env (apair-e2 e) env)])
               (apair v1 v2))]

 
          [(1st? e)
           (let ([v (eval-under-env (1st-e1 e) env)])
             (if (apair? v)
                 (apair-e1 v)
                 (error "The first is only valid on a pair:(")))]
         

          [(2nd? e)
          (let ([v (eval-under-env (2nd-e1 e) env)])
            (if (apair? v)
                (apair-e2 v)
                (error "The second is only valid on a pair:(")))]

          


          [(letrec? e)
            (let* ([ext-env-tmp (cons (cons (letrec-s1 e) (letrec-e1 e)) env)]
            [ext-env (cons (cons (letrec-s2 e) (letrec-e2 e)) ext-env-tmp)])
            (eval-under-env (letrec-e3 e) ext-env))]

          
          [(key? e)
         (let (
              [v1 (eval-under-env (key-e e) env)]
              )
               (if (string? (key-s e))
                (key (key-s e) v1)
                (error "The key type should be string")))]


       [(record? e)
         (let ([v1 (eval-under-env (record-k e) env)]
              [v2 (eval-under-env (record-r e) env)])
               (if (key? v1)
               (cond [(munit? v2) (record v1 (munit))]                          
               [(record? v2) (record v1 v2)]
               [#t (error "The second argument should be a record or munit")])          
               (error "The first argument should be a key")))]



       [(value? e)
         (let ([v (eval-under-env (value-r e) env)])
               (if (string? (value-s e))
                (if (record? v)
                    (cond [(equal? (key-s (record-k v)) (value-s e)) (key-e (record-k v))]                          
                          [(munit? (record-r v)) (munit)]
                          [#t (getvalue (value-s e) (record-r v))])
                    (error "The second argument should be a record"))
                (error "The first argument(key) should be a string")))]
          
         
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
   (if(munit? e1) e2 e3))

(define (with* bs e2)
  (cond [(null? bs) e2 ]
   [#t (with (car(car bs)) (cdr(car bs)) (with* (cdr bs) e2))]
   ))

(define (ifneq e1 e2 e3 e4)
  (cnd (iseq e1 e2) e4  e3 ))

;; Problem 4

(define numex-filter (lam "function" "mapfun"
                          (lam "map" "numex-list"
                               (cnd (ismunit (var "numex-list"))
                                    (munit)
                                    (ifnzero (apply (var "mapfun") (1st (var "numex-list")))
                                             (apair(apply (var "mapfun") (1st (var "numex-list"))) (apply (var "map") (2nd (var "numex-list"))))
                                             (apply (var "map") (2nd (var "numex-list"))))))))
  ;;map , apply mapfun to all element of the list

(define numex-all-gt
        (lam "function" "i" (apply numex-filter (lam "greater than" "element" (ifleq (var "element") (var "i") (num 0)(var "element")))))
  )

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function



;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond [(var? e) 
         (set(var-string e))]
        [(plus? e) 
         (let ([freev1 (compute-free-vars (plus-e1 e))]
               [freev2 (compute-free-vars (plus-e2 e))])
           (set-union freev1 freev2))]

        [(minus? e) 
         (let ([freev1 (compute-free-vars (minus-e1 e))]
               [freev2 (compute-free-vars (minus-e2 e))])
           (set-union freev1 freev2))]


        [(mult? e) 
         (let ([freev1 (compute-free-vars (mult-e1 e))]
               [freev2 (compute-free-vars (mult-e2 e))])
           (set-union freev1 freev2))]
        
        [(div? e) 
         (let ([freev1 (compute-free-vars (div-e1 e))]
               [freev2 (compute-free-vars (div-e2 e))])
           (set-union freev1 freev2))]
        
        [(num? e)
         (set null)]

        [(bool? e)
         (set null)]


        [(lam? e)
           (let ([freev1 (compute-free-vars lam-body)]
                 [freev2 (compute-free-vars lam-formal)])
             (fun-challenge (lam-nameopt e) (lam-formal e) (lam-body e) (set-remove freev1 freev2)))]

        
        [(apply? e)
         (let([freev1 (compute-free-vars apply-funexp)]
              [freev2 (compute-free-vars apply-actual)])
            (set-union freev1 freev2))]
     ;;????   
        
        [(munit? e)
           (set(null))]
        
        [(ismunit? e) 
         (let ([freev (compute-free-vars ismunit-e)])
           freev)]


        
        [(neg? e)
         (let([freev (compute-free-vars (neg-e e))])
           freev)]
        
        [(andalso? e) 
         (let ([freev1 (compute-free-vars (andalso-e1 e))]
               [freev2 (compute-free-vars (andalso-e2 e))])
           (set-union freev1 freev2))]
        
        [(orelse? e) 
         (let ([freev1 (compute-free-vars (orelse-e1 e))]
               [freev2 (compute-free-vars (orelse-e2 e))])
           (set-union freev1 freev2))]
        
       [(cnd? e) 
         (let ([freev1 (compute-free-vars (cnd-e1 e))]
               [freev2 (compute-free-vars (cnd-e2 e))]
               [freev3 (compute-free-vars (cnd-e3 e))])
           (set-union(set-union freev1 freev2) freev3))]
        
        [(iseq? e) 
         (let ([freev1 (compute-free-vars (iseq-e1 e))]
               [freev2 (compute-free-vars (iseq-e2 e))])
               (set-union freev1 freev2))]
        
        [(ifleq? e) 
         (let ([freev1 (compute-free-vars (ifleq-e1 e))]
               [freev2 (compute-free-vars (ifleq-e2 e))]
               [freev3 (compute-free-vars (ifleq-e3 e))]
               [freev4 (compute-free-vars (ifleq-e4 e))])
           (set-union(set-union(set-union freev1 freev2) freev3)freev4))]
        
        [(ifnzero? e) 
         (let ([freev1 (compute-free-vars (ifnzero-e1 e))]
               [freev2 (compute-free-vars (ifnzero-e2 e))]
               [freev3 (compute-free-vars (ifnzero-e3 e))])
           (set-union(set-union freev1 freev2) freev3))]

        [(with? e)
         (let([freev2 (compute-free-vars (with-e2 e))])
           freev2)]
        
        [(apair? e) 
         (let ([freev1 (compute-free-vars (apair-e1 e))]
               [freev2 (compute-free-vars (apair-e2 e))])
               (set-union freev1 freev2))]

 
          [(1st? e)
         (let([freev1 (compute-free-vars 1st-e1)])
           freev1)]
         

          [(2nd? e)
         (let ([freev2 (compute-free-vars 2nd-e1)])
           freev2)]



          [(letrec? e)
            (let ([freev3 (compute-free-vars (letrec-e3 e))])
           freev3)]

          
          [(key? e)
         (let ([freev1 (compute-free-vars (key-s e))]
              [freev2 (compute-free-vars (key-e e))]
              )
               (set-union freev1 freev2))]

;;?????????
       [(record? e)
         (let ([freev1 (compute-free-vars (record-k e))]
              [freev2 (compute-free-vars (record-r e))])
               (set-union freev1 freev2))]

;;?????????

       [(value? e)
         (let ([freev1 (compute-free-vars (value-r e))])
               freev1)]
                  
        [#t (error (format "bad NUMEX expression: ~v" e))]))


;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(plus? e) 
         (let ([v1 (eval-under-env-c (plus-e1 e) env)]
               [v2 (eval-under-env-c (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]

        [(minus? e) 
         (let ([v1 (eval-under-env-c (minus-e1 e) env)]
               [v2 (eval-under-env-c (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]
        
        [(num? e)
         (if (integer? (num-int e))
             e
             (error "The argument given is incompatible"))]


        [(fun-challenge? e)
           (closure env e)]
        
        [(closure? e) e]

        
        [(apply? e)
         (let ([fClosure (eval-under-env-c (apply-funexp e) env)]
               [act-p (eval-under-env-c (apply-actual e) env)])
           (if (or (closure? fClosure) (lam? fClosure))
               (if (lam? fClosure)
                   (let* ([lamClosure (eval-under-env-c fClosure env)]
                          [func (closure-f lamClosure)])
                     (if (lam? func)
                           (eval-under-env-c (lam-body func) (cons (cons (lam-formal func) act-p) (cons (cons (lam-nameopt func) lamClosure) (closure-env lamClosure))))
                         (error "Application must be performed on a function")
                      ))
                      (let ([func (closure-f fClosure)])
                           (if (lam? func)
                               (let* ([ext-env-tmp1 (cons (cons (lam-formal func) act-p) (closure-env fClosure))]
                                     [ext-env (cons (cons (lam-nameopt func) fClosure) ext-env-tmp1)])
                                  (eval-under-env-c (lam-body func) ext-env))
                               (error "Application must be performed on a function")))
                     ) 
               (error "First argument must be a closure")))]
        
        
        [(munit? e)  (munit)]
        
        [(ismunit? e)
          (let ([v (eval-under-env-c (ismunit-e e) env)])
            (if (munit? v) (bool #t) (bool #f)))]
        

        
        [(bool? e)
              (if (boolean? (bool-boolean e)) e (error "NUMEX ERROR IN Bool: BAD ARGUMENT IN " e))]
        
        [(mult? e) 
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multipication applied to non-number")))]
        
        [(div? e) 
         (let ([v1 (eval-under-env-c (div-e1 e) env)]
               [v2 (eval-under-env-c (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (equal? (num-int v2) 0)
                   (error "Error for devision by 0")
                   (num (quotient (num-int v1)(num-int v2))))

               (error "NUMEX division applied to non-number")))]
        
        [(neg? e) 
         (let ([v1 (eval-under-env-c (neg-e e) env)])
           (cond
                 [(bool? v1) (if(false? (bool-boolean v1)) (bool #t) (bool #f))]

                 [(num? v1) (num (- (num-int v1)))]
                 
                 [#t (error "NUMEX negation applied to non-number-or-boolean")]))]
        
        [(andalso? e)
            (let ([v1 (eval-under-env-c (andalso-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #f) (bool #f)
                                  (let ([v2 (eval-under-env-c (andalso-e2 e) env)])
                                          (if (bool? v2) v2 (error "The second argument is not boolean"))))
                  (error "The first argument is not boolean")))]
        
        [(orelse? e)
            (let ([v1 (eval-under-env-c (orelse-e1 e) env )])
              (if (bool? v1) (if (equal? (bool-boolean v1) #t) (bool #t)
                                 (let ([v2 (eval-under-env-c (orelse-e2 e) env)])
                                   (if (bool? v2) v2 (error "The second argument is not boolean"))))
                  (error "The first argument is not boolean")))]
        
        [(cnd? e)
          (let ([v1 (eval-under-env-c (cnd-e1 e) env)])

            (if (bool? v1)
                (if(equal? (bool-boolean v1) #t) (eval-under-env-c (cnd-e2 e) env) (eval-under-env-c (cnd-e3 e) env))
                (error "The first argument is not boolean"))
            )]
        
        [(iseq? e) 
         (let ([x (eval-under-env-c (iseq-e1 e) env)]
               [y (eval-under-env-c (iseq-e2 e) env)])
         (cond [(and (num? x)(num? y))
               (if (equal? (num-int x)  (num-int y)) (bool #t) (bool #f))                    
               ]
             
               [(and (bool? x)(bool? y))
              
               (if(equal? (bool-boolean x)   (bool-boolean y)) (bool #t) (bool #f))       
               ]
               [#t (bool #f)]))
          ]
        
        [(ifleq? e)
           (let ([v1 (eval-under-env-c (ifleq-e1 e) env)]
                 [v2 (eval-under-env-c (ifleq-e2 e) env)]
                 )
             (if (and (num? v1) (num? v2)) (if (> (num-int v1) (num-int v2)) (eval-under-env-c (ifleq-e4 e) env) (eval-under-env-c (ifleq-e3 e) env)
                                            ) (error "The argument is not boolean"))
           )]
        
        [(ifnzero? e)
           (let ([v1 (eval-under-env-c (ifnzero-e1 e) env)])
             (if (num? v1)
                 (if (equal? (num-int v1)  0)
                     (eval-under-env-c (ifnzero-e3 e) env)
                     (eval-under-env-c (ifnzero-e2 e) env))
                 (error "The first argument is not number")))]

        [(with? e)
            (let* ([v (eval-under-env-c (with-e1 e) env)]
            [ext-env (cons (cons (with-s e) v) env)])
            (eval-under-env-c (with-e2 e) ext-env))]
        
        [(apair? e)
            (let ([v1 (eval-under-env-c (apair-e1 e) env)]
              [v2 (eval-under-env-c (apair-e2 e) env)])
               (apair v1 v2))]

 
          [(1st? e)
           (let ([v (eval-under-env-c (1st-e1 e) env)])
             (if (apair? v)
                 (apair-e1 v)
                 (error "The first is only valid on a pair:(")))]
         

          [(2nd? e)
          (let ([v (eval-under-env-c (2nd-e1 e) env)])
            (if (apair? v)
                (apair-e2 v)
                (error "The second is only valid on a pair:(")))]

          


          [(letrec? e)
            (let* ([ext-env-tmp (cons (cons (letrec-s1 e) (letrec-e1 e)) env)]
            [ext-env (cons (cons (letrec-s2 e) (letrec-e2 e)) ext-env-tmp)])
            (eval-under-env-c (letrec-e3 e) ext-env))]

          
          [(key? e)
         (let (
              [v1 (eval-under-env-c (key-e e) env)]
              )
               (if (string? (key-s e))
                (key (key-s e) v1)
                (error "The key type should be string")))]


       [(record? e)
         (let ([v1 (eval-under-env-c (record-k e) env)]
              [v2 (eval-under-env-c (record-r e) env)])
               (if (key? v1)
               (cond [(munit? v2) (record v1 (munit))]                          
               [(record? v2) (record v1 v2)]
               [#t (error "The second argument should be a record or munit")])          
               (error "The first argument should be a key")))]



       [(value? e)
         (let ([v (eval-under-env-c (value-r e) env)])
               (if (string? (value-s e))
                (if (record? v)
                    (cond [(equal? (key-s (record-k v)) (value-s e)) (key-e (record-k v))]                          
                          [(munit? (record-r v)) (munit)]
                          [#t (getvalue (value-s e) (record-r v))])
                    (error "The second argument should be a record"))
                (error "The first argument(key) should be a string")))]
          
         
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
