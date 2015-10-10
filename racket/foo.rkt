;(include "sicp/foo.rkt")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define isfooloaded "yes")

(define (even? x)
    (= 0 (remainder x 2)))

(define (square x) (* x x))

(define (smallest-divisor n)
    (if (= (remainder n 2) 0)
        2
        (find-divisor n 3)))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n) (= n (smallest-divisor n)))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (remainder (square (expmod base (/ exp 2) m))
                m))
        (else 
            (remainder (* base (expmod base (- exp 1) m))
                m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it(+ 1 (random (- n 1)))))
    
(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
        
(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (current-inexact-milliseconds)))
    
(define (start-prime-test n start-time)
    (when (prime? n)
        (report-prime (- (current-inexact-milliseconds) start-time))))
        
(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
    
    
(define (search-for-primes start end)
    (timed-prime-test start)
    (when (< start end) (search-for-primes (+ 1 start) end)))

(define (fast-timed-prime-test n)
    (newline)
    (display n)
    (start-fast-prime-test n (current-inexact-milliseconds)))
    
(define (start-fast-prime-test n start-time)
    (when (fast-prime? n 100)
        (report-prime (- (current-inexact-milliseconds) start-time))))
        
(define (fast-search-for-primes start end)
    (fast-timed-prime-test start)
    (when (< start end) (fast-search-for-primes (+ 1 start) end)))
    
(define (sumold term a next b)
    (if (> a b)
    0
    (+ (term a)
        (sum term (next a) next b))))

(define (identity x) x)

(define (inc x) (+ 1 x))

(define (sum-integers a b)
    (sum identity a inc b))

(define (test1 f)
    (define (test2)
        (+ f 1))
    (test2))
    
(define (cube x)
    (* x x x))
    
(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (y k) (f (+ a (* k h))))
    (define (term k)
        (cond
            ((or (= k 0) (= k n)) (y k))
            ((even? k) (* (y k) 2))
            (else (* (y k) 4))))
    (/ (* h (sum term 0 inc n)) 3))
    
(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))
    
(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))
    
(define (pifac terms)
    (define (upterm i)
        (if (even? i)
            (+ 2 i)
            (+ 1 i)))
    (define (downterm i)
        (if (even? i)
            (+ 1 i)
            (+ 2 i)))
    (define (term i)
        (/ (upterm i) (downterm i)))
    (* 4. (product term 1 inc terms)))

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))
    
(define (filtered-accumulate condition combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (if (condition a)
                (iter (next a) (combiner result (term a)))
                (iter (next a) (combiner result null-value)))))
    (iter a null-value))
    
(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value)
                    (search f neg-point midpoint))
                    ((negative? test-value)
                    (search f midpoint pos-point))
                    (else midpoint))))))
                
(define (close-enough? x y)
    (< (abs (- x y)) 0.001))
    
(define (average x y)
    (/ (+ x y) 2))
    
(define (half-interval-method f a b)
    (let
        ((a-value (f a))
         (b-value (f b)))
            (cond
                ((and (negative? a-value) (positive? b-value))
                    (search f a b))
                ((and (negative? b-value) (positive? a-value))  
                    (search f b a))
                (else
                    (error "Values not of opposite sign" a b )))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (display guess)
        (newline)
        (let ((next (f guess)))
            ( if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define (sqrt x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0))
    
(define (cont-frac n d k)
    (define (cont-frac-recur n d k i)
        (if (= k i)
            (/ (n i) (d i))
            (/ (n i) (+ (d i) (cont-frac-recur n d k (+ i 1))))))
    (cont-frac-recur n d k 1))
    
(define (cont-frac-iter n d k)
    (define (cont-frac-iter n d i partial)
        (if (= i 0)
            partial
            (cont-frac-iter n d (- i 1) (/ (n i) (+ (d i) partial)))))
    (cont-frac-iter n d k (/ (n k) (d k))))    
                
(define (euler k)
    (let (
        [n (lambda (tmp) 1.0)]
        [d (lambda (i) 
            (cond
                ((= (remainder i 3) 1) 1)
                ((= (remainder i 3) 2) (* 2 (/ (+ i 1) 3)))
                ((= (remainder i 3) 0) 1)))])
        (+ 2 (cont-frac-iter n d k))))
        
(define (tan-cf x k)
    (let (
        [n (lambda (i1) (if (= i1 1) x (- 0 (square x))))]
        [d (lambda (i2) (- (* i2 2) 1))])
        (cont-frac-iter n d k)))
        
(define (average-damp f)
    (lambda (x) (average x (f x))))
  
(define (sqrt1 x)
    (fixed-point 
        (average-damp (lambda (y) (/ x y)))
        1.0))
        
(define (cube-root x)
    (fixed-point 
        (average-damp (lambda (y) (/ x (square y))))
        1.0))
        
(define (deriv g)
    (lambda (x)
        (/
            (-
                (g 
                    (+ 
                        x
                        dx))
                (g x))
            dx)))

(define dx 0.00001)

(define (newton-transform g)
    (lambda (x)
        (- x (/ (g x) ((deriv g) x)))))
       
(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))
                
(define (sqrt x)
    (newtons-method (lambda (y) (- (square y) x))
        1.0))                

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))
    
(define (sqrt x)
    (fixed-point-of-transform 
        (lambda (y) (/ x y))
        average-damp
        1.0))
        
(define (sqrt x)
    (fixed-point-of-transform 
        (lambda (y) (- (square y) x))
        newton-transform
        1.0))  
        
(define (cubic a b c)
    (lambda (x)
        (+
            (cube x)
            (* a (square x))
            (* b x)
            c)))
            
(define (double f)
    (lambda (x) (f (f x))))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))
        
(define (repeated f n)
    (define (repeated-iter f n i tf)
        (if (= i n)
            tf
            (repeated-iter f n (+ 1 i) (compose f tf))))
    (repeated-iter f n 1 f))
        
(define (smooth f)
    (lambda (x)
        (/ 
            (+ 
                (f (- x dx))
                (f x)
                (f (+ x dx)))
            3)))
            
(define (n-smooth f n)
    ((repeated smooth n) f))
    
(define (add-rat x y)
    (make-rat 
        (+ 
            (* (numer x) (denom y))
            (* (numer y) (denom x)))
        (*
            (denom x)
            (denom y))))
            
(define (sub-rat x y)
    (make-rat 
        (- 
            (* (numer x) (denom y))
            (* (numer y) (denom x)))
        (*
            (denom x)
            (denom y))))            

(define (mul-rat x y)
    (make-rat 
        (* 
            (numer x) 
            (numer y))
        (*
            (denom x)
            (denom y))))      

(define (div-rat x y)
    (make-rat 
        (* 
            (numer x) 
            (denom y))
        (*
            (denom x)
            (numer y))))              
         
(define (equal-rat? x y)
    (= 
        (* 
            (numer x) 
            (denom y))
        (*
            (numer y)
            (denom x))))          

(define (make-rat n d)
  (let ((g (gcd n d)))
  (let ((nn (/ n g)) (nd (/ d g)))
    (cons (* (* (sgn nn) (sgn nd)) nn) (abs nd)))))
    
(define (numer x) ( car x ))
(define (denom x) (cdr x))

(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x)))

(define one-half (make-rat 1 2 ))

(define one-third (make-rat 1 3))
           
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point 
    	(average 
	  (x-point (start-segment segment)) 
	  (x-point (end-segment segment))) 	
	(average 
	  (y-point (start-segment segment)) 
	  (y-point (end-segment segment)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (rectangle ul dr)
  (cons ul dr))

(define (rectangle2 ur dl)
  (cons 
    (make-point (x-point dl) (y-point ur))
    (make-point (x-point ur) (y-point dl))))


(define (perimeter rect)
 (* 2
    (+
      (-
       (x-point (ur-point rect))
       (x-point (ul-point rect)))
      (-
       (y-point (dl-point rect))
       (y-point (ul-point rect))))))

(define (ul-point rect)
   (car rect))

(define (dr-point rect)
   (cdr rect))

(define (ur-point rect)
  (make-point
    	(x-point (dr-point rect))
	(y-point (ul-point rect))))

(define (dl-point rect)
  (make-point
    	(x-point (ul-point rect))
	(y-poiny (dr-point rect))))

(define (area rect)
 (*
   (-
      (x-point (ur-point rect))
      (x-point (ul-point rect)))
   (-
      (y-point (dl-point rect))
      (y-point (ul-point rect)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 

(define (d-cons a b)
	(*
	  (expt 2 a)
	  (expt 3 b)))

(define (d-car-numn x n)
  (d-car-numn-h x n 0))

(define (d-car-numn-h x n s)
  (cond
    ((= n 0) s)
    ((> (remainder n x) 0) s)
    (else (d-car-numn-h x (/ n x) (+ 1 s)))))

(define (d-car z)
 (d-car-numn 2 z))

(define (d-cdr z)
 (d-car-numn 3 z))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (d-zero f)
   (lambda (x) x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
		    (lower-bound y))
		 (+ (upper-bound x)
		    (upper-bound y))))

(define (mul-interval x y)
  (let (( p1 (* (lower-bound x) (lower-bound y)))
	( p2 (* (lower-bound x) (upper-bound y)))
	( p3 (* (upper-bound x) (lower-bound y)))
	( p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

#|(define (d-mul-interval x y)
	(cond (> (lower-bound x) 0)
	      (cond (> (lower-bound y) 0)
		    (make-interval (* (lower-bound x) 
				      (lower-bound y))
				   (* (upper-bound x)
				      (upper-bound y)))
	            (< (upper-bound y) 0)
		    (make-interval (* (upper-bound x) 
				      (lower-bound y))
				   (* (lower-bound x)
				      (upper-bound y)))
		    else
		    (make-interval (* (upper-bound x) 
				      (lower-bound y))
				   (* (upper-bound x)
				      (upper-bound y))))
	      (< (upper-bound x) 0)
	      (cond (> (lower-bound y) 0)
		    (make-interval (* (upper-bound x) 
				      (upper-bound y))
				   (* (lower-bound x)
				      (lower-bound y)))
	            (< (upper-bound y) 0)
		    (make-interval (* (lower-bound x) 
				      (lower-bound y))
				   (* (upper-bound x)
				      (upper-bound y)))
		    else
		    (make-interval (* (upper-bound x) 
				      (upper-bound y))
				   (* (upper-bound x)
				      (upper-bound y)))) ;this needs the extra step
|#		    
 
(define (div-interval x y)
  (if (= (lower-bound y) (upper-bound y))
    (print "Division bij interval of width zero")
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (lower-bound a)
  (car a))

(define (upper-bound a)
  (cdr a))

(define (sub-interval x y)
   (make-interval (- (lower-bound x)
		     (upper-bound y))
		  (- (upper-bound x)
		     (lower-bound y))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (* w (/ p 100)))  (+ c (* w (/ p 100)))))

(define (percent i)
  (* 100 (/ (width i) (center i))))

;2.13: just add them

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (list-ref items n)
  (if (= n 0) 
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (last-pair list1)
  (if (null? (cdr list1))
    (car list1)
    (last-pair (cdr list1))))

(define (snoc list1 a)
  (if (null? list1)
    (cons a list1)
    (cons (car list1) (snoc (cdr list1) a))))

(define (reverse list1)
  (if (null? list1)
    list1
    (snoc (reverse (cdr list1)) (car list1))))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount 
		(except-first-denomination coin-values))
            (cc (- amount
		   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination list1)
  (car list1))

(define (except-first-denomination list1)
  (cdr list1))

(define no-more? null?)

(define (same-parity x . y)
  (define (d-same-parity-h x ylist) 
    (if (null? ylist)
      ylist
      (if (or (and (even? x) 
	           (even? (car ylist))) 
	      (and (not (even? x)) 
		   (not (even? (car ylist)))))
        (cons (car ylist) (d-same-parity-h x (cdr ylist)))
        (d-same-parity-h x (cdr ylist)))))
  (d-same-parity-h x y))

(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define (square-list items)
  (if (null? items)
      items
      (cons (* (car items) (car items)) 
	    (square-list (cdr items)))))

(define (deep-reverse list1)
  (cond ((null? list1) null)
	((pair? (car list1)) (append (deep-reverse (cdr list1)) 
			       (list (deep-reverse (car list1)))))
	(else (snoc (deep-reverse (cdr list1))
		      (car list1)))))

(define (fringe list1)
  (cond ((null? list1) null)
	((pair? (car list1)) (append (fringe (car list1)) 
				     (fringe (cdr list1))))
	(else (cons (car list1) 
		    (fringe (cdr list1))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define mobile? pair?)

(define (branch-weight branch)
  (if (mobile? (branch-structure branch)) 
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (balanced? mobile)
  (and (= (* (branch-length (left-branch mobile))
	     (branch-weight (left-branch mobile)))

	  (* (branch-length (right-branch mobile))
	     (branch-weight (right-branch mobile))))

       (and (balanced-structure? (branch-structure (left-branch mobile)))
            (balanced-structure? (branch-structure (right-branch mobile))))))
  
(define (balanced-structure? structure)
  (if (mobile? structure)
    (balanced? structure)
    #t))

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))      

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))      
   	
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))      

(define (square-tree tree) (tree-map square tree))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest 
		(map (lambda (r) (cons (car s)
				        r))
		     rest)))))
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              null
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
  
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff
						   (* x higher-terms)))
              0
              coefficient-sequence))


(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves t)
  (accumulate +
	      0 
	      (map (lambda (x) 1) 
		   (enumerate-tree t))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (mv) (dot-product v mv)) m))
(define (transpose mat)
  (accumulate-n cons null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (reverse sequence)
  (fold-right (lambda (x y) snoc) null sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) cons) null sequence))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(define (permutations s)
  (if (null? s)                   
      (list null)                  
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-pairs n)
  (if (= n 1)
    null
   (append (unique-pairs (- n 1))
	   (map (lambda (x) (list n x)) 
		(enumerate-interval 1 (- n 1))))))
(define (prime-sum-pairs n)
  (filter (lambda (pair) (prime? (+ (car pair) 
		                    (car (cdr pair)))))
	  (unique-pairs n)))


(define (unique-triples n)
  (if (= n 1)
  null
  (append (unique-triples (- n 1))
	  (map (lambda (x) (cons n x))
		 (unique-pairs (- n 1))))))

(define (d-241 n s)
  (filter (lambda (triple) (= s (+ (car triple)
				 (car (cdr triple))
				 (car (cdr (cdr triple))))))
	  (unique-triples n)))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board null)

(define (adjoin-position row col rest)
  (cons (make-position row col) 
	rest))

(define (new-position positions)
  (car positions))

(define (old-positions positions)
  (cdr positions))

(define (make-position row col)
  (list row col))

(define position-row car)

(define (position-col position)
  (car (cdr position)))

(define (forall pred list1)
  (accumulate (lambda (new old) (and (pred new) old)) #t list1))

(define (safe? newcol positions)
  (let ((newqueen (new-position positions))
       (oldqueens (old-positions positions)))
    (define (attacks? q1 q2)
      (or (same-row? q1 q2)
	  (diagonal? q1 q2)))
    (define (same-row? q1 q2)
	(= (position-row q1) 
	     (position-row q2)))
    (define (diagonal? q1 q2)
	  (= (abs (- (position-row q1) (position-row q2)))
	     (abs (- (position-col q1) (position-col q2)))))
    (forall (lambda (old) (not (attacks? newqueen old))) oldqueens)))

#|
(define (up-split painter n)
 (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (split first second)
  (lambda (painter n)
  (if (= n 0)
    painter
    (let ((smaller ((split first second) painter (- n 1))))
	  (first painter (second smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))
|#
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? l1 l2)
  (if (null? l1)
    (if (null? l2)
      #t
      #f)
    (if (null? l2)
      #f
      (and (eq? (car l1)
		(car l2))
	   (equal? (cdr l1)
		   (cdr l2))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var)
			   1
			   0))
        ((sum? exp) (make-sum (deriv (addend exp)
				     var)
                              (deriv (augend exp)
				     var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
						(deriv (multiplicand exp)
						       var))
				  (make-product (deriv (multiplier exp)
						       var)
						(multiplicand exp))))
        ((exponentiation? exp) (make-product (exponent exp)
					     (make-product (make-exponentiation (base exp)
									        (- (exponent exp)
										   1))
							   (deriv (base exp) var))))
	(else
         (error "unknown expression type -- DERIV" exp))))
	
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) 
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and ( number? a1) (number? a2)) (+ a1 a2 ))
	(else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) 
       (eq? (cadr x) '+)))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
	((=number? exponent 1) base)
	((=number? base 1) 1)
	((=number? base 0) 0)
	(else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (augend1 s)
  (if (null? (cdddr s))
    (caddr s)
    (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) ( eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand s) (caddr s))

(define (multiplicand1 p) 
  (if (null? (cdddr p))
    (caddr p)
    (cons '* (cddr p))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set1) 
    set2
    (adjoin-set (car set1) 
		(union-set (cdr set1)
			   set2))))

(define (element-of-set?1 x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set1 x set)
  (cons x set))

(define (intersection-set1 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set1 set1 set2)
 (append set1 set2))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(define (adjoin-set x set)
  (if (null? set)
      '(x)
      (if (= (car set) x)
	set
	(if (> (car set) x)
	  (cons x 
		set)
	  (cons (car set)
		(adjoin-set x 
			    (cdr set)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((= (car set1) (car set2)) (cons (car set1) 
					 (union-set (cdr set1)
						    (cdr set2))))
	((< (car set1) (car set2)) (cons (car set1) 
					 (union-set (cdr set1)
						    set2)))
	((> (car set1) (car set2)) (cons (car set2) 
					 (union-set set1
						    (cdr set2))))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((= given-key 
	    (key (entry set-of-records))) (entry set-of-records))
	((< given-key
	    (key (entry set-of-records))) (lookup given-key
	    					  (left-branch set-of-records)))
	((> given-key
	    (key (entry set-of-records))) (lookup given-key
	    					  (right-branch set-of-records)))))


(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-in-list? symbol list1)
  (if (null? list1) 
    #f
    (if (eq? (car list1)
	     symbol)
      #t
      (symbol-in-list? symbol (cdr list1)))))

(define (encode-symbol symbol tree)
  (if (not (symbol-in-list? symbol 
			    (symbols tree)))
    (error "symbol not in tree")
    (if (leaf? tree)
      (if (eq? (symbol-leaf tree)
	       symbol)
	'()
	(error "symbol not in eaf"))
      (if (symbol-in-list? symbol
			   (symbols (left-branch tree)))
	(cons 0
	      (encode-symbol symbol (left-branch tree)))
        (if (symbol-in-list? symbol
			     (symbols (right-branch tree)))
	  (cons 1
	        (encode-symbol symbol (right-branch tree)))
	  (error "symbol not in left or right branch"))))))

(define (generate-huffman-tree pairs)
  (car (successive-merge (make-leaf-set pairs))))

(define (successive-merge leaf-set)  
  (if (null? (cdr leaf-set))
    leaf-set
    (successive-merge (adjoin-set (make-code-tree (car leaf-set)
     		                		  (cadr leaf-set))
				  (cddr leaf-set)))))
  

(define (make-from-mag-ang r th)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos th)))
          ((eq? op 'imag-part) (* r (sin th)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) th)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m passwordinput)
    (if (eq? passwordinput password)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    (else (error "Unknown request -- MAKE-ACCOUNT"
			 m)))
      (lambda (x) (print 'incorrect))))
  dispatch)

(define (make-accumulator initial)
  (lambda (amount)
    (begin (set! initial (+ initial amount))
	   initial)))

(define (make-monitored f)
  (define calls 0)
  (lambda (x)
    (cond ((eq? x 'how-many-calls?) calls)
	  ((eq? x 'reset-count) (set! calls 0))
	  (else (begin (set! calls (+ 1 calls))
		       (f x))))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (iter P x1 x2 y1 y2 trials trialsdone hit)
    (if (= trials trialsdone)
      hit
      (if (P (random-in-range x1 x2)
	     (random-in-range y1 y2))
	(iter P x1 x2 y1 y2 trials (+ 1 trialsdone) (+ 1 hit))
	(iter P x1 x2 y1 y2 trials (+ 1 trialsdone) hit))))
  (/ (* (- x2 x1)
        (- y2 y1)
        (iter P x1 x2 y1 y2 trials 0 0))
     trials))

(define (USP x y)
  (and (< (sqrt1 (+ (square x)
		   (square y)))
	  100)))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y) ;set-cdr is niet gedefinieerd in racket, kan wel geimporteerd worden.
          (loop temp x))))
  (loop x '()))

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))





