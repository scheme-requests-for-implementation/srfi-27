; REFERENCE IMPLEMENTATION OF THE "RANDOM"-SRFI
; =============================================
;
; Sebastian.Egner@philips.com, Feb-2002.
;
; compliance:
;   Scheme R5RS with integers covering at least {0..2^32-1}. In addition:
;     SRFI-9:  define-record-type
;     SRFI-23: error
;     SRFI-19: current-time, time-second, time-nanosecond
;
; loading this file into Scheme 48 0.57:
;   ,open srfi-9 srfi-23
;   ,open posix-time posix
;   (define time-second time-seconds)
;   (define (time-nanosecond t) 0)
;   ,load random.scm
;
; The Generator
; -------------
;
; The implementation is based on G. Marsaglia's DIEHARD library:
;
;   G. Marsaglia: 
;   Diehard -- Testsuite for Random Number Generators. 
;   http://stat.fsu.edu/~geo/diehard.html
;
; The generator we use is Marsaglia's COMBO generator. It is the
; combination of a 32-bit lagged multiplicative Fibonacci generator
; with a 16-bit multiply-with-carry generator. The sequence z is
;
;   z[n] = (x[n] + y[n]) mod mx;
;   y[n] = ay * (y[n-1] mod my) + (y[n-1] div my);
;   x[n] = (x[n-1] * x[n-2]) mod mx,
;
; where mx = 2^32, my = 2^16 and ay = 30903.
;
; The generator passes all tests of the Diehard testsuite and all
; bits are usable. The period of x is maximal if the seeds are both 
; odd and one of them is congruent to 3 mod 12. A simple way to ensure 
; this property is to the set x[0] = 3 (2 u + 1)^2, x[1] = 2 v + 1 for
; randomization values u, v. The period of y is maximal if the seed
; is non-zero.
;
; The Implementing Datastructure
; ------------------------------
;
; A random source is a record type of type :random-source which contains
; the operations on random sources in its fields. The actual state is stored
; in the binding time environment of the make-random-source procedure.

; private definition of :random-source

(define-record-type :random-source
  (:random-source-make generator state set-state! randomize!)
  :random-source?
  (generator  :random-source-generator)
  (state      :random-source-state)
  (set-state! :random-source-set-state!)
  (randomize! :random-source-randomize!))

; exported interface

(define (make-random-source)

; The COMBO Generator
; -------------------

; constants defining the iteration

  (define mx (expt 2 32))
  (define my (expt 2 16))
  (define ay 30903)

; the current state 

  (define x1 1457036163) ; x[n-1] in {0..mx-1}
  (define x0 2567069293) ; x[n]   in {0..mx-1}
  (define y       39968) ; y[n]   in {0..ay*my}

; obtain next value in {0..mx-1}: the COMBO generator

  (define (random-mx)
    (let ((x2 x1))
      (set! x1 x0)
      (set! x0 (modulo (* x1 x2) mx))
      (set! y (+ (* ay (modulo y my)) (quotient y my)))
      (modulo (+ x0 y) mx)))

; From (random mx) to (random n): Changing Range
; ----------------------------------------------

; obtain next value for 1 <= n < mx:
;   We choose x in {0..m-1}, where m = floor(mx/n)*n, by repeatedly
;   choosing x in {0..mx-1} and discarding values x >= m. The expected 
;   number of iterations is less than two. Once such an x has been 
;   chosen, it can be decomposed as x = x1 floor(mx/n) + x0 to obtain
;   a uniformly distributed x1 in {0..n-1}. We extract the high bits
;   because extracting the low bits may shorten the period (also here).

  (define (random-small n)
    (let* ((m-by-n (quotient mx n))
           (m (* m-by-n n)))
      (do ((x (random-mx) (random-mx)))
          ((< x m) (quotient x m-by-n)))))

; obtain next value for n = mx^k:
;   We choose x[0], .., x[k-1] in {0..mx-1} and return the value
;   x = x[0] + x[1] mx + .. + x[k-1] mx^(k-1).

  (define (random-expt-mx-k k)
    (if (= k 1)
        (random-mx)
        (+ (random-mx) (* mx (random-expt-mx-k (- k 1))))))

; obtain next value for n > mx:
;   We use essentially the same method as for random-small. Let k be
;   such that mx^(k-1) < n <= mx^k. The we repeat choosing a value 
;   x in {0..mx^k-1} until x < m = floor(mx^k/n)*n. The result is
;   floor(x/(m/n)), which is uniformly distributed in {0..n-1}.
;      This method can be improved if the generation of x and the
;   test whether x < m are mixed. Whether the gain (factor two at 
;   best) is worth the effort remains to be seen.

  (define (random-large n k expt-mx-k)
    (if (<= n expt-mx-k)
        (let* ((m-by-n (quotient expt-mx-k n))
               (m (* m-by-n n)))
          (do ((x (random-expt-mx-k k) (random-expt-mx-k k)))
              ((< x m) (quotient x m-by-n))))
        (random-large n (+ k 1) (* expt-mx-k mx))))

; obtain next value for arbitrary n >= 1:
;   For n < mx we dispatch to random-small, for n = mx we dispatch
;   to random-mx and for n > mx we dispatch to random-large.

  (define (random n)
    (cond ((< n 1)
	   (error "illegal range" n))
	  ((< n mx)
	   (random-small n))
	  ((= n mx)
	   (random-mx))
	  (else
	   (random-large n 2 (* mx mx)))))

; Accessing the State
; -------------------

; encode the current state

  (define (state)
    (list 'marsaglia-combo x1 x0 y))

; restore the state

  (define (set-state! s)
    (if (and (list? s) (= (length s) 4) (eq? (car s) 'marsaglia-combo))
        (begin (set! x1 (cadr s))
               (set! x0 (caddr s))
               (set! y  (cadddr s)))
        (error "illegal state of random source" s)))

; Randomizing the State
; ---------------------

; obtain a value that is supposed to be 'really' random:
;   For simplicity we use current-time with the maximum resolution 
;   the implementation of SRFI-19 provides. For simulation purposes 
;   and randomized datastructures this should be more than good enough.
;      For security applications it is certainly not sufficient, both
;   because it can be manipulated easily and because the amount of 
;   actual random bits in the time is too small. For serious security
;   (is there any other?) you should set up a physical source of 
;   somewhat random bits and 'mix&distil' this source. The methods are
;   beyond the scope of this simple reference implementation. Refer
;   to RFC1750.

  (define (randomization-seed)
    (let ((t (current-time)))
      (+ (* (time-second t) (expt 10 9)) (time-nanosecond t))))

; use randomization-seed to randomize the state:
;   The function makes sure that the state of the COMBO generator
;   satisfy some algebraic properties which ensure a maximal period.
;   Refer to G. Marsaglia's description of it in Diehard.

  (define (randomize!)
    (let* ((seed (modulo (randomization-seed) (* mx mx 2 my)))
           (u    (modulo seed mx))
           (v    (modulo (quotient seed mx) mx))
           (w    (quotient seed (* mx mx))))

      ; set state, enforcing maximum periods for x and y
      (set! x1 (modulo (* 3 (+ (* 2 u) 1) (+ (* 2 u) 1)) mx))
      (set! x0 (modulo (+ (* 2 v) 1) mx))
      (set! y  (+ (modulo w (- my 1)) 1))

      ; iterate the generator a few times to avoid initial anomalies
      (do ((i 0 (+ i 1)))
          ((>= i 8))
           (random-mx))

      ; return an unspecific value to the application
      (if #f #f)))

; Create the Random Source Object
; -------------------------------

  (:random-source-make random state set-state! randomize!))


; Export the Interface as Specified in the SRFI
; =============================================
;
; The performance overhead by the record is zero for the actual
; generator and very low (and probably unimportant) for the operations
; related to manipulating the state

(define random-source? 
  :random-source?)

(define (random-source-state s)
  ((:random-source-state s)))

(define (set-random-source-state! s state)
  ((:random-source-set-state! s) state))

(define (randomize-random-source! s)
  ((:random-source-randomize! s)))

(define (random-integer-generator s)
  (:random-source-generator s))

(define default-random-source
  (make-random-source))

(define random
  (random-integer-generator default-random-source))
