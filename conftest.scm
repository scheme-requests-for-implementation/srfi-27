; CONFIDENCE TESTS FOR "RANDOM"-SRFI
; ==================================
;
; Sebastian.Egner@philips.com, Feb-2002.
;
; This is a random collection of some simple tests that challenge an
; implementation of the SRFI to confess about a few assertions stated 
; by the specification. In addition, it allows to write random bits
; to the DIEHARD testsuite of random number generators.
;
; compliance:
;   Scheme R5RS. In addition:
;     SRFI-23: error
;     ascii->char for writing a file of bytes (e.g. ascii)
;
; To load the tests in Scheme 48 0.57, with 'random.scm' loaded:
;   ,open ascii
;   ,load conftest.scm

; (check-random-ranges s)
;    creates some integers for various ranges for the source s.
;
;    try: (check-random-ranges default-random-source)
;       Produces some integers and checks that they do indeed fall
;    into the ranges for which they have been created. If they do
;    not then an error is signalled. If they do #t is returned.

(define (check-random-ranges s)
  (let ((rand (random-integer-generator s)))
    (do ((i 0 (+ i 1)))
	((> i 1000) #t)
      (let* ((bits (rand 1024))
	     (n (rand (expt 2 bits)))
	     (x (rand n)))
	(if (not (and (<= 0 bits) 
		      (< bits 1024)
		      (<= 0 n)
		      (< n (expt 2 bits))
		      (<= 0 x)
		      (< x n)))
	    (error "range fault: " (list bits n x)))))))


; (check-state-get/set s)
;    creates a new random source t, transfers the state of the source
;    s into t and compares two numbers drawn from the sources.
;
;    try: (check-state-get/set default-random-source)
;       This should result is a something recognizable as true.

(define (check-state-get/set s)
  (let ((t (make-random-source))
        (state (random-source-state s)))
    (display "(random-source-state s) -> ")
    (display state)
    (newline)
    (set-random-source-state! t state)
    (list 'eq? 
	  ((random-integer-generator s) 100000) 
	  ((random-integer-generator t) 100000))))


; (check-histogram s n calls)
;    produces a histogram of the values obtained from the random source s
;    and the range n. The procedure s is invoked calls times. The result
;    is a vector of length n with the histogram.
;
;    try: (check-histogram default-random-source 10 1000000)
;      For this example, each component of the vector is 
;    Binomial(1/10, 10^6) distributed, although the components are not 
;    independent. The standard deviation is 300 and ideally the chance 
;    to find a value x such that |x - 10^5| > 10^3 is approximately 0.27%.

(define (check-histogram s n calls)
  (let ((h (make-vector n 0))
	(rand (random-integer-generator s)))
    (do ((i 0 (+ i 1))) ((= i calls) h)
      (let ((x (rand n)))
        (if (and (<= 0 x) (< x n))
            (vector-set! h x (+ (vector-ref h x) 1))
            (error "value out of range" x))))))
                     

; (write-diehard filename s bytes-per-call calls)
;    creates a binary file to which bytes-per-call * calls bytes are
;    written. The bytes are obtained from the random source s using
;    the range n = (expt 256 bytes-per-call).
;       The intention of write-diehard is to give implementors a 
;    '15 min.'-way of running their favourite random number generator 
;    through a pretty tough testsuite.
;
;    try: For the reference implementation, the call
;
;       (write-diehard "outfile" (make-random-source) 4 2867200)
;
;    should create exactly the same file as Diehard's 'makewhat' program
;    for generator No. 5 "COMBO" with seeds 1046188960 1283534646 39968.
;    Beware of problems with byte order in 32-bit words when you test if
;    the files are identical! The file COMBO.32 produced by 'makewhat'
;    looks as follows (md5sum = 6fbcbee456aa75eb4f00f68137526a81):
;
;       0000000 a7eb f84e 7976 b34a 36e5 b9be a6d5 db5e
;       0000020 6072 7bf7 60bb bc59 9cfd 9009 24ee 28f4
;       0000040 3fe8 0a4e 9866 8fd3 02c7 2240 f18f 4b91
;       0000060 aaf3 be82 ab46 b327 00a2 6692 1835 aa2b
;       0000100 291d 33cd 7b9e 0c9a c1f2 2fa4 cca9 0fff
;       0000120 3099 d7fc 6f7b eb07 dc0e 3405 692f b19d
;       0000140 573d 679b f643 d509 dad8 fbff 36e6 8588
;       ..total length is 11468800 bytes.

(define (write-diehard filename s bytes-per-call calls)
  (let ((port (open-output-file filename))
	(rand (random-integer-generator s))
        (n (expt 256 bytes-per-call)))
    (do ((i 0 (+ i 1))) ((= i calls) (close-output-port port))
      (let ((x (rand n)))
        (do ((k 0 (+ k 1))) ((= k bytes-per-call))
          (write-char (ascii->char (modulo x 256)) port)
          (set! x (quotient x 256)))))))
