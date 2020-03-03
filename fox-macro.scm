;;; The implementation below requires SRFI-6 (Basic string ports).
;;; For compatibility with R6RS:
;;; (define inexact->exact exact)
;;; (define exact->inexact inexact)

(define (object->string object writer)
  (get-output-string
   (let ((str-port (open-output-string)))
     (writer object str-port)
     str-port)))

(define (str-convert str str-pro-num)
  (let ((left (car str-pro-num)) (right (cdr str-pro-num)))
    (cond
     ((string? right)
      (cond
       ((string? left) (string-append left str right))
       ((procedure? left) (string-append (left str) right))
       ((zero? left) (string-append str right))
       (else (string-append (let ((len (string-length str)))
			      (if (positive? left)
				  (if (< left len)
				      (substring str 0 left)
				      str)
				  (if (positive? (+ len left))
				      (substring str (abs left) len)
				      "")))
			    right))))
     ((procedure? right)
      (cond
       ((string? left) (right (string-append left str)))
       ((procedure? left) (right (left str)))
       ((zero? left) (right str))
       (else (right (let ((len (string-length str)))
		      (if (positive? left)
			  (if (< left len)
			      (substring str 0 left)
			      str)
			  (if (positive? (+ len left))
			      (substring str (abs left) len)
			      "")))))))
     ((zero? right)
      (cond
       ((string? left) (string-append left str))
       ((procedure? left) (left str))
       ((zero? left) str)
       (else (let ((len (string-length str)))
	       (if (positive? left)
		   (if (< left len)
		       (substring str 0 left)
		       str)
		   (if (positive? (+ len left))
		       (substring str (abs left) len)
		       ""))))))
     (else
      (let* ((lt-str (cond
		      ((string? left) (string-append left str))
		      ((procedure? left) (left str))
		      ((zero? left) str)
		      (else (let ((len (string-length str)))
			      (if (positive? left)
				  (if (< left len)
				      (substring str 0 left)
				      str)
				  (if (positive? (+ len left))
				      (substring str (abs left) len)
				      ""))))))
	     (lt-len (string-length lt-str)))
	(if (negative? right)
	    (if (positive? (+ lt-len right))
		(substring lt-str 0 (+ lt-len right))
		"")
	    (if (< right lt-len)
		(substring lt-str (- lt-len right) lt-len)
		lt-str)))))))

(define (str-char-index str char start end)
  (let lp ((n start))
    (if (= n end)
	#f
	(if (char=? char (string-ref str n))
	    n
	    (lp (+ n 1))))))

(define (str-num-index str start end)
  (let lp ((n start))
    (if (= n end)
	#f
	(if (char-numeric? (string-ref str n))
	    n
	    (lp (+ n 1))))))

(define (str-xnum-index str start end)
  (let lp ((n start))
    (if (= n end)
	#f
	(let ((ch (string-ref str n)))
	  (if (or (char-numeric? ch)
		  (memv ch '(#\a #\b #\c #\d #\e #\f
			     #\A #\B #\C #\D #\E #\F)))
	      n
	      (lp (+ n 1)))))))

(define (str-numeric? str start end)
  (let lp ((n start))
    (if (= n end)
	#t
	(if (char-numeric? (string-ref str n))
	    (lp (+ n 1))
	    #f))))

(define (num-separate str sep num sig)
  (let* ((len (string-length str))
	 (dot-index (str-char-index str #\. 1 len)))
    (if dot-index
	(if sig
	    (if (and (str-numeric? str 1 dot-index)
		     (str-numeric? str (+ 1 dot-index) len))
		(string-append
		 (apply string-append
			(let loop ((ini 0)
				   (pos (+ 1 (let ((pos (remainder
							 (- dot-index 1) num)))
					       (if (zero? pos) num pos)))))
			  (if (< pos dot-index)
			      (cons (substring str ini pos)
				    (cons sep (loop pos (+ pos num))))
			      (list (substring str ini dot-index)))))
		 "."
		 (apply string-append
			(let loop ((ini (+ 1 dot-index))
				   (pos (+ 1 dot-index num)))
			  (if (< pos len)
			      (cons (substring str ini pos)
				    (cons sep (loop pos (+ pos num))))
			      (list (substring str ini len))))))
		str)
	    (if (and (str-numeric? str 0 dot-index)
		     (str-numeric? str (+ 1 dot-index) len))
		(string-append
		 (apply string-append
			(let loop ((ini 0)
				   (pos (let ((pos (remainder dot-index num)))
					  (if (zero? pos) num pos))))
			  (if (< pos dot-index)
			      (cons (substring str ini pos)
				    (cons sep (loop pos (+ pos num))))
			      (list (substring str ini dot-index)))))
		 "."
		 (apply string-append
			(let loop ((ini (+ 1 dot-index))
				   (pos (+ 1 dot-index num)))
			  (if (< pos len)
			      (cons (substring str ini pos)
				    (cons sep (loop pos (+ pos num))))
			      (list (substring str ini len))))))
		str))
	(if sig
	    (if (str-numeric? str 1 len)
		(apply string-append
		       (let loop ((ini 0)
				  (pos (+ 1 (let ((pos (remainder (- len 1)
								  num)))
					      (if (zero? pos) num pos)))))
			 (if (< pos len)
			     (cons (substring str ini pos)
				   (cons sep (loop pos (+ pos num))))
			     (list (substring str ini len)))))
		str)
	    (if (str-numeric? str 0 len)
		(apply string-append
		       (let loop ((ini 0)
				  (pos (let ((pos (remainder len num)))
					 (if (zero? pos) num pos))))
			 (if (< pos len)
			     (cons (substring str ini pos)
				   (cons sep (loop pos (+ pos num))))
			     (list (substring str ini len)))))
		str)))))

(define (str-separate str sep num)
  (let ((len (string-length str))
	(n (abs num)))
    (apply string-append
	   (let loop ((ini 0)
		      (pos (if (negative? num)
			       n
			       (let ((pos (remainder len n)))
				 (if (zero? pos) n pos)))))
	     (if (< pos len)
		 (cons (substring str ini pos)
		       (cons sep (loop pos (+ pos n))))
		 (list (substring str ini len)))))))

(define (fix-mold str pre)
  (let* ((len (string-length str))
	 (ind (str-char-index str #\. 0 len)))
    (if ind
	(let ((d-len (- len (+ ind 1))))
	  (cond
	   ((= d-len pre) str)
	   ((< d-len pre) (string-append str (make-string (- pre d-len) #\0)))
	   ;;((char<? #\4 (string-ref str (+ 1 ind pre)))
	   ;;(let ((com (expt 10 pre)))
	   ;;  (number->string (/ (round (* (string->number str) com)) com))))
	   ((or (char<? #\5 (string-ref str (+ 1 ind pre)))
		(and (char=? #\5 (string-ref str (+ 1 ind pre)))
		     (or (< (+ 1 pre) d-len)
			 (memv (string-ref str (+ ind (if (= 0 pre) -1 pre)))
			       '(#\1 #\3 #\5 #\7 #\9)))))
	    (apply
	     string
	     (let* ((minus (char=? #\- (string-ref str 0)))
		    (str (substring str (if minus 1 0) (+ 1 ind pre)))
		    (char-list
		     (reverse
		      ;;(let lp ((index (- (string-length str) 1))
		      (let lp ((index (- (+ ind pre) (if minus 1 0)))
			       (raise #t))
			(if (= -1 index)
			    (if raise '(#\1) '())
			    (let ((chr (string-ref str index)))
			      (if (char=? #\. chr)
				  (cons chr (lp (- index 1) raise))
				  (if raise
				      (if (char=? #\9 chr)
					  (cons #\0 (lp (- index 1) raise))
					  (cons (integer->char
						 (+ 1 (char->integer chr)))
						(lp (- index 1) #f)))
				      (cons chr (lp (- index 1) raise))))))))))
	       (if minus (cons #\- char-list) char-list))))
	   (else
	    (substring str 0 (+ 1 ind pre)))))
	(string-append str "." (make-string pre #\0)))))

(define (fix-mold-nan-infinite str pre)
  (let* ((len (string-length str))
	 (ind (str-char-index str #\. 1 len))
	 (d-len (- len (+ ind 1))))
    (if (char-numeric? (string-ref str (- ind 1)))
	(cond
	 ((= d-len pre) str)
	 ((< d-len pre) (string-append str (make-string (- pre d-len) #\0)))
	 ;;((char<? #\4 (string-ref str (+ 1 ind pre)))
	 ;;(let ((com (expt 10 pre)))
	 ;;  (number->string (/ (round (* (string->number str) com)) com))))
	 ((or (char<? #\5 (string-ref str (+ 1 ind pre)))
	      (and (char=? #\5 (string-ref str (+ 1 ind pre)))
		   (or (< (+ 1 pre) d-len)
		       (memv (string-ref str (+ ind (if (= 0 pre) -1 pre)))
			     '(#\1 #\3 #\5 #\7 #\9)))))
	  (apply
	   string
	   (let* ((minus (char=? #\- (string-ref str 0)))
		  (str (substring str (if minus 1 0) (+ 1 ind pre)))
		  (char-list
		   (reverse
		    ;;(let lp ((index (- (string-length str) 1))
		    (let lp ((index (- (+ ind pre) (if minus 1 0)))
			     (raise #t))
		      (if (= -1 index)
			  (if raise '(#\1) '())
			  (let ((chr (string-ref str index)))
			    (if (char=? #\. chr)
				(cons chr (lp (- index 1) raise))
				(if raise
				    (if (char=? #\9 chr)
					(cons #\0 (lp (- index 1) raise))
					(cons (integer->char
					       (+ 1 (char->integer chr)))
					      (lp (- index 1) #f)))
				    (cons chr (lp (- index 1) raise))))))))))
	     (if minus (cons #\- char-list) char-list))))
	 (else
	  (substring str 0 (+ 1 ind pre))))
	(error "fox: infinities or nans cannot have precisions" str pre))))

(define (num-mold str pre)
  (let* ((len (string-length str))
	 (e-index (str-char-index str #\e 1 (- len 1))))
    (if e-index
	(string-append (fix-mold (substring str 0 e-index) pre)
		       (substring str e-index len))
	(fix-mold-nan-infinite str pre))))

(define (exp-mold str pre)
  (let* ((len (string-length str))
	 (e-index (str-char-index str #\e 1 (- len 1))))
    (string-append (fix-mold (substring str 0 e-index) pre)
		   (substring str e-index len))))

;; (define (remove-zero str len negative)
;;   (if negative
;;	 (let lp ((n 1))
;;	(let ((c (string-ref str n)))
;;	  (cond
;;	   ((char=? #\0 c) (lp (+ 1 n)))
;;	   ((char=? #\. c)
;;	    (if (= n 2)
;;		str
;;		(string-append "-" (substring str (- n 1) len))))
;;	   (else
;;	    (if (= n 1)
;;		str
;;		(string-append "-" (substring str n len)))))))
;;	 (let lp ((n 0))
;;	(let ((c (string-ref str n)))
;;	  (cond
;;	   ((char=? #\0 c) (lp (+ 1 n)))
;;	   ((char=? #\. c)
;;	    (if (= n 1)
;;		str
;;		(substring str (- n 1) len)))
;;	   (else
;;	    (if (zero? n)
;;		str
;;		(substring str n len))))))))

(define (make-fix n)
  (let* ((str (number->string (exact->inexact n)))
	 (len (string-length str))
	 (e-index (str-char-index str #\e 1 (- len 1))))
    (if e-index
	(let ((e-number (string->number (substring str (+ 1 e-index) len)))
	      (d-index (str-char-index str #\. 1 e-index)))
	  (if (negative? e-number)
	      (if d-index
		  (if (negative? n)
		      (let ((p-number (- (abs e-number) (- d-index 1))))
			(if (negative? p-number)
			    (let ((pnumber (+ 1 (abs p-number))))
			      (string-append (substring str 0 pnumber)
					     "."
					     (substring str pnumber d-index)
					     (substring str (+ 1 d-index)
							e-index)))
			    (string-append "-0."
					   (make-string p-number #\0)
					   (substring str 1 d-index)
					   (substring str (+ 1 d-index)
						      e-index))))
		      (let ((p-number (- (abs e-number) d-index)))
			(if (negative? p-number)
			    (let ((pnumber (abs p-number)))
			      (string-append (substring str 0 pnumber)
					     "."
					     (substring str pnumber d-index)
					     (substring str (+ 1 d-index)
							e-index)))
			    (string-append "0."
					   (make-string p-number #\0)
					   (substring str 0 d-index)
					   (substring str (+ 1 d-index)
						      e-index)))))
		  (if (negative? n)
		      (let ((p-number (- (abs e-number) (- e-index 1))))
			(if (negative? p-number)
			    (let ((pnumber (+ 1 (abs p-number))))
			      (string-append (substring str 0 pnumber)
					     "."
					     (substring str pnumber e-index)))
			    (string-append "-0."
					   (make-string p-number #\0)
					   (substring str 1 e-index))))
		      (let ((p-number (- (abs e-number) e-index)))
			(if (negative? p-number)
			    (let ((pnumber (abs p-number)))
			      (string-append (substring str 0 pnumber)
					     "."
					     (substring str pnumber e-index)))
			    (string-append "0."
					   (make-string p-number #\0)
					   (substring str 0 e-index))))))
	      (if d-index
		  (let ((p-number (- e-number (- e-index (+ d-index 1)))))
		    (if (negative? p-number)
			;; A procedure REMOVE-ZERO is unnecessary
			;; due to number->string.
			;; 0.00123 -> 00.0123 or 000123
			;; -0.00123 -> -00.0123 or -000123
			;;(remove-zero (string-append
			;;	      (substring str 0 d-index)
			;;	      (substring str (+ 1 d-index)
			;;			 (+ 1 d-index e-number))
			;;	      "."
			;;	      (substring str (+ 1 d-index e-number)
			;;			 e-index))
			;;	     e-index
			;;	     (< n 0))
			(string-append (substring str 0 d-index)
				       (substring str (+ 1 d-index)
						  (+ 1 d-index e-number))
				       "."
				       (substring str (+ 1 d-index e-number)
						  e-index))
			;; A procedure REMOVE-ZERO is unnecessary
			;; due to number->string.
			;; 0.00123 -> 00.0123 or 000123
			;; -0.00123 -> -00.0123 or -000123
			;;(remove-zero (string-append
			;;	      (substring str 0 d-index)
			;;	      (substring str (+ 1 d-index) e-index)
			;;	      (make-string p-number #\0)
			;;	      ".0")
			;;	     (+ e-index p-number 1)
			;;	     (< n 0))))
			(string-append (substring str 0 d-index)
				       (substring str (+ 1 d-index) e-index)
				       (make-string p-number #\0) ".0")))
		  (string-append (substring str 0 e-index)
				 (make-string e-number #\0)
				 ".0"))))
	(let ((d-index (str-char-index str #\. 1 len)))
	  (if (char-numeric? (string-ref str (- d-index 1)))
	      str
	      (error "fox: infinities or nans cannot be changed into fixed-format floating-point numbers" n))))))

(define (non-0-index str start)
  (let lp ((n start))
    (if (char=? #\0 (string-ref str n))
	(lp (+ 1 n))
	 n)))

(define (non-0-index-right str end)
  (let lp ((n (- end 1)))
    (if (char=? #\0 (string-ref str n))
	(lp (- n 1))
	n)))

;; (define (non-0-dot-index-right str end)
;;   (let lp ((n (- end 1)))
;;     (let ((c (string-ref str n)))
;;	 (if (or (char=? #\0 c) (char=? #\. c))
;;	  (lp (- n 1))
;;	  n))))

(define (make-exp n)
  (let* ((str (number->string (exact->inexact n)))
	 (len (string-length str))
	 (e-index (str-char-index str #\e 1 (- len 1))))
    (if e-index
	str
	(let ((d-index (str-char-index str #\. 1 len)))
	  (if (< -1 n 1)
	      (if (zero? n)
		  (string-append str "e+0") ;for -0.0 or +0.0
		  (let ((n-index (non-0-index str (+ 1 d-index))))
		    (string-append (if (negative? n) "-" "")
				   (substring str n-index (+ 1 n-index))
				   "."
				   (if (= n-index (- len 1))
				       "0"
				       (substring str (+ 1 n-index) len))
				   "e-"
				   (number->string (- n-index d-index)))))
	      ;;(let ((n-index (non-0-dot-index-right str len)))
	      ;;  (if (< n-index d-index)
	      (let ((n-index (non-0-index-right str len)))
		(if (= n-index d-index)
		    (let ((n-index (non-0-index-right str d-index)))
		      (if (char-numeric? (string-ref str n-index))
			  (if (negative? n)
			      (string-append (substring str 0 2)
					     "."
					     (if (= n-index 1)
						 "0"
						 (substring str 2
							    (+ 1 n-index)))
					     "e+"
					     (number->string (- d-index 2)))
			      (string-append (substring str 0 1)
					     "."
					     (if (= n-index 0)
						 "0"
						 (substring str 1
							    (+ 1 n-index)))
					     "e+"
					     (number->string (- d-index 1))))
			  (error "fox: infinities or nans cannot be changed into floating-point numbers" n)))
		    (if (negative? n)
			(string-append (substring str 0 2)
				       "."
				       (substring str 2 d-index)
				       (substring str (+ 1 d-index)
						  (+ 1 n-index))
				       "e+"
				       (number->string (- d-index 2)))
			(string-append (substring str 0 1)
				       "."
				       (substring str 1 d-index)
				       (substring str (+ 1 d-index)
						  (+ 1 n-index))
				       "e+"
				       (number->string (- d-index 1)))))))))))

;; define-macro
(define-macro (let-fox* z vars . body)
  (let ((var (car vars)))
    (let ((n (car var)) (d (cadr var)) (t (caddr var)))
      (if (null? (cdr vars))
	  `(let ((,n (if (null? ,z)
			 ,d
			 (if (null? (cdr ,z))
			     (let ((,n (car ,z)))
			       (if ,t ,n (error 'fox "too many argument" ,z)))
			     (error 'fox "too many arguments" ,z)))))
	     ,@body)
	  (let ((head (gensym)) (tail (gensym)))
	    `(let ((,n (if (null? ,z)
			   ,d
			   (let ((,n (car ,z)))
			     (if ,t
				 (begin (set! ,z (cdr ,z)) ,n)
				 (let lp ((,head (list ,n)) (,tail (cdr ,z)))
				   (if (null? ,tail)
				       ,d
				       (let ((,n (car ,tail)))
					 (if ,t
					     (begin
					       (set! ,z (append (reverse ,head)
								(cdr ,tail)))
					       ,n)
					     (lp (cons ,n ,head)
						 (cdr ,tail)))))))))))
	       (let-fox* ,z ,(cdr vars) ,@body)))))))

;; define-syntax
;; (define-syntax let-fox*
;;   (syntax-rules ()
;;     ((let-fox* z ((n d t)) bd ...)
;;	(let ((n (if (null? z)
;;		  d
;;		  (if (null? (cdr z))
;;		      (let ((n (car z)))
;;			(if t n (error 'fox "too many argument" z)))
;;		      (error 'fox "too many arguments" z)))))
;;	  bd ...))
;;     ((let-fox* z ((n d t) ndt ...) bd ...)
;;	(let ((n (if (null? z)
;;		  d
;;		  (let ((n (car z)))
;;		    (if t
;;			(begin (set! z (cdr z)) n)
;;			(let lp ((head (list n)) (tail (cdr z)))
;;			  (if (null? tail)
;;			      d
;;			      (let ((n (car tail)))
;;				(if t
;;				    (begin (set! z (append (reverse head)
;;							   (cdr tail)))
;;					   n)
;;				    (lp (cons n head) (cdr tail)))))))))))
;;	  (let-fox* z (ndt ...) bd ...)))))

;; (define (integer/string/procedure? is)
;;   (or (integer? is) (string? is) (procedure? is)))

(define (fox object . rest)
  (if (null? rest)
      (cond
       ((number? object) (number->string object))
       ((string? object) object)
       ((symbol? object) (symbol->string object))
       ((char? object) (string object))
       ((boolean? object) (if object "#t" "#f"))
       (else (object->string object display)))
      ;; partition: pre-string, optional arguments, post-strings
      (let* ((pre-str #f)
	     (strs (let ((first (car rest)))
		     (if (string? first)
			 (if (or (null? (cdr rest)) (string? (cadr rest)))
			     (let ((tmp rest)) (set! rest '()) tmp)
			     (let lp ((head (list (cadr rest)))
				      (tail (cddr rest)))
			       (if (null? tail)
				   (begin (set! pre-str first)
					  (set! rest (cdr rest))
					  #f)
				   (let ((arg (car tail)))
				     (if (string? arg)
					 (begin (set! pre-str first)
						(set! rest (reverse head))
						tail)
					 (lp (cons arg head)
					     (cdr tail)))))))
			 (let lp ((head (list first))
				  (tail (cdr rest)))
			   (if (null? tail)
			       #f
			       (let ((arg (car tail)))
				 (if (string? arg)
				     (begin (set! rest (reverse head))
					    tail)
				     (lp (cons arg head)
					 (cdr tail))))))))))
	(if (null? rest)
	    (apply string-append 
		   (cond
		    ((number? object) (number->string object))
		    ((string? object) object)
		    ((symbol? object) (symbol->string object))
		    ((char? object) (string object))
		    ((boolean? object) (if object "#t" "#f"))
		    (else (object->string object display)))
		   strs)
	    (let-fox* rest
	      ((port #f (or (boolean? port) (output-port? port))) ;boolean, port
	       (width 0 (integer? width))			  ;integer
	       (char #\space (char? char))			  ;char
	       (list-for-number #f (list? list-for-number))	  ;list
	       (writer display (procedure? writer))		  ;procedure
	       (converter #f (pair? converter))			  ;pair
	       (separator #f (vector? separator)))		  ;vector
	      (let ((str
(if (number? object)
    (if (and list-for-number (or (eq? writer display) (eq? writer write)))
	(let-fox* list-for-number
	  ((precision #f (and (integer? precision) (not (negative? precision))))
	   (point #f (memq point '(fixed floating)))
	   (radix 'decimal (memq radix '(decimal octal binary hexadecimal)))
	   (sign #f (eq? 'sign sign))
	   (exactness #f (memq exactness '(exact inexact))))
	  (let* ((inexact-sign
		  (and (not (eq? radix 'decimal))
		       (or (and (or precision point)
				(error "fox: non-decimal cannot have a decimal point" radix precision))
			   (and (inexact? object) (not (eq? exactness 'exact)))
			   (eq? exactness 'inexact))
		       "#i"))
		 (str
		  (cond
		   (point
		    (if (eq? point 'fixed)
			;; fixed-point
			(if precision
			    (if (real? object)
				(fix-mold (make-fix object) precision)
				(let ((imag-str (make-fix (imag-part object))))
				  (string-append
				   (fix-mold (make-fix (real-part object))
					     precision)
				   ;; for N+0.0i
				   ;; (if (char-numeric? (string-ref imag-str 0))
				   ;;	  "+" "")
				   (if (char=? #\- (string-ref imag-str 0))
				       "" "+")
				   (fix-mold imag-str precision)
				   "i")))
			    (if (real? object)
				(make-fix object)
				(let ((imag-str (make-fix (imag-part object))))
				  (string-append
				   (make-fix (real-part object))
				   ;; for N+0.0i
				   ;; (if (char-numeric? (string-ref imag-str 0))
				   ;;	  "+" "")
				   (if (char=? #\- (string-ref imag-str 0))
				       "" "+")
				   imag-str
				   "i"))))
			;; floating-point
			(if precision
			    (if (real? object)
				(exp-mold (make-exp object) precision)
				(let ((imag-str (make-exp (imag-part object))))
				  (string-append
				   (exp-mold (make-exp (real-part object))
					     precision)
				   ;; for N+0.0i
				   ;; (if (char-numeric? (string-ref imag-str 0))
				   ;;	  "+" "")
				   (if (char=? #\- (string-ref imag-str 0))
				       "" "+")
				   (exp-mold imag-str precision)
				   "i")))
			    (if (real? object)
				(make-exp object)
				(let ((imag-str (make-exp (imag-part object))))
				  (string-append
				   (make-exp (real-part object))
				   ;; for N+0.0i
				   ;; (if (char-numeric? (string-ref imag-str 0))
				   ;;	  "+" "")
				   (if (char=? #\- (string-ref imag-str 0))
				       "" "+")
				   imag-str
				   "i"))))))
		   (precision
		    (if (real? object)
			(num-mold (number->string (exact->inexact object))
				  precision)
			(let ((imag-str (number->string
					 (exact->inexact (imag-part object)))))
			  (string-append
			   (num-mold (number->string
				      (exact->inexact (real-part object)))
				     precision)
			   ;; for N+0.0i
			   ;; (if (char-numeric? (string-ref imag-str 0))
			   ;;	  "+" "")
			   (if (char=? #\- (string-ref imag-str 0))
			       "" "+")
			   (num-mold imag-str precision)
			   "i"))))
		   (else
		    (number->string
		     (cond
		      (inexact-sign (inexact->exact object))
		      (exactness (if (eq? exactness 'exact)
				     (inexact->exact object)
				     (exact->inexact object)))
		      (else object))
		     (cdr (assq radix '((decimal . 10)
					(octal . 8)
					(hexadecimal . 16)
					(binary . 2))))))))
		 (str (if (and separator (eq? radix 'decimal))
			  (if (string? (vector-ref separator 0))
			      (num-separate str
					    (vector-ref separator 0)
					    (if (= 1 (vector-length separator))
						3
						(abs (vector-ref separator 1)))
					    (negative? (real-part object)))
			      (num-separate str
					    (if (= 1 (vector-length separator))
						","
						(vector-ref separator 1))
					    (abs (vector-ref separator 0))
					    (negative? (real-part object))))
			  str))
		 (str (string-append
		       (or inexact-sign "")
		       (if (and (eq? exactness 'exact) (or precision point))
			   "#e" "")
		       (cdr (assq radix '((decimal . "")
					  (octal . "#o")
					  (hexadecimal . "#x")
					  (binary . "#b"))))
		       (if (and sign
				;;(positive? (real-part object)))
				;; for 0.0
				(let ((ch (string-ref str 0)))
				  (not (or (char=? #\- ch)
					   (char=? #\+ ch))))) ;for +inf.0
			   "+" "")
		       str))
		 (str (if converter (str-convert str converter) str))
		 (pad (- (abs (if (exact? width) width (inexact->exact width)))
			 (string-length str))))
	    ;; The following use infinite? and nan? predicates.
	    ;; (cond
	    ;;	((<= pad 0) str)
	    ;;	((inexact? width)
	    ;;	 (let* ((head ((if (positive? width) ceiling floor) (/ pad 2)))
	    ;;	     (tail (- pad head)))
	    ;;	(if (eq? radix 'hexadecimal)
	    ;;	    (if (or (char-numeric? char)
	    ;;		    (memv char '(#\a #\b #\c #\d #\e #\f
	    ;;				 #\A #\B #\C #\D #\E #\F)))
	    ;;		(let* ((len (string-length str))
	    ;;		       (index (str-xnum-index str 0 len)))
	    ;;		  (if index
	    ;;		      (string-append (substring str 0 index)
	    ;;				     (make-string head char)
	    ;;				     (substring str index len)
	    ;;				     (make-string tail char))
	    ;;		      (string-append (make-string head char)
	    ;;				     str
	    ;;				     (make-string tail char))))
	    ;;		(string-append (make-string head char)
	    ;;			       str
	    ;;			       (make-string tail char)))
	    ;;	    (if (char-numeric? char)
	    ;;		(if (or (infinite? object) (nan? object))
	    ;;		    (string-append (make-string pad char) str)
	    ;;		    (let* ((len (string-length str))
	    ;;			   (index (str-num-index str 0 len)))
	    ;;		      (if index
	    ;;			  (string-append (substring str 0 index)
	    ;;					 (make-string head char)
	    ;;					 (substring str index len)
	    ;;					 (make-string tail char))
	    ;;			  (string-append (make-string head char)
	    ;;					 str
	    ;;					 (make-string tail char)))))
	    ;;		(string-append (make-string head char)
	    ;;			       str
	    ;;			       (make-string tail char))))))
	    ;;	((positive? width)
	    ;;	 (if (eq? radix 'hexadecimal)
	    ;;	  (if (or (char-numeric? char)
	    ;;		  (memv char '(#\a #\b #\c #\d #\e #\f
	    ;;			       #\A #\B #\C #\D #\E #\F)))
	    ;;	      (let* ((len (string-length str))
	    ;;		     (index (str-xnum-index str 0 len)))
	    ;;		(if index
	    ;;		    (string-append (substring str 0 index)
	    ;;				   (make-string pad char)
	    ;;				   (substring str index len))
	    ;;		    (string-append (make-string pad char) str)))
	    ;;	      (string-append (make-string pad char) str))
	    ;;	  (if (char-numeric? char)
	    ;;	      (if (or (infinite? object) (nan? object))
	    ;;		  (string-append (make-string pad char) str)
	    ;;		  (let* ((len (string-length str))
	    ;;			 (index (str-num-index str 0 len)))
	    ;;		    (if index
	    ;;			(string-append (substring str 0 index)
	    ;;				       (make-string pad char)
	    ;;				       (substring str index len))
	    ;;			(string-append (make-string pad char) str))))
	    ;;	      (string-append (make-string pad char) str))))
	    ;;	(else (string-append str (make-string pad char))))))
	    (cond
	     ((<= pad 0) str)
	     ((inexact? width)
	      (let* ((head ((if (positive? width) ceiling floor) (/ pad 2)))
		     (tail (- pad head)))
		(if (eq? radix 'hexadecimal)
		    (if (or (char-numeric? char)
			    (memv char '(#\a #\b #\c #\d #\e #\f
					 #\A #\B #\C #\D #\E #\F)))
			(let* ((len (string-length str))
			       (index (str-xnum-index str 0 len)))
			  (if index
			      (string-append (substring str 0 index)
					     (make-string head char)
					     (substring str index len)
					     (make-string tail char))
			      (string-append (make-string head char)
					     str
					     (make-string tail char))))
			(string-append (make-string head char)
				       str
				       (make-string tail char)))
		    (if (char-numeric? char)
			(let* ((len (string-length str))
			       (index (str-num-index str 0 len)))
			  (if index
			      (if (or (zero? index)
				      ;; for infinities and nans
				      (char=? (string-ref str (- index 1)) #\.))
				  (string-append (make-string head char)
						 str
						 (make-string tail char))
				  (string-append (substring str 0 index)
						 (make-string head char)
						 (substring str index len)
						 (make-string tail char)))
			      (string-append (make-string head char)
					     str
					     (make-string tail char))))
			(string-append (make-string head char)
				       str
				       (make-string tail char))))))
	     ((positive? width)
	      (if (eq? radix 'hexadecimal)
		  (if (or (char-numeric? char)
			  (memv char '(#\a #\b #\c #\d #\e #\f
				       #\A #\B #\C #\D #\E #\F)))
		      (let* ((len (string-length str))
			     (index (str-xnum-index str 0 len)))
			(if index
			    (string-append (substring str 0 index)
					   (make-string pad char)
					   (substring str index len))
			    (string-append (make-string pad char) str)))
		      (string-append (make-string pad char) str))
		  (if (char-numeric? char)
		      (let* ((len (string-length str))
			     (index (str-num-index str 0 len)))
			(if index
			    (if (or (zero? index)
				    ;; for infinities and nans
				    (char=? (string-ref str (- index 1)) #\.))
				(string-append (make-string pad char) str)
				(string-append (substring str 0 index)
					       (make-string pad char)
					       (substring str index len)))
			    (string-append (make-string pad char) str)))
		      (string-append (make-string pad char) str))))
	     (else (string-append str (make-string pad char))))))
	(let* ((str (if (or (eq? writer display) (eq? writer write))
			(number->string object)
			(object->string object writer)))
	       (str (if separator
			(if (string? (vector-ref separator 0))
			    (num-separate str
					  (vector-ref separator 0)
					  (if (= 1 (vector-length separator))
					      3
					      (abs (vector-ref separator 1)))
					  (negative? (real-part object)))
			    (num-separate str
					  (if (= 1 (vector-length separator))
					      ","
					      (vector-ref separator 1))
					  (abs (vector-ref separator 0))
					  (negative? (real-part object))))
			str))
	       (str (if converter (str-convert str converter) str))
	       (pad (- (abs (if (exact? width) width (inexact->exact width)))
		       (string-length str))))
	  ;; The following use infinite? and nan? predicates.
	  ;; (cond
	  ;;  ((<= pad 0) str)
	  ;;  ((inexact? width)
	  ;;	(let* ((head ((if (positive? width) ceiling floor) (/ pad 2)))
	  ;;	       (tail (- pad head)))
	  ;;	  (if (char-numeric? char)
	  ;;	      (if (or (infinite? object) (nan? object))
	  ;;		  (string-append (make-string pad char) str)
	  ;;		  (let* ((len (string-length str))
	  ;;			 (index (str-num-index str 0 len)))
	  ;;		    (if index
	  ;;			(string-append (substring str 0 index)
	  ;;				       (make-string head char)
	  ;;				       (substring str index len)
	  ;;				       (make-string tail char))
	  ;;			(string-append (make-string head char)
	  ;;				       str
	  ;;				       (make-string tail char)))))
	  ;;	      (string-append (make-string head char)
	  ;;			     str
	  ;;			     (make-string tail char)))))
	  ;;  ((positive? width)
	  ;;	(if (char-numeric? char)
	  ;;	    (if (or (infinite? object) (nan? object))
	  ;;		(string-append (make-string pad char) str)
	  ;;		(let* ((len (string-length str))
	  ;;		       (index (str-num-index str 0 len)))
	  ;;		  (if index
	  ;;		      (string-append (substring str 0 index)
	  ;;				     (make-string pad char)
	  ;;				     (substring str index len))
	  ;;		      (string-append (make-string pad char) str))))
	  ;;	    (string-append (make-string pad char) str)))
	  ;;  (else (string-append str (make-string pad char))))))
	  (cond
	   ((<= pad 0) str)
	   ((inexact? width)
	    (let* ((head ((if (positive? width) ceiling floor) (/ pad 2)))
		   (tail (- pad head)))
	      (if (char-numeric? char)
		  (let* ((len (string-length str))
			 (index (str-num-index str 0 len)))
		    (if index
			(if (or (zero? index)
				;; for infinities and nans
				(char=? (string-ref str (- index 1)) #\.))
			    (string-append (make-string head char)
					   str
					   (make-string tail char))
			    (string-append (substring str 0 index)
					   (make-string head char)
					   (substring str index len)
					   (make-string tail char)))
			(string-append (make-string head char)
				       str
				       (make-string tail char))))
		  (string-append (make-string head char)
				 str
				 (make-string tail char)))))
	   ((positive? width)
	    (if (char-numeric? char)
		(let* ((len (string-length str))
		       (index (str-num-index str 0 len)))
		  (if index
		      (if (or (zero? index)
			      ;; for infinities and nans
			      (char=? (string-ref str (- index 1)) #\.))
			  (string-append (make-string pad char) str)
			  (string-append (substring str 0 index)
					 (make-string pad char)
					 (substring str index len)))
		      (string-append (make-string pad char) str)))
		(string-append (make-string pad char) str)))
	   (else (string-append str (make-string pad char))))))
    (let* ((str (if (eq? writer display)
		    (cond
		     ((string? object) object)
		     ((symbol? object) (symbol->string object))
		     ((char? object) (string object))
		     ((boolean? object) (if object "#t" "#f"))
		     (else (object->string object writer)))
		    (if (eq? writer write)
			(cond
			 ((symbol? object) (symbol->string object))
			 ((char? object) (string-append "#\\" (string object)))
			 ((boolean? object) (if object "#t" "#f"))
			 (else (object->string object writer)))
			(object->string object writer))))
	   (str (if (and separator (= 2 (vector-length separator)))
		    (if (string? (vector-ref separator 0))
			(str-separate str
				      (vector-ref separator 0)
				      (vector-ref separator 1))
			(str-separate str
				      (vector-ref separator 1)
				      (vector-ref separator 0)))
		    str))
	   (str (if converter (str-convert str converter) str))
	   (pad (- (abs (if (exact? width) width (inexact->exact width)))
		   (string-length str))))
      (cond
       ((<= pad 0) str)
       ((inexact? width)
	(let* ((head ((if (positive? width) ceiling floor) (/ pad 2)))
	       (tail (- pad head)))
	  (string-append (make-string head char) str (make-string tail char))))
       ((positive? width)
	(string-append (make-string pad char) str))
       (else (string-append str (make-string pad char))))))))
(if port
    (let ((port (if (eq? port #t) (current-output-port) port)))
      (if strs
	  ;; All of the different implementations have the different
	  ;; performance efficiency of `display' and `string-append'.
	  ;; 1. (display (apply string-append pre-str str strs) port)
	  ;; 2. (begin
	  ;;	  (display pre-str port)
	  ;;	  (display str port)
	  ;;	  (for-each (lambda (x) (display x port)) strs))
	  (if pre-str
	      (display (apply string-append pre-str str strs) port)
	      (display (apply string-append str strs) port))
	  (if pre-str
	      (display (string-append pre-str str) port)
	      (display str port))))
    (if strs
	(if pre-str
	    (apply string-append pre-str str strs)
	    (apply string-append str strs))
	(if pre-str
	    (string-append pre-str str)
	    str)))))))))

;;; eof
