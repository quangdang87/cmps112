#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.16 2019-01-20 13:41:07-08 - - $
;; Quang Dang
;; qvdang
;; ID#: 1673764
;;
;;NAME
;;	sbi.scm - silly basic interpeter
;;SYNOSIS
;;	sbi.scm filename.sbir
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
;; make hash table
;; *function-table*
(define *function-table* (make-hash) )
(define (function-put key value) 
	(hash-set! *function-table* key value)
)	
(define (function-get key)
	(hash-ref *function-table* key '())
)
;; *label-table*
(define *label-table* (make-hash))
(define (label-put key label)
	(hash-set! *label-table* key label)
)
(define (label-get key)
	(hash-ref *label-table* key #f)
)

;;*variable-table*
(define *variable-table* (make-hash))
(define (variables-put key value)
	(hash-set! *variable-table* key value)
)
(define (variables-get key)
	(hash-ref *variable-table* key #f)
)
;;*array-table*
(define *array-table* (make-hash))
(define (array-put key value)
	(hash-set! *array-table* key value)
)
(define (array-get key)
	(hash-ref *array-table* key #f)
)
;
;;define functions
(define (evaluate-expression expression)
	(when (not (null? expression))
		(cond [(pair? expression)
			(begin
				(apply (function-get (car expression))
					 (map evaluate-expression (cdr expression)))
			)]
			[(number? expression) expression]
			[(string? expression) expression]
			[else 
				(if (not (equal? (variables-get expression) #f))
					(variables-get expression)
					expression)]
		)
	)
)
;; interpret-printf
(define (interpret-printf expression)
	(when (not(null? expression))
		(if (pair? expression)
			(begin
				(if (string? (car expression))
					(printf (car expression))
					(begin
						(if (string? (evaluate-expression (car expression)))
							(printf (evaluate-expression (car expression)))
							(printf (number->string (evaluate-expression (car expression)))))
					)
				)
				(when (null? (cdr expression))
					(newline)
				)
				(interpret-printf (cdr expression))
			)
			(begin
				(cond [(string? expression) (printf expression)]
					[(number? expression) (printf (number->string expression))]
					[(symbol? expression) (printf (symbol->string expression))]
				[else (usage-error)])
				(newline)
			)
		)
	)					
) 
;; interpret-dim
(define (interpret-dim expression)
	(if (not (equal? (caar expression) 'asub))
		(begin
			(usage-error)
			(display "missing Arrayref in Syntax" *stderr*)
			(newline))
		(begin
			(if (not (number? (evaluate-expression (caddar expression))))
				(usage-error)
				(array-put (cadar expression)
					 (let ((x (make-vector
					 (+ (evaluate-expression (caddar expression)) 1)))) x)))))
)
;; interpret-let
(define (interpret-let expression) 
	(if (pair? expression)
		(cond [(pair? (car expression))
		;; check for "Memory" whether it is a variable or an Arrayref
			(if (not (equal? (caar expression) 'asub))
				(begin
					(usage-error)
					(display "missing Arrayref in Syntax" *stderr*)
					(newline)
				)
				(begin ;; if it's an Arrayref
					;;check is there a array
					;;with the given name in *array-table*
					;;if yes then update the value for it
					(if (equal? (evaluate-expression (car expression))
							 "No such an array exists")
						(begin 
							(printf (evaluate-expression (car expression)))
						)
						(begin
							(if (not (null? (cddr expression)))
								(usage-error)
								(begin
									(if (> (evaluate-expression (caddar expression)) 
										(vector-length (array-get (cadar expression))))
										(begin 
											(display "Range is out of bound of an array" *stderr*)
											(newline)
										)
										(vector-set! (array-get (cadar expression)) 
											(evaluate-expression (caddar expression)) 
												(evaluate-expression (cadr expression)))))
							)
						)
					)
				)
			)]
			[(symbol? (car expression))
				(begin;; esle it is a variable
					(variables-put (car expression) 
						(evaluate-expression (cadr expression)))
				)]
			[else (usage-error)]
		)
		(begin
			(usage-error)	
			(display "let Memory Expression" *stderr*)
		)
	)
)
	
(define (interpret-goto expression)
	;; expression = label is a symbol
	;; search label in *label-table* if there is such label exist
	;; jump to that line (value associate with that label)
	(when (not(null? expression ))
		(if (symbol? (car expression)) ;; valid label
			(begin
				(if (not (equal? (label-get (car expression)) #f))
					(begin
						(let* ((program (label-get (car expression))))
							(begin	
								(interpret-program program)
								(exit 0)
							))
					)	
					;; label has not found
					(begin
						(display "Label is undefined" *stderr*)
						(newline)
					)
				)
			)
			;;invalid label
			(begin
				(usage-error)
				(display "Label must be a symbol" *stderr*)
				(newline)
			)
		
		)
	)	
)
(define (interpret-if expression)
	(if (pair? (car expression))
		(let ((condition (car expression)) (expr (cdr expression)))
			(if (boolean? (evaluate-expression condition))
				(begin
					(if (evaluate-expression condition)
						(begin
							(if (null? expr)
								(begin
									(usage-error)
									(display "Missing Expression")
									(newline)
								)
								(begin
									;;assume that expression just have
									;;one pair (or symbol) after condition
									(cond [(symbol? (car expr)) (interpret-goto expr)]
										[(pair? (car expr)) (evaluate-expression expr)]
										[else (begin
											(usage-error)
											(display "Expression 
											must be a pair or a symbol" *stderr*)
											(newline))]	
									))))
						(begin
							(when (not(null? (cdr expr)))
								(cond [(symbol? (cadr expr)) (evaluate-expression (cdr expr))]
									[(pair? (cadr expr)) (evaluate-expression (cadr expr))]
									[else (begin
										(usage-error)
										(display "Expression must be a pair or a symbol" *stderr*)
										(newline))]
								)
							
							)
						)
					)	
				)
				(begin
					(display "Condition expression must be return boolean" *stderr*)
					(newline)
				)
			)
		)	
		(begin
			(usage-error)
			(display "Missing condition on If statement")
			(newline)
		)
	)
)
(define len (lambda (l)
	(define len.. (lambda (l.. n)
		(if (null? l..) n
			(len (cdr l) (+ n 1))))
	)
	(len.. l 0))
)

(define (interpret-input expression)	
	(define (input-get expr)
		(when (not (null? (car expr)))
			(let ((x (read)))
			(cond 	[(eof-object? x) 
				(let* ((y (variables-get 'nan))) (begin (variables-put 'eof 1) y))] 
				[(number? x) (variables-put (car expr) x)]
				[else ;; input is not a number
					(let* ((y (variables-get 'nan))) (variables-put (car expr) y))])
			)
			(when (not(null? (cdr expr)))
				(input-get (cdr expr))
			)
		)
	)
	(if (null? expression)
		(usage-error)
		(input-get expression)
	)
)
;; Implement functions
(for-each (lambda (pair)
	(function-put (car pair) (cadr pair)))
	`(
		(dim ,interpret-dim)
		(let ,interpret-let)
		(goto ,interpret-goto)
		(if ,interpret-if)
		(print ,interpret-printf)
		(input ,interpret-input)
		(= ,=)
		(< ,<)
		(> ,>)
		(<= ,<=)
		(>= ,>=)
		(<> ,(lambda (x y) (not(equal? x y))))
		(+ ,+)
		(- ,-)
		(* ,(lambda (x y) (if (or (not(number? (evaluate-expression x)))
					 (not(number? (evaluate-expression y))))
						(begin	
							(display "Variable is undefined" *stderr*)
							 (newline))
						(* (evaluate-expression x) (evaluate-expression y)))))
	
		(/ ,(lambda (x y) (if (or (not(number? (evaluate-expression x))) 
						(not(number? (evaluate-expression y))))
						(begin 
							(display "Variable is undefined" *stderr*)
							 (newline))
						(/ (evaluate-expression x) 
							(if (equal? (evaluate-expression y) 0) 0.0 
								(evaluate-expression y)))))) 
		(^ ,(lambda (x y) (if (or (not(number? (evaluate-expression x)))
					 (not(number? (evaluate-expression y))))
						(begin	
							(display "Variable is undefined" *stderr*)
							(newline))
						(expt (evaluate-expression x) (evaluate-expression y)))))		
		(exp ,exp)
		(abs ,abs)
		(round ,round)
		(% ,(lambda (x y) (- x (* (floor (/ x y)) y))))
		(quot ,(lambda (x y) (truncate (/ x y))))
		(rem ,(lambda (x y) (- x (* (truncate (/ x y)) y))))
		(ceil ,ceiling)
		(floor ,floor)
		(log ,(lambda (x) (log (if (equal? 0 x )0.0 x))))
		(sqrt ,sqrt)
		(sin ,sin) (cos ,cos) (tan ,tan)
		(asin ,asin) (acos ,acos)
		(atan ,(lambda (x) (atan (if (equal? x 0) 0.0 x))))
		(asub ,(lambda (x y) (begin
				(if (not (equal? (array-get x) #f))
					(vector-ref (array-get x) (evaluate-expression y))
					"No such an array exists"))))
	)
)
;; define special variable
(for-each (lambda (pair) 
	(variables-put (car pair) (cadr pair)))
	`(
		(pi ,3.141592653589793238462643383279502884197169399)
		(e ,2.718281828459045235360287471352662497757247093)
		(nan ,(/ 0.0 0.0))
		(eof ,0.0)
	)
)
(define *runfile* 
	(let-values
		(((dirpath basepath root?)
			(split-path (find-system-path 'run-file))))
		(path->string basepath))
)

(define (die list)
	(for-each (lambda (item) (display item *stderr*)) list)
	(newline)
	(exit 1)
)
(define (error-message message)
	((lambda (item) (display item *stderr*)) message)
	(newline)
)
(define (usage-error)
	(error-message `("Syntax error"))
)
(define (usage-exit)
	(die `("Usage:" ,*runfile* "filename"))
)

(define (readlist-from-inputfile filename) 
	(let ((inputfile (open-input-file filename)))
		(if (not (input-port? inputfile))
			(die `(,*runfile* ,filename ": open failed"))
			(let ((program (read inputfile)))
				(close-input-port inputfile)
					program)))
)
(define (execute expression)
	(when (not(null? expression))	
		;;check for valid expression and execute it 
		;;otherwise print the error
		(if (not(equal? (function-get (car expression)) '()))
			(begin
				(let ((expr (cdr expression)))
					(begin
						((function-get (car expression)) expr)
					)
				)
			)
			(begin
				(usage-error)
				(display "No such function in *function-table*")
				(newline)
			)
		)
	)
)
(define (interpret-program expression)
	(let* ((program expression)) 
	(when (pair? program)
		(when (not(null? (car program)))
			(if (null? (cdar program))
				(begin
					
					(interpret-program (cdr program))
				)
				(begin
					(let ((line (car program)))
						(when (not (null? (cdr line)))
							(cond [(pair?(cadr line))
								(begin
									(execute (cadr line))
								)]
								[(symbol? (cadr line))
								(begin
									(when (not (null? (cddr line)))
										(execute (caddr line)))
								)]
								[else 
									(begin
										(display "Control transfer must be Symbol" *stderr*)
										(newline)
								)]	
							)
						)
					)
					(interpret-program (cdr program))
				)			
			)
		)
	))
)
(define (add_label _list)
	(begin
		(let (( label (if (not (null? (cdar _list)))
				(if (symbol? (cadar _list))
					(cadar _list)
					'())
			'()))) (when (not(equal? label '())) 
					(begin
						(label-put label  _list)
					))
		)
		(when (not (null? (cdr _list)))
			(add_label (cdr _list)))
	)
)
(define (main args)
	(if (or (null? args) (not (null? (cdr args))))
		(usage-exit)
		(let* ((subprog (car args))
			(program (readlist-from-inputfile subprog)))
				;;checking for a label in each line and put it
				;; into the *lable-table*
				(begin
					(add_label program)	
					(interpret-program program)
				)
		)
	)
)

(main (vector->list (current-command-line-arguments)))	
