#lang racket

(require racket/runtime-path racket/file)

(provide server server-start server-running? server-shutdown 
         server-write server-read server-error server-initialize
         output-smt printf/current-server)

(define current-server (make-parameter #f))

; If true, log all SMT output to a temporary file
(define output-smt
  (make-parameter #f (lambda (on?)
                       (unless (or (boolean? on?) (path-string? on?) (output-port? on?))
                         (raise-argument-error 'output-smt "(or/c boolean? path-string? output-port?)" on?))
                       (if (path-string? on?) (expand-user-path on?) on?))))

; A server manages the creation, use, and shutdown of external processes.  
; The path field specifies the absolute path to the program to be called 
; in an external process, and the opts field specifies the list of options / arguments 
; with which to call the process. The init field is a procedure that takes as
; input the server struct, and initializes the corresponding process by
; interacting with its input and output ports.
; 
; The custodian, process, stdout, stdin, and stderr fields store the custodian, 
; process handle, standard output, standard input, and standard error for the 
; created process.  These fields are populated with non-false values when a 
; server is initialized A server can be initialized and shutdown any number of times.  
; Calling initialize on a server that has already been initialized has no effect.
(struct server 
  (path opts init
   [custodian #:auto #:mutable]
   [process   #:auto #:mutable] 
   [stdout    #:auto #:mutable] 
   [stdin     #:auto #:mutable] 
   [stderr    #:auto #:mutable]
   [log       #:auto #:mutable])
  #:auto-value #f
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "server(path=~a,opts=~a,pid=~a)" 
              (server-path self) (server-opts self) (server-process self)))])

(define (set-server-values! s custodian process stdout stdin stderr log)
  (set-server-custodian! s custodian)
  (set-server-process! s process)
  (set-server-stdout! s stdout)
  (set-server-stdin! s stdin)
  (set-server-stderr! s stderr)
  (set-server-log! s log))

; Returns true if the given server has been initialized and is running.
(define (server-running? s) 
  (define p (server-process s))
  (and p (equal? (subprocess-status p) 'running)))

; Initializes the given server if it's not already initialized and returns it.
(define (server-start s)
  (unless (server-running? s) 
    (parameterize ([current-custodian (make-custodian)]
                   [current-subprocess-custodian-mode 'kill])
      (define-values (p out in err) 
        (apply subprocess #f #f #f (server-path s) (server-opts s)))
      (set-server-values! s (current-custodian) p out in err (open-output-nowhere))
      (server-initialize s)))
  s)

; Invoke the server's initialize procedure
(define (server-initialize s)
  ((server-init s) s))

; Initialize the server's log output if required
(define (server-initialize-log s)
  (unless (file-stream-port? (server-log s))
    (cond
      [(output-port? (output-smt))
       (set-server-log! s (output-smt))]
      [else
       (when (path-string? (output-smt))
         (make-directory* (output-smt)))
       (define dir (if (path-string? (output-smt)) (output-smt) #f))
       (define log (make-temporary-file "rosette~a.smt2" #f dir))
       (eprintf "Outputting SMT to file: ~a\n" (path->string log))
       (set-server-log! s (open-output-file log #:exists 'truncate))]))
  (server-log s))

; Evaluates the given expression with current-server set
; to the given server, and returns the result.
; Initializes the server if it is not already running.
(define-syntax-rule (server-write s expr ...)
  (parameterize ([current-server (server-start s)])
    (begin0 
      expr ...
      (flush-output (server-stdin s))
      (flush-output (server-log s)))))

; Like printf, but with the output port set to the current server's
; stdin. Optionally logs the printed values to a file.
(define-syntax-rule (printf/current-server expr ...)
  (begin
    (when (output-smt)
      (fprintf (server-initialize-log (current-server)) expr ...))
    (fprintf (server-stdin (current-server)) expr ...)))

; Evaluates the given expression with current-input-port 
; set to the given server's output port and returns the result.
; Assumes that the server is running.
(define-syntax-rule (server-read s expr ...)
  (parameterize ([current-input-port (server-stdout s)])
    expr ...))

; Evaluates the given expression with current-input-port 
; set to the given server's error port and returns the result.
; Assumes that the server is running.
(define-syntax-rule (server-error s expr ...)
  (parameterize ([current-input-port (server-stderr s)])
    expr ...))
          
; Shuts down the given solver's process, if any.
(define (server-shutdown s)
  (when (server-running? s)
    (subprocess-kill (server-process s) #t)
    (custodian-shutdown-all (server-custodian s))
    (set-server-values! s #f #f #f #f #f #f)))
 