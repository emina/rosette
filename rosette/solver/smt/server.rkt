#lang racket

(require racket/runtime-path  "../../base/util/log.rkt")

(provide server server-write server-read server-shutdown)

; A server manages the creation, use, and shutdown of external processes.  
; The path field specifies the absolute path to the program to be called 
; in an external process, and the opts field specifies the list of options / arguments 
; with which to call the process. 
; 
; The custodian, process, stdout, stdin, and stderr fields store the custodian, 
; process handle, standard output, standard input, and standard error for the 
; created process.  These fields are populated with non-false values when a 
; server is initialized A server can be initialized and shutdown any number of times.  
; Calling initialize on a server that has already been initialized has no effect.
(struct server 
  (path opts 
   [custodian #:auto #:mutable]
   [process   #:auto #:mutable] 
   [stdout    #:auto #:mutable] 
   [stdin     #:auto #:mutable] 
   [stderr    #:auto #:mutable])
  #:auto-value #f
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "server(path=~a,opts=~a,pid=~a)" 
              (server-path self) (server-opts self) (server-process self)))])

(define (set-server-values! s custodian process stdout stdin stderr)
  (set-server-custodian! s custodian)
  (set-server-process! s process)
  (set-server-stdout! s stdout)
  (set-server-stdin! s stdin)
  (set-server-stderr! s stderr))

; Returns true if the given server has been initialized.
(define (initialized? s) 
  (define p (server-process s))
  (and p (equal? (subprocess-status p) 'running)))

; Initializes the given server if it's not already initialized.
(define (initialize s)
  (unless (initialized? s) 
    (parameterize ([current-custodian (make-custodian)]
                   [current-subprocess-custodian-mode 'kill])
      (define-values (p out in err) 
        (apply subprocess #f #f #f (server-path s) (server-opts s)))
      (thread (thunk (let loop () 
                       (match (read err)
                         [(? eof-object?) (void)]
                         [expr (log-error [s] "~a" expr)])
                       (loop))))
      (set-server-values! s (current-custodian) p out in err))))
   
; Calls the given procedure on the server's input port and returns the result.
(define (server-write s proc)
  (initialize s)
  (begin0 (proc (server-stdin s))
          (flush-output (server-stdin s))))

; Calls the given procedure on the server's output port and returns the result.
(define (server-read s proc)
  (proc (server-stdout s)))
          
; Shuts down the given solver's process, if any.
(define (server-shutdown s)
  (when (initialized? s) 
    (subprocess-kill (server-process s) #t)
    (custodian-shutdown-all (server-custodian s))
    (set-server-values! s #f #f #f #f #f)))
 