#lang racket

(require "../data.rkt" "../record.rkt" "../graph.rkt" "../reporter.rkt"
         "renderer.rkt"
         "syntax.rkt")

(provide make-trace-renderer)

(struct trace-renderer (source name) 
  #:transparent
  #:methods gen:renderer
  [(define start-renderer void)
   (define (finish-renderer self profile)
     (match-define (trace-renderer source name) self)
     (render-trace (profile-state->graph profile) source name))])

(define (make-trace-renderer source name [options (hash)])
  (trace-renderer source name))

(define (render-trace profile source name)
  (define (indent n)
    (string-join (for/list ([i n]) "  ") ""))
  (printf "Trace for ~a (~v)\n" name source)
  (let rec ([node profile][level 0])
    (define metrics (profile-data-metrics (profile-node-data node)))
    (printf "~a* ~a (~v ms, ~v merges, ~v unions, ~v terms)\n"
            (indent level) (procedure-name (profile-data-procedure (profile-node-data node)))
            (metrics-ref metrics 'time)
            (metrics-ref metrics 'merge-count)
            (metrics-ref metrics 'union-count)
            (metrics-ref metrics 'term-count))
    (for ([c (profile-node-children node)])
      (rec c (add1 level)))))
