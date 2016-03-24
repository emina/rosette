#lang racket

(require rosette/lib/roseunit)

(run-all-tests
 "itunes100_2.rkt"
 "itunes100_4.rkt"
 "itunes100_8.rkt"
 "itunes100_16.rkt"
 
 "imdb250_2.rkt"
 "imdb250_4.rkt"
 "imdb250_8.rkt"
 "imdb250_16.rkt"
 
 "alanon_arkansas_2.rkt"
 "alanon_arkansas_4.rkt"
 "alanon_arkansas_8.rkt"
 "alanon_arkansas_16.rkt")
