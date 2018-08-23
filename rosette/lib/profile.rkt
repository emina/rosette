#lang racket

(require "profile/tool.rkt" "profile/compile.rkt")

; The symbolic profiler can be run in two ways:
; (1) require this module in your code, wrap the code you wish to profile in
;     (profile-thunk (thunk ...)), and invoke racket with:
;       $ racket -l rosette/lib/profile -t path/to/file.rkt
; (2) invoke
;       $ raco symprofile path/to/file.rkt
;     to profile the entire execution of a file.

(current-compile symbolic-profile-compile-handler)

(provide profile-thunk profile)