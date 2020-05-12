#lang racket

(provide sample)

(define sample
  `(#hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 24)
                             (line . 16)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg
               .
               "select: arity mismatch;\n the expected number of arguments does not match the given number\n  expected: 2\n  given: 1\n  arguments...:\n   {[(&& (< xs@1 xs@0) (< xs@2 xs@0) (< ...)) (xs@1 xs@2 xs@3)] [(&& (! (< xs@1 xs@0)) (! (< xs@2 xs@0)) ...) ()] [(|| (&& (|| (&& (! (< xs@1 xs@0)) (< ...)) ...) ...) ...) ((ite* (⊢ (&& (|| (&& (! (< xs@1 xs@0)) ...) ...) ...) ...) ...) (ite* (⊢ (...")
              (exn_trace
               .
               (#hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . "evaluate-with-asserts")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 304)
                             (source
                              .
                              "/rosette/rosette/base/core/bool.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . "[running body]")
                      (srcloc
                       .
                       #hash((column . #f)
                             (line . #f)
                             (source
                              .
                              "\"/rosette/test/trace/code/all/ex-3.rkt\""))))
                #hash((name . "temp35_0") (srcloc . null))
                #hash((name . "for-loop") (srcloc . null))
                #hash((name . "run-module-instance!") (srcloc . null))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 24)
                            (line . 16)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(select <pivot)")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg . "assert: unexpected empty list")
              (exn_trace
               .
               (#hash((name . "raise-assertion-error")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 299)
                             (source
                              .
                              "/rosette/rosette/base/core/bool.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . "evaluate-with-asserts")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 304)
                             (source
                              .
                              "/rosette/rosette/base/core/bool.rkt"))))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 18)
                            (line . 8)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(assert #f \"unexpected empty list\")")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 24)
                             (line . 16)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg
               .
               "select: arity mismatch;\n the expected number of arguments does not match the given number\n  expected: 2\n  given: 1\n  arguments...:\n   {[(&& (&& (<= xs@0 xs@1) (<= xs@0 xs@2) (<= xs@0 ...)) ...) (xs@2 xs@3)] [(|| (|| (&& (! (<= xs@0 xs@1)) (! (<= xs@0 xs@2)) ...) ...) ...) ()] [(|| (&& (|| (&& (|| (&& (! (<= xs@0 xs@1)) (<= xs@0 ...)) ...) ...) ...) ...) ...) ((ite* (⊢ (&& (|| (&...")
              (exn_trace
               .
               (#hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 24)
                            (line . 16)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(select <pivot)")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg . "assert: unexpected empty list")
              (exn_trace
               .
               (#hash((name . "raise-assertion-error")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 299)
                             (source
                              .
                              "/rosette/rosette/base/core/bool.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 18)
                            (line . 8)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(assert #f \"unexpected empty list\")")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 24)
                             (line . 16)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg
               .
               "select: arity mismatch;\n the expected number of arguments does not match the given number\n  expected: 2\n  given: 1\n  arguments...:\n   {[(&& (&& (&& (<= xs@0 xs@1) (<= xs@0 xs@2) (<= xs@0 xs@3)) (<= (ite* ...) ...) ...) ...) (xs@3)] [(|| (|| (&& (|| (&& (|| (&& (! (<= xs@0 xs@1)) (<= xs@0 xs@2)) (&& (<= xs@0 xs@1) ...)) ...) ...) ...) ...) ...) ()]}")
              (exn_trace
               .
               (#hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 24)
                            (line . 16)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(select <pivot)")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg . "assert: unexpected empty list")
              (exn_trace
               .
               (#hash((name . "raise-assertion-error")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 299)
                             (source
                              .
                              "/rosette/rosette/base/core/bool.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 18)
                            (line . 8)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(assert #f \"unexpected empty list\")")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 24)
                             (line . 16)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg
               .
               "select: arity mismatch;\n the expected number of arguments does not match the given number\n  expected: 2\n  given: 1\n  arguments...:\n   '()")
              (exn_trace
               .
               (#hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 24)
                            (line . 16)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(select <pivot)")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg . "assert: unexpected empty list")
              (exn_trace
               .
               (#hash((name . "raise-assertion-error")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 299)
                             (source
                              .
                              "/rosette/rosette/base/core/bool.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 18)
                            (line . 8)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx . "(assert #f \"unexpected empty list\")")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data
        .
        #hash((call_stack
               .
               (#hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 18)
                             (line . 17)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))
                #hash((name . "select")
                      (srcloc
                       .
                       #hash((column . 28)
                             (line . 21)
                             (source
                              .
                              "/rosette/test/trace/code/all/ex-3.rkt"))))))
              (exn_msg . "assert: both branches infeasible")
              (exn_trace
               .
               (#hash((name . "raise-assertion-error")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 299)
                             (source
                              .
                              "/rosette/rosette/base/core/bool.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 5)
                             (line . 119)
                             (source
                              .
                              "/rosette/rosette/lib/trace/tool.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))
                #hash((name . "call-with-exception-handler")
                      (srcloc
                       .
                       #hash((column . 2)
                             (line . 265)
                             (source
                              .
                              "/Applications/Racket v7.6/collects/racket/private/more-scheme.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . ".../more-scheme.rkt:261:28") (srcloc . null))
                #hash((name . "branch-and-merge")
                      (srcloc
                       .
                       #hash((column . 0)
                             (line . 33)
                             (source
                              .
                              "/rosette/rosette/base/form/control.rkt"))))
                #hash((name . null)
                      (srcloc
                       .
                       #hash((column . 11)
                             (line . 267)
                             (source
                              .
                              "/rosette/rosette/lib/trace/compile.rkt"))))))
              (stx_info
               .
               #hash((srcloc
                      .
                      #hash((column . 10)
                            (line . 14)
                            (source
                             .
                             "/rosette/test/trace/code/all/ex-3.rkt")))
                     (stx
                      .
                      "(cond\n  [(= n len<) pivot]\n  [(< n len<) (select <pivot)]\n  [else (select >=pivot (- n len< 1))])")))
              (timestamp . 1588627028)))
       (type . "trace"))
 #hash((data . #hash((assertion . 0) (solver . 0))) (type . "stats"))))
