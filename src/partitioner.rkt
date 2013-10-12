#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "parser.rkt" 
         "partition-storage.rkt"
         "visitor-interpreter.rkt" 
         "visitor-collector.rkt" 
         "visitor-rename.rkt"
         "visitor-placetype.rkt"
         "visitor-printer.rkt"
         "visitor-evaluator.rkt"
         "visitor-loopbound.rkt"
         "visitor-unroll.rkt"
         )

(require rosette/solver/kodkod/kodkod)
(require rosette/solver/z3/z3)

(provide optimize-comm (struct-out result))

;; Set bidwidth for rosette
;(configure [bitwidth bitwidth])

;; struct used to return result from optimize-comm
(struct result (msgs cores ast))

(define (optimize-comm my-ast #:name [name "temp"]
                        #:cores [num-core 144] 
                        #:capacity [capacity 256] 
                        #:max-msgs [max-msgs #f]
                        #:verbose [verbose #f])
  ;(current-solver (new z3%))
  ;(current-solver (new kodkod%))
  (current-solver (new kodkod-incremental%))
  (configure [bitwidth 32])
  
  ;; Define printer
  (define concise-printer (new printer% [out #t]))

  ;; Place type linker
  (send my-ast accept (new placetype-linker%))
  (when verbose
    (pretty-display "=== After placetype linker  ===")
    (send my-ast pretty-print)
    )

  ;; Unroll
  (send my-ast accept (new loopbound-computer%))
  (when verbose
    (pretty-display "=== After bound compute  ===")
    (send my-ast pretty-print)
    )
  (send my-ast accept (new loop-unroller%))
  (when verbose
    (pretty-display "=== After unroll  ===")
    (send my-ast pretty-print)
    )
  
  ;; Count number of messages
  (define cores (make-cores #:capacity capacity #:max-cores num-core))
  (define interpreter (new count-msg-interpreter% [places cores]))
  
  ;; Place holder for solution
  (define num-msg #f)
  (define partial-hash (make-hash))
  
  (define (solve-function func-ast)
    (define start (current-seconds))
    (set! num-msg (comminfo-msgs (send func-ast accept interpreter)))
    
    (when verbose
      (pretty-display "\n=== After interpreter ===")
      (send my-ast pretty-print))
      ;(send my-ast accept concise-printer))

    (when verbose (pretty-display `(num-msg , num-msg)))
    
    (define num-cores (cores-count cores)) 
    (define lowerbound 0)
    (define upperbound max-msgs)
    (define middle (if upperbound 
                       (floor (/ (+ lowerbound upperbound) 2))
                       #f))
    (define best-sol #f)
    
    (define (update-global-sol)
      ;; Use this when solve the entire program at once.
      (set-global-sol best-sol)
      (cores-evaluate cores)
      (define stop (current-seconds))
      
      
      (when verbose
        (pretty-display "\n=== Update Global Solution ===")
        ;;(send func-ast accept concise-printer) 
        (pretty-display global-sol)
        ;;(display-cores cores)
        (pretty-display (format "synthesis time = ~a sec" (- stop start)))
        )
      )
    
    (define t 0)

    (define (inner-loop)
      (set! t (current-seconds))
      (assert (< num-msg (evaluate num-msg)))
      (solve+ #t)

      (pretty-display `(solve-time ,(- (current-seconds) t)))
      (set! best-sol (current-solution))
      
      ;; display
      (pretty-display (format "# messages = ~a" (evaluate num-msg)))
      (pretty-display (format "# cores = ~a" (evaluate num-cores)))
      
      (inner-loop))

    (define (outter-loop)
      (with-handlers* 
       ([exn:fail? (lambda (e) 
                     (pretty-display `(solve-time ,(- (current-seconds) t)))
                     (if (or (equal? (exn-message e)
                                     "solve: no satisfying execution found")
                             (equal? (exn-message e)
                                     "assert: failed"))
                         (begin
                           (update-global-sol)
                           (clear-asserts)
                           )
                         (begin
                           (pretty-display e)
                           (raise e))))])
       (solve+ #t)
       (pretty-display ">> FIRST SOLVE")
       (set! best-sol (current-solution))
       (inner-loop)))
    
    (outter-loop)
    )
    
  (for ([decl (get-field stmts my-ast)])
    (if 
     ;(is-a? decl FuncDecl%) ;; Use this for solving function by function
     (and (is-a? decl FuncDecl%) (equal? (get-field name decl) "main"))
        (begin
          (solve-function decl)
          (when verbose (pretty-display "------------------------------------------------")))
        (send decl accept interpreter)))

  (with-output-to-file #:exists 'truncate (format "~a/~a.part" outdir name)
    (lambda () (send my-ast accept concise-printer)))
  
  ;; (pretty-display "\n=== Final Solution ===")
  ;; (send my-ast accept concise-printer)
  ;; (display-cores cores)
  
  (let ([evaluator (new symbolic-evaluator% [num-cores num-core])])
    (send my-ast accept evaluator)
    )
  
  ;; (when verbose
  ;;   (pretty-display "\n=== After evaluate ===")
  ;;   (send my-ast accept concise-printer))
  
  (result (evaluate-with-sol num-msg) 
          cores 
          my-ast)
)

(require "visitor-linker.rkt" "visitor-tempinsert.rkt" "visitor-desugar.rkt")
 
(define (parse file)
  (define concise-printer (new printer% [out #t]))
  (define my-ast (ast-from-file file))
  (pretty-display "--------------- ast ------------------")
  (send my-ast pretty-print)
  
  (define need-temp (send my-ast accept (new linker%)))
  (pretty-display "--------------- linker ------------------")
  (send my-ast pretty-print)
  
  (send my-ast accept (new temp-inserter%))
  (pretty-display "--------------- temp ------------------")
  (send my-ast pretty-print)
  
  (send my-ast accept (new desugar%))
  (pretty-display "--------------- desugar ------------------")
  (send my-ast pretty-print)
  
  my-ast)

#|
(define t (current-seconds))
(define my-ast (parse "../tests/add-pair.cll"))
(result-msgs (optimize-comm my-ast #:cores 16 #:capacity 300 #:verbose #t))
(pretty-display (format "partitioning time = ~a" (- (current-seconds) t)))
|#
