#lang s-exp rosette

(require "parser.rkt" 
         "partitioner.rkt" 
	 "partition-storage.rkt" 
         "visitor-linker.rkt" 
         "visitor-tempinsert.rkt" 
         "visitor-desugar.rkt"
	 rackunit)

(define testdir "../tests/")

;; Parse HLP from file to AST
(define (parse file)
  ;(define concise-printer (new printer% [out #t]))
  (define my-ast (ast-from-file file))
  (pretty-display "=============== before link ===============")
  (send my-ast pretty-print)
  (define need-temp (send my-ast accept (new linker%)))
  (pretty-display "=============== after link ===============")
  (send my-ast pretty-print)
  (send my-ast accept (new temp-inserter%))
  (pretty-display "=============== after temp-insert ===============")
  (send my-ast pretty-print)
  (send my-ast accept (new desugar%))
  (pretty-display "=============== after desugar ===============")
  (send my-ast pretty-print)
  my-ast)

(define (optimize-file file cores capacity max-msgs)
  (define my-ast (parse file))
  (optimize-comm my-ast #:cores cores #:capacity capacity #:max-msgs max-msgs 
                 #:verbose #t))

;; Check with expected number of messages
(define (test-num-msgs name expected-msgs 
                       #:cores [cores 4] #:capacity [capacity 256] #:max-msgs [max-msgs 8]
                       [file (string-append testdir name ".cll")])
  (check-equal? 
   (result-msgs (optimize-file file cores capacity max-msgs))
   expected-msgs
   name)
  )

(test-num-msgs "for-array3"    0)
