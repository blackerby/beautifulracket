#lang br/quicklang

;; READER

;; Every reader must export a read-syntax function
(define (read-syntax path port)
  ;; read the source code from port (ignore path)
  (define src-lines (port->lines port))
  ;; pass the data from src-lines to the function format-datums
  ;; datum: the raw repre­sen­ta­tion of the code as it appears in the source
  ;; items from src-lines will be returned as a list
  (define src-datums (format-datums '~a src-lines))
  ; use (display src-datums) to see what this looks like
  ;; use a quasiquote ` to put src-datums into the module datum
  (define module-datum `(module funstackerby-mod "funstackerby.rkt"
                          (handle-args ,@src-datums)))
  ;; return a syntax object describing a module
  ;; #f means no program context associated with the returned syntax object
  (datum->syntax #f module-datum))
(provide read-syntax)

;; EXPANDER

;; define a macro called funstackerby-module-begin
;; HANDLE-ARGS-EXPR is pattern variable: a named match within the syntax pattern
;; the syntax pattern is HANDLE-ARGS-EXPR
(define-macro (funstackerby-module-begin HANDLE-ARGS-EXPR)
  ;; return a syntax object with program context
  #'(#%module-begin
     (display (first HANDLE-ARGS-EXPR)))) ; display the result of evaluating (handle-args ...)
(provide (rename-out [funstackerby-module-begin #%module-begin]))

;; implement the handle-args function
;; handle-args takes one optional argument
;; it's a rest argument, meaning all the arguments passed to it are gathered into a list
(define (handle-args . args)
  (for/fold ([stack-acc empty])
            ([arg (in-list args)] #:unless (void? arg)) ;; ignore void
    (cond
      ;; if arg is a number, we push it onto the stack
      [(number? arg) (cons arg stack-acc)]
      ;; if arg is +, *
      [(or (equal? + arg) (equal? * arg))
       ;; we pop two items off the stack and apply arg to them
       ;; assigning the result to op-result
       (define op-result (arg (first stack-acc) (second stack-acc)))
       (cons op-result (drop stack-acc 2))]
      ;; if arg is -, /
      [(or (equal? - arg) (equal? / arg))
       ;; we pop two items off the stack and apply arg to them in reverse order
       ;; to get the precedence right
       ;; assigning the result to op-result
       (define op-result (arg (second stack-acc) (first stack-acc)))
       (cons op-result (drop stack-acc 2))])))
(provide handle-args)

(provide + * - /)
