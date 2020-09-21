#lang br/quicklang

;; READER

;; Every reader must export a read-syntax function
(define (read-syntax path port)
  ;; read the source code from port (ignore path)
  (define src-lines (port->lines port))
  ;; pass the data from src-lines to the function format-datums
  ;; datum: the raw repre­sen­ta­tion of the code as it appears in the source
  ;; items from src-lines will be returned wrapped as '(handle x)
  ;; where x is an item from src-lines
  (define src-datums (format-datums '(handle ~a) src-lines))
  ;; use a quasiquote ` to put src-datums into the module datum
  (define module-datum `(module stackerby-mod "stackerby.rkt"
                          ,@src-datums))
  ;; return a syntax object describing a module
  ;; #f means no program context associated with the returned syntax object
  (datum->syntax #f module-datum))
(provide read-syntax)

;; EXPANDER

;; define a macro called stackerby-module-begin
;; HANDLE-EXPR is pattern variable: a named match within the syntax pattern
;; the syntax pattern is HANDLE-EXPR ...
(define-macro (stackerby-module-begin HANDLE-EXPR ...)
  ;; return a syntax object with program context
  #'(#%module-begin
     HANDLE-EXPR ...
     ;; print whatever is on top of the stack at the end
     (display (first stack))))
     ;; (display `(,@stack)))) ; a side experiment
(provide (rename-out [stackerby-module-begin #%module-begin]))

;; implement the stack for managing arguments
(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

;; implement the handle function
;; handle takes one optional argument
(define (handle [arg #f])
  ;; calls to handle without an argument will be ignored
  (cond
    ;; if arg is a number, we push it onto the stack
    [(number? arg) (push-stack! arg)]
    ;; if arg is +, *, -, or /
    [(or (equal? + arg) (equal? * arg))
     ;; we pop two items off the stack and apply arg to them
     ;; assigning the result to op-result
     (define op-result (arg (pop-stack!) (pop-stack!)))
     ;; we push op-result onto the stack
     (push-stack! op-result)]
    [(or (equal? - arg) (equal? / arg))
     (define args (reverse (list (pop-stack!) (pop-stack!))))
     (define op-result (arg (first args) (second args)))
     (push-stack! op-result)]))
(provide handle)

(provide + * - /)