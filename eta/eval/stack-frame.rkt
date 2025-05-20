#lang racket


(require "../eval/runtime-values.rkt"
         "../utils/error.rkt"
)


(provide 
    CallFrame
    make-call-frame
    CallFrame-proc
    CallFrame-args
    CallFrame-env
    CallFrame-loc
    CallFrame-parent
    CallFrame-tail?
    
    CallStack
    init-call-stack
    call-stack-push
    call-stack-pop
    call-stack-top
    call-stack-empty?
    call-stack-current-depth

    MAX-STACK-DEPTH
)


;; Maximum depth of the call stack to prevent infinite recursion
(define MAX-STACK-DEPTH 1000)

;; CallFrame
;;    Represents a function call frame on the call stack
;; Arguments:
;;    proc - The procedure/function being called
;;    args - The arguments passed to the procedure
;;    env - The environment in which the procedure is executed
;;    loc - The source location information for debugging
;;    parent - The parent frame (for maintaining lexical scope chain)
;;    tail? - Whether this is a tail call (#t) or not (#f)
(struct CallFrame (proc args env loc parent tail?) #:transparent)

;; make-call-frame
;;    Creates a new call frame for a function call
;; Arguments:
;;    proc - The procedure/function being called
;;    args - The arguments passed to the procedure
;;    env - The environment in which the procedure is executed
;;    loc - The source location information for debugging
;;    parent - The parent frame (for maintaining lexical scope chain)
;;    tail? - Whether this is a tail call (#t) or not (#f)
;; Returns:
;;    A new CallFrame structure
(define (make-call-frame proc args env loc parent tail?)
  (CallFrame proc args env loc parent tail?))

;; CallStack
;;    Represents the runtime call stack of the interpreter
;; Arguments:
;;    frames - List of CallFrame objects, with the most recent at the head
;;    current-depth - Current depth of the call stack (for overflow checking)
(struct CallStack (frames current-depth) #:transparent)

;; init-call-stack
;;    Initializes an empty call stack
;; Returns:
;;    A new empty CallStack structure
(define (init-call-stack)
  (CallStack '() 0))

;; call-stack-push
;;    Pushes a new frame onto the call stack
;; Arguments:
;;    stack - The current call stack
;;    frame - The frame to push onto the stack
;; Returns:
;;    A new call stack with the frame added or RuntimeError if stack depth exceeds MAX-STACK-DEPTH
;; Notes:
;;    - Returns a RuntimeError if the stack depth exceeds MAX-STACK-DEPTH
;;    - For tail calls, the current frame is replaced instead of pushing a new one
(define (call-stack-push stack frame)
    (cond 
      [(>= (CallStack-current-depth stack) MAX-STACK-DEPTH)
       (make-runtime-error "Stack overflow: maximum stack depth exceeded")]
      
      ; For tail calls with a non-empty stack, replace the top frame
      [(and (CallFrame-tail? frame) (not (call-stack-empty? stack)))
       (CallStack (cons frame (cdr (CallStack-frames stack)))
                 (CallStack-current-depth stack))]
      
      ; For normal calls or tail calls with empty stack, push as usual
      [else
       (CallStack (cons frame (CallStack-frames stack))
                  (+ 1 (CallStack-current-depth stack)))]))

;; call-stack-pop
;;    Removes and returns the top frame from the call stack
;; Arguments:
;;    stack - The current call stack
;; Returns:
;;    If stack is not empty: Three values - the popped frame, the new stack, and the new depth
;;    If stack is empty: A RuntimeError
;; Notes:
;;    Returns a RuntimeError if the stack is empty
(define (call-stack-pop stack)
    (if (null? (CallStack-frames stack))
        (make-runtime-error "Stack underflow: no frames to pop")
        (let ([frame (car (CallStack-frames stack))])
            (values frame
                    (CallStack (cdr (CallStack-frames stack))
                    (- (CallStack-current-depth stack) 1))))))

;; call-stack-top
;;    Returns the top frame of the call stack without removing it
;; Arguments:
;;    stack - The current call stack
;; Returns:
;;    The top frame of the stack or RuntimeError if stack is empty
;; Notes:
;;    Returns a RuntimeError if the stack is empty
(define (call-stack-top stack)
    (if (null? (CallStack-frames stack))
        (make-runtime-error "Stack underflow: no frames to pop")
        (car (CallStack-frames stack))))

;; call-stack-empty?
;;    Checks if the call stack is empty
;; Arguments:
;;    stack - The call stack to check
;; Returns:
;;    #t if the stack is empty, #f otherwise
(define (call-stack-empty? stack)
    (null? (CallStack-frames stack)))

;; call-stack-current-depth
;;    Returns the current depth of the call stack
;; Arguments:
;;    stack - The call stack to check
;; Returns:
;;    The current depth of the stack as an integer
(define (call-stack-current-depth stack)
    (CallStack-current-depth stack))


