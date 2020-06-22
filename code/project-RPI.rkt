
#lang racket


(require racket/format "components.rkt")
(provide simple-inc)


(define (turing name state-init state-term blank rules symbols)
  (define state  state-init)
  (define head   500)
  (define bottom 500)
  (define top    500)
  (define step   0)
  (define tape   (make-vector 1000))
  (define led-tape (make-vector 11))
  (define running #f)

  (displayln "machine: ")
  (displayln name)

  (define (initialize)
    (let create-leds
      ((idx 0)
       (pin-idx 100)
       (end (vector-length led-led-tape)))
      (when (< idx end)
        (vector-set! led-tape idx (mk-led pin-idx))
        (create-leds (+ idx 1) (+ pin-idx 1) end)))
    (reset '(1 1 1)))

  (define (update-leds!) ; led -vector updaten
    (let update
      ((led-idx 0)
       (end (vector-length led-tape))
       (tape-idx (- head 5)))
      (when (< led-idx end)
          (let ((tape-value (vector-ref tape tape-idx))
                (led (vector-ref led-tape led-idx)))
            ((led 'update!) tape-value)
            (update (+ led-idx 1) end (+ tape-idx 1))))))

            
      

  (define (reset symbols) ; reset tape
    (set! state state-init)
    (set! head 500)
    (set! bottom 500)
    (set! top 500)
    (set! step 0)
    (set! tape (make-vector 1000))
    (do ()((null? symbols))
      (vector-set! tape top (car symbols))
      (set! symbols (cdr symbols))
      (set! top (+ top 1)))
    (vector-set! tape top blank)
    (update-leds!))
  
  
  
  (define (show)
    (display " step: ")  (display (~a step  #:align 'right #:width 2  #:pad-string " "))
    (display " state: ") (display (~a state #:align 'left  #:width 5  #:pad-string " "))
    (display " tape: ")
    (do ((pos bottom (+ pos 1))) ((> pos top))
      (display (if (= pos head) "[" " "))
      (display (vector-ref tape pos))
      (display (if (= pos head) "]" " ")))
    (newline))

  (define (left) 
    (set! head (- head 1)) 
    (when (< head bottom)
      (set! bottom head))
    (update-leds!)) 

  (define (right) 
    (set! head (+ head 1))
    (when (> head top)
      (set! top head))
    (update-leds!))
 
  (define (write/erase!)
    (let ((old (vector-ref tape head)))
      (if (or (eq? old blank) (eq? old 0))
          (vector-set! tape head 1)
          (vector-set! tape head 0))
      (update-leds!))) 

  (define (try rules)  
    (cond
      ((not (null? rules))
       (let* ((rule          (car rules)) 
              (state-before  (vector-ref rule 0)) 
              (symbol-before (vector-ref rule 1))) 
         (if (and (eq? state-before state)
                  (eq? symbol-before (vector-ref tape head))) 
             (let ((symbol-after (vector-ref rule 2)) 
                   (action       (vector-ref rule 3)) 
                   (state-after  (vector-ref rule 4))) 
               (vector-set! tape head symbol-after) 
               (case action 
                 ((left)  
                  (set! head (- head 1)) 
                  (cond
                    ((< head bottom)
                     (set! bottom head))))
                 ((right) 
                  (set! head (+ head 1))
                  (cond
                    ((> head top)
                     (set! top head)))))
               (set! state state-after)) 
             (try (cdr rules)))))))
 
 
  (define (next-step)
    (when (not (eq? state state-term))
      (set! step (+ step 1))
      (try rules)
      (update-leds!)))
  
  (define (run)
    (if (eq? state state-term)
        (begin (show) (update-leds!))
        (if running
            (begin
              (next-step)
              (show)
              (sleep 1.5)(run))
            (run))))
        
  
  
  (lambda (m) 
    (cond ((eq? m 'init) (initialize))
          ((eq? m 'run) (set! running #t)(run))
          ((eq? m 'pause) (set! running #f))
          ((eq? m 'step) (next-step))
          ((eq? m 'left) (left))
          ((eq? m 'right) (right))
          ((eq? m 'reset!) (reset '(1 1 1)))
          ((eq? m 'write/erase!) (write/erase!))
          ((eq? m 'show) (show))
          (else (error "unknown message -- turing-machine" m))))


  )



(define simple-inc 
  (turing "Simple incrementer"
          'q0
          'qf
          'B
          '(#(q0 1 1 right q0)
            #(q0 B 1 stay  qf))
          '(1 1 1)))


(define move-left (mk-button 26))
(define write/erase (mk-button 28))
(define move-right (mk-button 98))
(define pause (mk-button 27))
(define step (mk-button 29))
(define run/cont (mk-button 97))
(define buzzer (mk-led 99))
(define (rest) (gpio-delay-ms 500))

(simple-inc 'init)

(thread (lambda () (let listen
                     ()
                       (cond ((move-left 'pushed?) (displayln "left pushed") (buzzer 'bip)(simple-inc 'left) 
                                                   (rest)(listen))
                             ((move-right 'pushed?) (displayln "right pushed") (buzzer 'bip)(simple-inc 'right) 
                                                    (gpio-delay-ms 1000)(listen))
                             ((write/erase 'pushed?) (displayln "write pushed") (buzzer 'bip)(simple-inc 'write/erase!) 
                                                     (rest) (listen))
                             ((pause 'pushed?) (displayln "pause pushed") (buzzer 'bip)(simple-inc 'pause) 
                                               (rest)(listen))
                             ((step 'pushed?)(displayln "step pushed")(buzzer 'bip)(simple-inc 'step)
                                             (rest)(listen))
                             ((run/cont 'pushed?) (displayln "run pushed")(buzzer 'bip)(simple-inc 'run) 
                                                  (rest)(listen))
                             (else (listen))))))



