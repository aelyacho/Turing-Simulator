#lang racket

(provide mk-led mk-button mk-thread gpio-delay-ms)
(require "raspi-gpio.rkt")
(gpio-setup)

(define pin_base 95)
(define i2cAddr #x20)
(gpio-mcp23017-setup pin_base i2cAddr)

;---------------- led ADT  ---------------------

(define (mk-led PIN)
  (define state 'off)
  (gpio-set-pin-mode PIN 'output)
  (define thread? #f)

  (define (update! sym)
    (cond ((eq? sym 0) (off))
          ((eq? sym 1) (on))
          ((eq? sym 'B) (blank)) 
          ))

  (define (on)
    (blink-thread 'stop)
    (gpio-digital-write PIN 1)
    (set! state 'on))

  (define (off)
    (blink-thread 'stop)
    (gpio-digital-write PIN 0)
    (set! state 'off))

  (define blink-thread 
    (mk-thread (lambda ()
                 (let loop ()
                   (gpio-digital-write PIN 1)
                   (sleep 0.5)
                   (gpio-digital-write PIN 0)
                   (sleep 0.5)
                   (loop)))))
 
  (define (bip)
    (gpio-digital-write PIN 1)
    (gpio-delay-ms 300)
    (gpio-digital-write PIN 0))

  (define (blank)
    (set! state 'blank)
    (blink-thread 'execute))

  (define (toggle)
    (if (or (eq? state 'blank) (eq? state 'off))
        (on)
        (off)))

  (lambda (m)
    (cond ((eq? m 'on) (on))
          ((eq? m 'off) (off))
          ((eq? m 'blank) (blank))
          ((eq? m 'state) state)
          ((eq? m 'bip) (bip))
          ((eq? m 'toggle) (toggle))
          ((eq? m 'update!) update!)
          (else (error "Unknown message -- led ADT" m)))))

;--------------- button adt --------------------------
(define (mk-button PIN)
  (define state 'released)
  (gpio-set-pin-mode PIN 'input)
  (gpio-set-pull-resistor PIN 'down)

  (define (pushed?)
    (= 1 (gpio-digital-read PIN)))

  (define (released?)
    (= 1 (gpio-digital-read PIN)))

  (lambda (m)
    (cond ((eq? m 'state) state)
          ((eq? m 'pushed?) (pushed?))
          ((eq? m 'released?) (released?))
          (else (error "Unknown message -- button ADT "m)))))
  

 
(define (mk-thread chunk)
  (define original chunk)
  (define started? #f)


  (define (execute-thread)
    (set! started? #t)
    (set! chunk (thread chunk)))

  (define (stop-thread)
    (when started?
      (kill-thread chunk)
      (set! chunk original)
      (set! started? #f)))


  (lambda (m)
    (cond ((eq? m 'execute) (execute-thread))
          ((eq? m 'stop) (stop-thread))
          (else (error "unknown message" m)))))




