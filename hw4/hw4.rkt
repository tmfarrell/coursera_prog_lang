
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; for later testing
;(define ones (lambda () (cons 1 ones)))

;(define nats
;  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
;    (lambda () (f 1))))


;; 1.
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2.
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; 4.
(define (stream-for-n-steps stream n)
  (let ([hd (car (stream))] [rest (cdr (stream))])
  (if (= n 1)
      (list hd)
      (cons hd (stream-for-n-steps rest (- n 1))))))

;; 5.
(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([x_ ((lambda () (if (= (remainder x 5) 0) (* -1 x) x)))])
                (cons x_ (lambda () (f (+ x 1))))))])
  (lambda () (f 1))))

;; 6.
(define dan-then-dog
  (letrec ([f (lambda (x)
                (let ([x_ (if (= x 0) "dan.jpg" "dog.jpg")]
                      [next (if (= x 0) 1 0)])
                (cons x_ (lambda () (f next)))))])
    (lambda () (f 0))))

;; 7.
(define (stream-add-zero s)
  (letrec ([f (lambda (s_)
                (let ([hd (car (s_))] [rest (cdr (s_))])
                  (cons (cons 0 hd) (lambda () (f rest)))))])
    (lambda () (f s))))

;; 8.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([x (list-nth-mod xs n)]
                      [y (list-nth-mod ys n)])
                  (cons (cons x y) (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

;; for later testing
;(define this-list
;  (stream-for-n-steps (cycle-lists (stream-for-n-steps nats 10) (stream-for-n-steps nats 5)) 10))

;; 9.
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (pos)
                (if (< pos len)
                    (let ([p (vector-ref vec pos)])
                      (if (and (pair? p) (equal? v (car p))) p (f (+ pos 1))))
                    #f))])
    (f 0)))

;; 10.
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [cache-loc 0]
           [f (lambda (v)
                (let ([p (vector-assoc v cache)])
                  (if p p (let ([res (assoc v xs)])
                            (if (not res) res (begin (vector-set! cache cache-loc res)
                                                     (set! cache-loc (+ cache-loc 1))
                                                     res))))))])
    f))

;; 11.
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec
         ([e e1]
          [f (lambda () (if (< e2 e) (f) #t))])
       (f))]))

; for testing
; (define a 2)
; (while-less 7 do (begin (set! a (+ a 1)) (print "x") a))