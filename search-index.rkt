#lang racket
(require "page-rank.rkt")
(require "generate.rkt")
(require "search.rkt")

(provide master-function)

(define (master-function t)
(define text t)
(define s-i (map (lambda (x) (string->symbol x)) (string-split text)))

(define index the-web-index)  ;;the-index -> Index made by crawling the web
(define web the-web)

(define (sanitize x)
  (cond [(symbol? x) (symbol->string x)]
        [(string? x) x]
        [""]))

(define (ls->string l)
  (define s (sanitize (car l)))
  (define (helper l)
    (if (null? (cdr l)) (string-append s " " (sanitize (car l)))
        (begin
          (set! s (string-append s " " (sanitize (car l))))
          (helper (cdr l)))))
  (helper (cdr l)))

(define (o s)     ;;list of URLs associated with symbol s
  (if (hash-has-key? (cadr index) s)
      (vector->list (hash-ref (cadr index) s))
      '()))

(define (all-o l)
  (define (helper l acc)
    (if (null? l) acc
        (let* ([l-s (o (car l))])
          (helper (cdr l) (append acc l-s)))))
  (remove-duplicates (helper l '())))

(page-rank!)

(define (sort-i)
  (let* ([l-1 (all-o s-i)]
         [l* (map (lambda (x) (cons x (hash-ref p-r x))) l-1)]
         [l-2 (sort l* #:key cdr >)]
         [l (map (lambda (x) (car x)) l-2)])
    l))

(define (out-list)
  (let* ([l (sort-i)]
         [l* (map (lambda (x)
                    (cons (symbol->string x) (ls->string (find-URL-text web x)))) l)])
    l*))

(out-list))