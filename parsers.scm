#!chezscheme

;; PRELUDE PARSERS
;;
;; Parser is a function that accepts a character sequence, and returns a
;; sequence of pairs the first element of which is a parsing result, and the
;; second is a next position in the character sequence.

(library (prelude parsers)
  (export
    <-
    match
    parse
    p/char=
    p/char
    p/end
    p/return
    p/bind
    p/seq
    p/do
    p/or
    p/try
    p/*
    p/+)

  (import (chezscheme)
          (prelude sequences))

  (define-syntax match
    (identifier-syntax
      (syntax-error 'match "misplaced aux syntax")))

  (define-syntax <-
    (identifier-syntax
      (syntax-error '<- "misplaced aux syntax")))

  ;; PARSE
  ;;
  ;; Apply character sequence to the given parser and return the first obtained
  ;; result or #f if a string cannot be parsed.

  (define (parse parser str)
    (s/advance
      (lambda (r _) (car r))
      (lambda () #f)
      (parser (string->list str))))

  ;; CHAR
  ;;
  ;; Match single character from input stream by given predicate.

  (define (p/char fn?)
    (lambda (in)
      (s/advance
        (lambda (c cs)
          (if (fn? c)
            (cons (cons c cs) #f)
            #f))
        (lambda () #f)
        in)))

  ;; CHAR=
  ;;
  ;; Match only the specified character from an input stream.

  (define (p/char= ch)
    (lambda (in)
      (s/advance
        (lambda (c cs)
          (if (char=? c ch)
            (cons (cons c cs) #f)
            #f))
        (lambda () #f)
        in)))

  ;; END
  ;;
  ;; Matches only the end on character sequence.

  (define p/end
    (lambda (in)
      (s/advance
        (lambda (_ cs) #f)
        (lambda () (cons (cons #t #f) #f))
        in)))

  ;; RETURN
  ;;
  ;; Return parser yields a return value and keeps character sequence intact.

  (define (p/return ret)
    (lambda (in)
      (cons (cons ret in) #f)))

  ;; BIND
  ;;
  ;; Conventional monadic bind operator for parsers.

  (define (p/bind parser fn)
    (lambda (in)
      (s/flatmap
        (lambda (branch)
          (let ((return (car branch))
                (unparsed (cdr branch)))
            ((fn return) unparsed)))
        (parser in))))

  ;; SEQ
  ;;
  ;; Sequence operator, which returns only the return value of the last parser
  ;; in the sequence.

  (define (p/seq parser . parsers)
    (define (fapply p branches)
      (s/flatmap
        (lambda (branch)
          (let ((unparsed (cdr branch)))
            (p unparsed)))
        branches))
    (lambda (in)
      (fold-left
        (lambda (branches p)
          (fapply p branches))
        (parser in)
        parsers)))

  ;; DO
  ;;
  ;; Monadic syntax for describing parsers.

  (define-syntax p/do
    (syntax-rules (<- match)
      ((p/do (match name <- parser) body ...)
       (p/bind parser (lambda (name) (p/do body ...))))
      ((p/do (match parser) body ...)
       (p/seq parser (p/do body ...)))
      ((p/do return) return)))

  ;; OR
  ;;
  ;; Branches input by alterating between given parsers.

  (define (p/or . parsers)
    (lambda (in)
      (s/flatmap
        (lambda (parser)
          (parser in))
        parsers)))

  ;; TRY
  ;;
  ;; Makes given parser lookahead by branching current input.

  (define (p/try parser)
    (lambda (in)
      (delay
        (s/join
          (parser in)
          (cons (cons #f in) #f)))))

  ;; *
  ;;
  ;; Repeat given parser zero or more times, returns a list
  ;; of parsed elements. Gready.

  (define (p/* parser)
    (p/do
      (match head <- (p/try parser))
      (cond
        ((eq? head #f)
         (p/return '()))
        (else
          (p/do
            (match tail <- (p/* parser))
            (p/return (cons head tail)))))))

  ;; +
  ;;
  ;; Repeat given parser at least once.

  (define (p/+ parser)
    (p/do
      (match head <- parser)
      (match tail <- (p/* parser))
      (p/return (cons head tail)))))
