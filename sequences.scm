#!chezscheme

;; PRELUDE SEQUENCES
;;
;; Inspired by lazy sequences explained in SICP.
;;
;; Sequences are cons-lists with the last element being a terminal value, which
;; is always ignored, or a procedure that itself produces a sequence.  The tail
;; of sequence can be wrapped with delay to create infinite lazy sequences.
;; Sequence functions are transparent to lists as well.

(library (prelude sequences)
  (export
    s/let
    s/advance
    s/map
    s/map*
    s/join
    s/flatmap
    s/take
    s/drop
    s/take-while
    s/drop-while
    s/eager
    s/fold
    s/consume
    s/list)

  (import (chezscheme))

  ;; LET
  ;;
  ;; Named let for defining sequences.  Body of a macro should return either
  ;; terminal value or a pair.  The sequence itself can be referenced with
  ;; parameters carrying a state.

  (define-syntax s/let
    (syntax-rules ()
      ((s/let self ((name init) ...) body ...)
       ((rec self
             (lambda (name ...)
               (delay
                 body ...))) init ...))))

  ;; ADVANCE
  ;;
  ;; Advance the sequence by calling a procedure, deconstructing pair or
  ;; ignoring the terminal value (terminal value is always ignored).  Can be
  ;; applied to lists.

  (define (s/advance sfn efn seq)
    (cond
      ((procedure? seq)
       (s/advance sfn efn (force seq)))
      ((pair? seq)
       (sfn (car seq) (cdr seq)))
      (else (efn))))

  ;; MAP
  ;;
  ;; Produce {(fn x)} sequence from given sequence {x}.

  (define (s/map fn seq)
    (delay
      (s/advance
        (lambda (x xs)
          (cons (fn x) (s/map fn xs)))
        (lambda () #f)
        seq)))

  ;; MAP*
  ;;
  ;; Variant of map function which zips different sequences using `fn`.

  (define (s/map* fn . seqs)
    (delay
      (call/cc
        (lambda (return)
          (let* ((advanced (apply map list
                                  (map
                                    (lambda (seq)
                                      (s/advance
                                        list
                                        (lambda () (return #f))
                                        seq))
                                    seqs)))
                 (heads (car advanced))
                 (rests (cadr advanced)))
            (cons (apply fn heads) (apply s/map* fn rests)))))))

  ;; JOIN
  ;;
  ;; Replace terminal value of the given sequence with `tail`

  (define (s/join seq tail)
    (delay
      (s/advance
        (lambda (x xs)
          (cons x (s/join xs tail)))
        (lambda ()
          (cond
            ((procedure? tail)
             (force tail))
            (else tail)))
        seq)))

  ;; FLATMAP
  ;;
  ;; Given a funciton `fn` which produces a sequence from a singe element,
  ;; apply each individual element of `seq` to `fn` and join resulting
  ;; sequences.

  (define (s/flatmap fn seq)
    (delay
      (s/advance
        (lambda (x xs)
          (s/join (fn x) (s/flatmap fn xs)))
        (lambda () #f)
        seq)))

  ;; TAKE
  ;;
  ;; Produce a sequence which will only consist of first `n` elements from the
  ;; given sequence.

  (define (s/take n seq)
    (delay
      (if (not (> n 0))
        #f
        (s/advance
          (lambda (x xs)
            (cons x (s/take (1- n) xs)))
          (lambda () #f)
          seq))))

  ;; DROP
  ;;
  ;; Produce a sequence which discards first `n` elements from original
  ;; sequence.

  (define (s/drop n seq)
    (cond
      ((> n 0)
       (s/advance
         (lambda (x xs)
           (s/drop (1- n) xs))
         (lambda () #f)
         seq))
      (else seq)))

  ;; FILTER
  ;;
  ;; Filter sequence items by a given predicate.

  (define (s/filter fn? seq)
    (delay
      (s/advance
        (lambda (x xs)
          (if (fn? x)
            (cons x (s/filter fn? xs))
            (s/filter fn? xs)))
        (lambda () #f)
        seq)))

  ;; TAKE-WHILE
  ;;
  ;; Take only the first elements that satisfy given predicate.  Advance
  ;; through sequence eagerly and lazily return the rest so that composition
  ;; drops.

  (define (s/take-while fn? seq)
    (delay
      (s/advance
        (lambda (x xs)
          (if (fn? x)
            (cons x (s/take-while fn? xs))
            #f))
        (lambda () #f)
        seq)))

  ;; DROP-WHILE
  ;;
  ;; Drop only the first elements that satisfy given predicate.

  (define (s/drop-while fn? seq)
    (delay
      (s/advance
        (lambda (x xs)
          (if (fn? x)
            (s/drop-while fn? xs)
            (cons x xs)))
        (lambda () #f)
        seq)))

  ;; EAGER
  ;;
  ;; Advance to first element of the sequence and then return it unmodified.
  ;; Can be used to force filters to drop reference to the original sequence in
  ;; order for it to be garbage collected.

  (define (s/eager seq)
    (s/advance
      cons
      (lambda () #f)
      seq))

  ;; FOLD
  ;;
  ;; Eagerly fold a given sequence by applying `fn` starting with `init` as
  ;; first element for folding.

  (define (s/fold fn init seq)
    (define (fold val seq)
      (s/advance
        (lambda (x xs)
          (fold (fn val x) xs))
        (lambda () val)
        seq))
    (fold init seq))

  ;; CONSUME
  ;;
  ;; Eagerly apply a given `fn` to each element of the sequence.

  (define (s/consume fn seq)
    (s/advance
      (lambda (x xs)
        (fn x)
        (s/consume fn xs))
      (lambda () #f)
      seq))

  ;; LIST
  ;;
  ;; Get values from a sequence in the form of a list, evaluating all elements
  ;; in the sequence.

  (define (s/list seq)
    (define (traverse seq ptr)
      (s/advance
        (lambda (x xs)
          (let ((mut (list x)))
            (set-cdr! ptr mut)
            (traverse xs mut)))
        (lambda () #f)
        seq))
    (s/advance
      (lambda (x xs)
        (let ((mut (list x)))
          (traverse xs mut)
          mut))
      (lambda () '())
      seq)))
