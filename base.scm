#!chezscheme

;; PRELUDE
;;
;; Essential syntaxic extensions for Scheme, including support for association
;; list binds, complex pattern matching, and message passing style:
;;
;; Implements macro for named arguments, such as `assoc-let` and `assoc-bind`
;; which allow to take advantage of builtin assoc procedure by binding values.
;; Along with `conses` provide an easy way for defining functions with optional
;; named arguments.
;;
;; Provides `named-tuple` and `dispatcher` as utilities to define in-place
;; message passing data and behavior functions.
;;
;; Implements a full-fledged pattern matching macro with guard expressions.

(library (prelude base)
  (export
    conses
    named-tuple
    assoc-let
    assoc-bind
    assoc-lambda
    dispatcher
    match-case
    match-lambda)

  (import (chezscheme))

  ;; CONSES
  ;;
  ;; Group neighbour elements of a list into cons-pairs, so that the result
  ;; forms a valid association list, suitable for assoc-let and assoc-bind
  ;; macros.

  (define (conses lst)
    (define (carf in out)
      (cond ((null? in) out)
            (else (cdrf (car in) (cdr in) out))))
    (define (cdrf x in out)
      (cond ((null? in) (cons (cons x #f) out))
            (else (carf (cdr in) (cons (cons x (car in)) out)))))
    (reverse! (carf lst '())))

  ;; NAMED-TUPLE
  ;;
  ;; Utility function for defining in-place data structures that use symbolic
  ;; names associated with values. Uses hashtable as a backend structure.
  ;;
  ;; Accepts lists of arguments that will used to create a message passing data
  ;; object with possible messages defined by odd arguments keys and initial
  ;; values bound from even arguments. Values can be altered by providing an
  ;; optional argument to the object.

  (define (named-tuple . args)
    (let* ((backend (make-hashtable symbol-hash eq?)))
      (map (lambda (def)
             (hashtable-set! backend (car def) (cdr def)))
           (conses args))
      (case-lambda
        ((name)
         (let* ((f (gensym))
                (v (hashtable-ref backend name f)))
           (if (eq? f v)
             (assertion-violation 'named-tuple
                                  "name is not defined in tuple"
                                  (hashtable-keys backend)
                                  name)
             v)))
        ((name value)
         (hashtable-set! backend name value)))))

  ;; ASSOC-LET
  ;;
  ;; Follows the same structure as let-values, allows to bind values from keys
  ;; of an assoclist, a default value can be provided to each bind form with
  ;; the help of `or` keyword.

  (define-syntax assoc-let
    (syntax-rules (or)
      ((_ (() assoclist) expr ...)
       (begin expr ...))
      ((_ (((name key or default) binds ...) assoclist) expr ...)
       (let* ((src assoclist)
              (name (cond ((assoc key src) => cdr) (else default))))
         (assoc-let ((binds ...) assoclist) expr ...)))
      ((_ (((name key) binds ...) assoclist) expr ...)
       (let* ((src assoclist)
              (name (cond ((assoc key src) => cdr)
                          (else (assertion-violation 'assoc-let
                                                     "expecting mandatory key"
                                                     key)))))
         (assoc-let ((binds ...) assoclist) expr ...)))))


  ;; ASSOC-BIND
  ;;
  ;; An alternative variant of assoc-let, which binds names to values associated
  ;; with symbols that represent quoted names.

  (define-syntax assoc-bind
    (syntax-rules (quote)
      ((_ (() assoclist) expr ...)
       (begin expr ...))
      ((_ ((((quote name) default) rest ...) assoclist) expr ...)
       (let ((src assoclist))
         (assoc-let (((name (quote name) or default)) src)
           (assoc-bind ((rest ...) src) expr ...))))
      ((_ (((quote name) rest ...) assoclist) expr ...)
       (let ((src assoclist))
         (assoc-let (((name (quote name))) src)
           (assoc-bind ((rest ...) src) expr ...))))))

  ;; ASSOC-LAMBDA
  ;;
  ;; Allows to define in-place lambda function with optional named arguments.
  ;; Expands into lambda with an assoc-bind.

  (define-syntax assoc-lambda
    (syntax-rules (quote)
      ((_ ((formals ...) named ...) expr ...)
       (lambda (formals ... . opts)
         (let ([e (conses opts)])
           (assoc-lambda "named-args" e (named ...) expr ...))))
      ((_ "named-args" src ((quote name) rest ...) expr ...)
       (assoc-bind [((quote name)) src]
         (assoc-lambda "named-args" src (rest ...) expr ...)))
      ((_ "named-args" src (((quote name) default) rest ...) expr ...)
       (assoc-bind [(((quote name) default)) src]
         (assoc-lambda "named-args" src (rest ...) expr ...)))
      ((_ "named-args" src () expr ...)
       (begin expr ...))))

  ;; DISPATCHER
  ;;
  ;; Macro for defining message-passing style behaviours, as opposed to
  ;; `named-tuple` used primary to create in-place data structures, dispatcher
  ;; macro expands to a lambda which accepts specified set of messages.

  (define-syntax dispatcher
    (syntax-rules (else =>)
      ((_ "apply" args => fn)
       (apply fn args))
      ((_ "apply" args lambda-args expr ...)
       (apply (lambda lambda-args expr ...) args))
      ((_ (method def ...) ... (else clause ...))
       (lambda (msg . args)
         (case msg
           (method
             (dispatcher "apply" args def ...)) ...
           (else
             (dispatcher "apply" (cons msg args) clause ...)))))
      ((_ (method def ...) ...)
       (lambda (msg . args)
         (case msg
           (method
             (dispatcher "apply" args def ...)) ...)))))

  ;; MATCH-CASE
  ;;
  ;; Pattern matching macro inspired by racket's match macro and CL's
  ;; destructive-bind. Supports placeholders, lists, vectors, quoted datum,
  ;; strings, numbers, characters, booleans, boxes, and guarded patterns.

  (define-syntax match-case
    (lambda (form)
      (define (atomic-datum? datum)
        (or (boolean? datum)
            (string? datum)
            (number? datum)
            (char? datum)))
      (define (placeholder? syntax)
        (eq? '_ (syntax->datum syntax)))
      (syntax-case form (else quote guard =>)
        ((_ src (else expr ...))
         #'(begin expr ...))
        ((_ src)
         #'(assertion-violationf 'match-case
                                 "no matching clause for ~a"
                                 src))
        ((_ src (guard pat guards ...) rest ...)
         #'(let* ((e src)
                  (cont (lambda () (match-case e rest ...))))
             (match-case e
               (pat (match-case "guard" e cont (guards ...)))
               (else (cont)))))
        ((_ "guard" src esc ())
         #'(esc))
        ((_ "guard" src esc ((else expr ...)))
         #'(begin expr ...))
        ((_ "guard" src esc ((g expr ...) guards ...))
         #'(if g
             (begin expr ...)
             (match-case "guard" src esc (guards ...))))
        ((_ src ((x => pat) expr ...) rest ...)
         #'(let ((e src))
             (match-case e
               (pat (let ((x e))
                      expr ...))
               rest ...)))
        ((_ src ((quote s) expr ...) rest ...)
         #'(let* ((e src)
                  (cont (lambda () (match-case e rest ...))))
             (if (equal? e (quote s))
               (begin expr ...)
               (cont))))
        ((_ src (#(u ...) expr ...) rest ...)
         (let* ((vecpat (syntax->vector #'#(u ...)))
                (veclen (vector-length vecpat)))
           #`(let* ((e src)
                    (cont (lambda () (match-case e rest ...))))
               (if (and (vector? e) (eq? #,veclen (vector-length e)))
                 #,(let rec ((i 0))
                     (if (= i veclen)
                       #'(begin expr ...)
                       #`(match-case (vector-ref e #,i)
                           (#,(vector-ref vecpat i)
                            #,(rec (1+ i)))
                           (else (cont)))))))))
        ((_ src (() expr ...) rest ...)
         #'(let ((e src))
             (if (eq? e '())
               (begin expr ...)
               (match-case e rest ...))))
        ((_ src ((x . xs) expr ...) rest ...)
         #'(let* ((e src)
                  (cont (lambda () (match-case e rest ...))))
             (cond
               ((pair? e)
                (let ((mcar (car e))
                      (mcdr (cdr e)))
                  (match-case mcar
                    (x (match-case mcdr
                         (xs expr ...)
                         (else (cont))))
                    (else (cont)))))
               (else (cont)))))
        ((_ src (x expr ...) rest ...)
         #`(let* ((e src)
                  (cont (lambda () (match-case e rest ...))))
             #,(cond
                 ((placeholder? #'x)
                  #'(begin expr ...))
                 ((identifier? #'x)
                  #'(let ((x e))
                      (begin expr ...)))
                 ((atomic-datum? (syntax->datum #'x))
                  #`(if (equal? e x)
                      (begin expr ...)
                      (cont)))
                 ((box? (syntax->datum #'x))
                  (syntax-case #'x ()
                    (#&un #`(if (box? e)
                              (match-case (unbox e)
                                (un expr ...)
                                (else (cont)))
                              (cont)))))
                 (else (syntax-error #'x "invalid atom data for match-case"))))))))

  ;; MATCH-LAMBDA
  ;;
  ;; Convenient macro for defining functions that makes use of pattern matching
  ;; over its own arguments.

  (define-syntax match-lambda
    (syntax-rules ()
      ((_ (pat expr ...) ...)
       (lambda t
         (match-case t
           (pat expr ...) ...))))))
