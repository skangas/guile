;;; Python 3 for Guile

;; Copyright (C) 2012 Stefan Kangas.
;; Copyright (C) 2012 Per Reimers.
;; Copyright (C) 2012 David Spångberg.
;; Copyright (C) 2012 Krister Svanlund.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language python3 compile-tree-il)
  #:use-module (language python3 impl)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:use-module (srfi srfi-1)
  #:export (compile-tree-il test))

;; Syntax-rules stolen from (language ecmascript compile-tree-il) :)
(define-syntax-rule (-> (type arg ...))
  `(type ,arg ...))

(define-syntax-rule (@implv sym)
  (-> (@ '(language python3 impl) 'sym)))

(define-syntax-rule (@impl sym arg ...)
  (-> (call (@implv sym) arg ...)))

(define (econs name gensym env)
  (cons (list name gensym) env))

;; for now only look in local env
(define (lookup name env)
  "Looks up a given name in a given environment."
  (car (assq-ref env name)))

(define (display-ln obj)
  (display obj) (newline))

(define (debug str . rest)
  (display str) (display " ")
  (map (lambda (x) (display x) (display ", ")) rest)
  (newline))

(define (compile-tree-il exp env opts)
    "Compiles a given python3 expressions in a given environment into a
corresponding tree-il expression."
  (let ((ret (comp exp '())))
    (values
     (parse-tree-il (car ret))
     env
     env)))

(define (comp x e)
  "Compiles a given python3 expressions in a given environment into a
corresponding tree-il expression."
  (pmatch x
    ;; module code
    ((<module> ,stmts)
     (comp-block stmts e))

    ;; stmt code

    ((<function-def> ,id ,args ,body ,decos ,ret)
     (list
      `(define ,id
         (lambda ()
           ,(comp-fun-body id args body e)))
      (econs id id e)))
    ((<return> ,exp)
     (list `(primcall return ,(car (comp exp e))) e))
    ((<assign> ,targets ,value)
     (pmatch targets
       (((<name> ,name <store>))
        (list `(define ,name ,(car (comp value e)))
              (econs name name e)))
       (((<tuple> ,names))
        (list #f #f))))
    ((<expr> ,exp)
     (comp exp e))

    ;; expressions
    ((<num> ,n)
     (list `(const ,n) e))
    ((<name> ,name ,ctx)
     (list `(lexical name ,(lookup name e)) e))
    ((<tuple> ,exps ,ctx)
     (comp-list-or-tuple exps e))
    ((<list> ,exps ,ctx)
     (comp-list-or-tuple exps e))))

(define (comp-list-or-tuple exps env)
  "Compiles a list or tuple expression into a list of values."
  (list (til-list (map (lambda (x) (car (comp x env))) exps)) env))

(define (comp-block stmts env)
  "Compiles a block of statements. Updates the environment in between
every statement."
  (let lp ((in stmts) (out '()) (e env))
    (pmatch in
      ((,stmt . ,rest)
       (let ((ret (comp stmt e)))
         (lp rest (cons (car ret) out) (cadr ret))))
      ('()
       (list `(begin ,@(reverse! out)) env)))))

(define (add2env env args values)
  "Adds a list of symbols to the supplied environment."
  (append (zip args values) env))

;; Handles all types of calls not involving kwargs and keyword
;; arguments.
(define (comp-fun-body id args body env)
  "Compiles a function body."
  (let* ((stararg (cadr args))
         (argnames (if (and (car args) stararg)
                       (append (map car (car args)) (list stararg))
                       (or (map car (car args)) stararg '())))
         (argconsts (map (lambda (x) `(const ,x)) argnames))
         (inits (map (lambda (x) (car (comp x env))) (seventh args)))
         (rest (gensym "rest$"))
         (argsym (gensym "args$"))
         ;; (kwargsym (gensym "kwargs$"))    kwargs not implemented yet
         (gensyms (map (lambda (x) (gensym (string-append
                                            (symbol->string x) "$")))
                       argnames)))
    `(lambda-case
      ((() #f ,rest
        (#f (#:args ,argsym ,argsym)) ;; (#:kwargs ,kwargsym ,kwargsym))
        ((const #f)) (,rest ,argsym))
       (let-values
         (primcall apply (primitive values)
                   ,(@impl fun-match-arguments
                           `(const ,id)
                           `(primcall list ,@argconsts)
                           `(const ,(if stararg #t #f))
                           (lex1 rest)
                           (lex1 argsym)
                           ;; (lex1 kwargsym)
                           `(primcall list ,@inits)))
         (lambda-case
          ((,argnames #f #f () () ,gensyms)
           ,(car (comp-block body (add2env env argnames gensyms))))))))))

;;;; The documentation for let-values in tree-il is incorrect. This is
;;;; an example for how it could be used.
;; (let-values (call (primitive apply)
;;                   (primitive values)
;;                   (call (primitive list) (const 3) (const 4)))
;;   (lambda-case (((a b) #f #f () () (a b)) (lexical b b))))

(define (lex1 sym)
  "A shorthand for lexical references where the symbol is the same as
the gensym."
  `(lexical ,sym ,sym))

(define (til-list a)
  "Makes a list of values into a tree-il list."
  `(primcall list ,@a))


(define (test str)
  (let ((code ((@ (language python3 parse) read-python3) (open-input-string str))))
  (display-ln code)
  (compile-tree-il code '() '())))
