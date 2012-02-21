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
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:use-module (srfi srfi-1)
  #:export (compile-tree-il test))

;; (define-syntax-rule (-> (type arg ...))
;;   `(type ,arg ...))

;; (define-syntax-rule (@implv sym)
;;   (-> (@ '(language ecmascript impl) 'sym)))

;; (define-syntax-rule (@impl sym arg ...)
;;   (-> (call (@implv sym) arg ...)))

(define (econs name gensym env)
  ;; (acons name (-> (lexical name gensym)) env))
  (acons name gensym env))

;; for now only look in local env
(define (lookup name env)
  (car (assq-ref env name)))

(define (display-ln obj)
  (display obj) (newline))

(define (compile-tree-il exp env opts)
  (let ((ret (comp exp '())))
    (values
     (parse-tree-il (car ret))
     env
     env)))

(define (comp x e)
  (pmatch x
    ;; module code
    ((<mod> ,stmts)
     (comp-block stmts e))
     ;; `(begin ,@(map (lambda (stmt) (comp stmt e)) stmts)))

    ;; stmt code

    ;; for now only handle the arguments and no default values
    ((<fundef> ,id ,args ,body ,decos ,ret)
     (let* ((argnames (get-args args))
            (gensyms (map (lambda (x) (gensym (symbol->string x))) argnames)))
       (list
        `(define ,id
           (lambda ()
             (lambda-case
              ((,argnames #f #f () () ,gensyms)
               ,(car (comp-block body (add2env e argnames gensyms)))))))
        (econs id id e))))
    ((<return> ,exp)
     (comp exp e))
    ((<expr> ,exp)
     (comp exp e))

    ;; expressions
    ((<num> ,n)
     (list `(const ,n) e))
    ((<name> ,name ,ctx)
     (list `(lexical name ,(lookup name e)) e))))

(define (comp-block stmts env)
  (let lp ((in stmts) (out '()) (e env))
    (pmatch in
      ((,stmt . ,rest)
       (let ((ret (comp stmt env)))
         (lp rest (cons (car ret) out) (cadr ret))))
      ('()
       (list `(begin ,@(reverse! out)) env)))))

(define (add2env env args values)
  (append (zip args values) env))

(define (get-args args)
  (map (lambda (a) (cadr a)) (cadr args)))

(define (test str)
  (let ((code ((@ (language python3 parse) read-python3) (open-input-string str))))
  (display-ln code)
  (compile-tree-il code '() '())))
