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

(define-module (language python3 impl)
  #:use-module (language python3 commons)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (system base pmatch)
  #:export (compare fun-match-arguments assign-match-arguments))

(define (assign-match-arguments targets vals)
  (lambda () (values vals)))

(define (fun-match-arguments id argnames has-stararg rest args inits)
  "`rest' represents all arguments passed to a method call. `args' is
the values passed to the `args' keyword argument. This method returns
the right arguments in the right order for use in a function body."
  (define (err-len)
    (error (string-concatenate `("Wrong number of arguments for "
                                 ,(symbol->string id)))))
  (let ((arg-len (if has-stararg
                     (1- (length argnames))
                     (length argnames))))
    (if args
        (set! rest (take-while (lambda (x) (not (equal? x #:args))) rest))
        (set! args '()))
    (let* ((first (append rest args))
           (first-len (length first))
           (inits+first-len (+ (length inits) first-len)))
      (if (< first-len arg-len)
          (if (< inits+first-len arg-len)
              (err-len)
              (append! first (drop inits (- inits+first-len arg-len)) '(())))
          (if (and (not has-stararg) (> first-len arg-len))
              (err-len)
              (call-with-values
                  (lambda () (split-at first arg-len))
                (lambda (a b) `(,@a ,b))))))))

(define (compare os vs)
  (let lp ((ops os) (vals vs))
    (pmatch ops
      ((,op . '())
       (apply op vals))
      ((,op . ,rest)
       (if (op (car vals) (cadr vals))
           (lp (cdr ops) (cdr vals))
           #f))
      ((,a) (debug "not matched" ops)))))

;;; Python object implementation

(define *undefined* ((@@ (oop goops) make-unbound)))

(define-class <py3-object> ()
  (id #:getter py-id #:init-form (gensym "pyclass$"))
  (type #:getter py-type #:init-keyword #:t)
  (dict #:getter py-dict #:init-keyword #:d))

(define-method (aget (o <py3-object>) (p <string>))
  (aget o (string->symbol p)))

(define-method (aget (o <py3-object>) p)
  (let ((h (hashq-get-handle (py-dict o) p)))
    (if h
        (cdr h)
        *undefined* ;; FIXME: traverse base classes
        )))

(define-method (aset (o <py3-object>) (p <string>) v)
  (aset o (string->symbol p) v))

(define-method (aset (o <py3-object>) p v)
  (hashq-set! (py-dict o) p v))

(define-method (aset (o <py3-object>) (ps <list>))
  (map (lambda (pv)
         (let ((p (car pv)) (v (cdr pv)))
           (aset o p v)))
       ps))

;; NB: We ignore __base__ for now, see:
;; http://code.activestate.com/lists/python-list/334282/

;;; Base classes

(define-syntax-rule (make-attrs as)
  (let ((h (make-hash-table 7)))
    (map (lambda (pv)
           (let ((p (car pv)) (v (cdr pv)))
             (hashq-set! h p v)))
         as)
    h))

(define class-type
  (make <py3-object>
    #:d (make-attrs '((__bases__ . (object))
                      (__doc__ . "type(object) -> the object's type\ntype(name, bases, dict) -> a new type")
                      (__name__ . "type")))))

(define object-type
  (make <py3-object>
    #:d (make-attrs '((__bases__ . ())
                      (__doc__ . "The most base object")
                      (__name__ . "object")))))

;;; Type classes

(define not-implemented-type
  #:d (make <py3-object>
        (make-attrs '((__bases__ . (object))
                      (__name__ . "NotImplementedType")))))

(define none-type
  #:d (make <py3-object>
        (make-attrs '((__bases__ . (object))
                      (__name__ . "NoneType")))))

(define ellipsis-type
  #:d (make <py3-object>
        (make-attrs '((__bases__ . (object))
                      (__name__ . "Ellipsis")))))

;; >>> object.__class__
;; <class 'type'>
;; >>> dir(object.__class__)
;; '__abstractmethods__'  -> Unkown, not necessary to implement
;; '__base__'             ->
;; '__bases__'            ->
;; '__basicsize__'        ->
;; '__call__'             -> Is called if the object is called as a method
;; '__class__'            -> Pointer to the class object
;; '__delattr__'          -> Called when deleting a attribute using `delete foo.bar`
;; '__dict__'             -> A dict containing all the attributes of the object
;; '__dictoffset__'       -> 
;; '__doc__'              ->
;; '__eq__'               ->
;; '__flags__'            ->
;; '__format__'           ->
;; '__ge__'               ->
;; '__getattribute__'     ->
;; '__gt__'               ->
;; '__hash__'             ->
;; '__init__'             ->
;; '__instancecheck__'    ->
;; '__itemsize__'         ->
;; '__le__'               ->
;; '__lt__'               ->
;; '__module__'           ->
;; '__mro__'              ->
;; '__name__'             ->
;; '__ne__'               ->
;; '__new__'              ->
;; '__prepare__'          ->
;; '__reduce__'           ->
;; '__reduce_ex__'        ->
;; '__repr__'             ->
;; '__setattr__'          ->
;; '__sizeof__'           ->
;; '__str__'              ->
;; '__subclasscheck__'    ->
;; '__subclasses__'       ->
;; '__subclasshook__'     ->
;; '__weakrefoffset__'    ->
;; 'mro']                 ->

;; >>> object().__class__
;; <class 'object'>
;; >>> dir(object().__class__)
;; ['__class__'
;; '__delattr__'
;; '__doc__'
;; '__eq__'
;; '__format__'
;; '__ge__'
;; '__getattribute__'
;; '__gt__'
;; '__hash__'
;; '__init__'
;; '__le__'
;; '__lt__'
;; '__ne__'
;; '__new__'
;; '__reduce__'
;; '__reduce_ex__'
;; '__repr__'
;; '__setattr__'
;; '__sizeof__'
;; '__str__'
;; '__subclasshook__']
