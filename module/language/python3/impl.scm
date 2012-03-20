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
  (type #:getter py-type #:init-keyword #:p)
  (base #:init-keyword #:b)
  (dict #:getter py-dict #:init-form (make-hash-table 7)))

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

;; >>> object.__class__
;; <class 'type'>
;; >>> dir(object.__class__)
;; ['__abstractmethods__', '__base__', '__bases__', '__basicsize__', '__call__',
;; '__class__', '__delattr__', '__dict__', '__dictoffset__', '__doc__',
;; '__eq__', '__flags__', '__format__', '__ge__', '__getattribute__',
;; '__gt__', '__hash__', '__init__', '__instancecheck__', '__itemsize__',
;; '__le__', '__lt__', '__module__', '__mro__', '__name__',
;; '__ne__', '__new__', '__prepare__', '__reduce__', '__reduce_ex__',
;; '__repr__', '__setattr__', '__sizeof__', '__str__', '__subclasscheck__',
;; '__subclasses__', '__subclasshook__', '__weakrefoffset__', 'mro']

(define py3-class-type (make <py3-object>))

(aset py3-class-type
      '((__doc__ . "type(object) -> the object's type\ntype(name, bases, dict) -> a new type")))

;; >>> object().__class__
;; <class 'object'>
;; >>> dir(object().__class__)
;; ['__class__', '__delattr__', '__doc__', '__eq__', '__format__',
;; '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__',
;; '__le__', '__lt__', '__ne__', '__new__', '__reduce__',
;; '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__',
;; '__subclasshook__']

;; (define class-object
;;   (create-class "object" '() (alist->vhash '((__doc__ . "The most base object")
;;                                              (__str__ . #f)))))




;;;

;object

;typ

; object -> typ relationen

; typklasser

;; ;; Create an object
;; (define (create-object type value id)
;;   `(alist->vhash
;;     ((type . ,type)
;;      (value . ,value)
;;      (id . 0) ;; FIXME: use gensym
;;      ))) 

;; ;; corresponds to type(name, bases, dict)
;; (define (create-class name bases dict)
;;   `(alist->vhash
;;     ((name . ,name)
;;      (bases . ,bases)
;;      (dict . ,dict))))

;;; Standard classes






;; (define class-not-implemented
;;   (create-class "NotImplementedType" '("object") '(__str__ . (const "NotImplemented"))))

;; (define class-none
;;   (create-class "None" '("object") '("object") '(__str__ . (const "None"))))

;; (define class-ellipsis
;;   (create-class "Ellipsis" '("object") '(;;???
;;                                          )))

;; >>> type(foo)
;; <class 'function'>
;; >>> dir(type(foo))
;; ['__annotations__', '__call__', '__class__', '__closure__', '__code__',
;; '__defaults__', '__delattr__', '__dict__', '__doc__', '__eq__',
;; '__format__', '__ge__', '__get__', '__getattribute__', '__globals__',
;; '__gt__', '__hash__', '__init__', '__kwdefaults__', '__le__',
;; '__lt__', '__module__', '__name__', '__ne__', '__new__',
;; '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__',
;; '__str__', '__subclasshook__']
;; 
;; 
