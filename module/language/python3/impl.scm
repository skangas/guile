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
  #:use-module (srfi srfi-1)
  #:use-module (system base pmatch)
  #:export (compare fun-match-arguments))

(define (fun-match-arguments id argnames has-stararg rest args inits)
  "`rest' represents all arguments passed to a method call. `args' is
the values passed to the `args' keyword argument. This method returns
the right arguments in the right order for use in a function body."
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
              (error (string-concatenate `("Wrong number of arguments for "
                                           ,(symbol->string id))))
              (append! first (drop inits (- inits+first-len arg-len)) '(())))
          (call-with-values
            (lambda () (split-at first arg-len))
            (lambda (a b) `(,@a ,b)))))))

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
