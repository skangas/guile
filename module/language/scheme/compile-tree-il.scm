;;; Guile Scheme specification

;; Copyright (C) 2001, 2009 Free Software Foundation, Inc.

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

(define-module (language scheme compile-tree-il)
  #:use-module (language tree-il)
  #:export (compile-tree-il))

;;; environment := #f
;;;                | MODULE

(define (compile-tree-il x e opts)
  (save-module-excursion
   (lambda ()
     (set-current-module e)
     (let* ((x (sc-expand x 'c '(compile load eval)))
            (cenv (current-module)))
       (values x cenv cenv)))))
