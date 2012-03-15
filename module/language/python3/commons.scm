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

(define-module (language python3 commons)
  #:use-module (system base pmatch)
  #:export (debug display-ln pzip read-python-string load-file-dir))

(define (debug str . rest)
  (display str) (display " ")
  (map (lambda (x) (display x) (display ", ")) rest)
  (newline))

(define (display-ln obj)
  (display obj) (newline))

(define (pzip a b)
  "`pzip' is similar to the `zip' function found in (srfi srfi-1) except
that it use `acons' to create a pair of the elements."
  (let lp ((as a) (bs b) (out '()))
    (pmatch as
      (() (reverse! out))
      ((,a . ,rest-as)
       (pmatch bs
         (() (reverse! out))
         ((,b . ,rest-bs)
          (lp rest-as rest-bs (acons a b out))))))))

(define (read-python-string port)
  (let* ((c (read-char port))
         (last (list #\newline))
         (snd-last '())
         (str last))
    (while (not (eof-object? c))
           (set-car! last c)
           (set-cdr! last (list #\newline))
           (set! snd-last last)
           (set! last (cdr last))
           (set! c (read-char port)))
    (set-cdr! snd-last '())
    (list->string str)))

(define (load-file-dir module)
  "The load directory of the supplied module."
  (debug "module =" module (module-filename module))
  (let* ((filename (module-filename module))
         (pos (and filename (string-rindex filename #\/))))
    (if pos
        (substring filename 0 pos)
        filename)))
