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

(define-module (language python3 parse)
  #:use-module (language python3 commons)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 lineio)
  #:export (read-python3))

;; Since I couldn't find some way to capture what is written to stdin
;; when running (system "some-command") this method currently dumps the
;; ast produced by the ast parser to a temporary file which is later
;; read into an s-exp.
(define (read-python3 port)
  (system "rm -f /tmp/python-ast.tmp")
  (let ((code (read-python-string port)))
    (if (eof-object? code)
        code
        (let* ((module (resolve-module '(language python3 parse)))
               (command
                (string-concatenate `("echo \"" ,code "\" | "
                                      ,(load-file-dir module)
                                      "/ast-parser.py > /tmp/python-ast.tmp"))))
          (system command)
          (read (open-file "/tmp/python-ast.tmp" "r"))))))
