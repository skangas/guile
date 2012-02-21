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

;; (define-module (language python3 preprocessor)
;;   #:use-module (ice-9 pretty-print)
;;   #:use-module (system base language)
;;   #:export (preprocessor))

(use-modules (ice-9 pretty-print))

(define *python-ex* (string-copy "

if len(sys.argv) == 1:
    print 'Usage: celsius temp1 temp2 ...'
    sys.exit(0)

# Loop over the arguments
for i in sys.argv[1:]:
    try: 
\tfahrenheit=float(string.atoi(i))
    except string.atoi_error:
 \tprint repr(i), \"not a # numeric value\"
    else: #test
        celsius=(fahrenheit-32)*5.0/9.0
        print '%i\\260F = %i\\260C' % (int(fahrenheit), int(celsius+.5))

define triple_quotes():
   \"\"\"
   testing a docstring
   \"\"\"

   print ''' testing another triple quote '''

   print '''
and
now
for
multiline'''

"))

(define *py-ind* "
if foo:
    if bar:
        x = 42
else:
   print foo
")

;;; Python reads program text as Unicode code points
;; TODO Proper Unicode Support

;;; 2.1.4. Encoding declarations
;; TODO


;; String helper functions

(define (string-insert str pos new)
  (string-append (substring str 0 pos)
                 new
                 (substring str pos (string-length str))))

(define (string-add-before str pos char new)
  (let ((cpos (string-index str char pos)))
    (string-insert str cpos new)))

(define (string-delete str from to)
  (string-append (substring str 0 from)
                 (substring str to (string-length str))))

(define (test-string-insert)
  (define test "abcd")
  (and (string=? (string-insert test 2 "XX") "abXXcd")))

(define (test-string-delete)
  (define test "0123456789")
  (and (string=? (string-delete test 4 7) "0123789")))

;; Various tokens
(define <indent>  " ##<INDENT>## ")
(define <dedent>  " ##<DEDENT>## ")
(define <newline> " ##<NEWLINE>## ")

;; Fix comments
(define* (kill-comments str)
  (define (first-unquoted-hash str pos)
    (define (find-unquoted char pos)
      (let* ((m (string-match (string-append "[" char "]") str pos))
             (m-pos (match:start m)))
        (if (eqv? #\\ (string-ref str (- m-pos 1)))
            (find-unquoted char (+ m-pos 1))
            m-pos)))
    (let* ((f (string-match "[\"'#]" str pos))
           (f-pos (and f (match:start f)))
           (f-match (and f (match:substring f))))
      (and f
           (if (equal? f-match "#")
               f-pos
               (first-unquoted-hash str (+ 1 (find-unquoted f-match (+ 1 f-pos))))))))
  (let recurse ((pos 0))
    (let ((hash-pos (first-unquoted-hash str pos)))
      (if (not hash-pos)
          str
          (let ((eol (string-index str #\newline hash-pos)))
            (substring-move! str eol (- (string-length str) 1)
                             str hash-pos)
            (recurse hash-pos))))))

;; Python Language Reference
;; 2.1.8. Indentation
;;
;; "First, tabs are replaced (from left to right) by one to eight spaces such
;; that the total number of characters up to and including the replacement is a
;; multiple of eight (this is intended to be the same rule as used by Unix)."

(define (string-first-non-space str pos)
  (or (string-skip str #\space pos) pos))

;; Count whitespace from pos, replace any tabs encountered s.t. we have multiple
;; of 8, stop when we are at a character that is neither space nor tab.
(define (fix-tabs str)
  (define (fix-tabs-at str pos)
    (let* ((non-space (string-first-non-space str pos))
           (spaces (- non-space pos)))
      (if (eqv? (string-ref str non-space) #\tab)
          (let ((tabstop (- 8 (modulo spaces 8))))
            (string-delete ;; delete tab
             (string-insert str non-space (make-string tabstop  #\space))
             (+ non-space tabstop) (+ non-space tabstop 1)))
          str)))
  (let fixer ((str str) (pos 0))
    (let ((fixed (or (and (>= pos (string-length str)) str)
                     (fix-tabs-at str pos)))
          (eol (string-index str #\newline pos)))
      (if eol
          (fixer fixed (+ 1 eol))
          fixed))))

(define (test-fix-tabs)
  (define input "
if foo:
    if bar:
\tx = 42
 \ty = 24
 \t z = 11
else:
   print foo
")
  (define correct "
if foo:
    if bar:
        x = 42
        y = 24
         z = 11
else:
   print foo
")
  (let ((got (fix-tabs input)))
    (pretty-print got)
    (string=? got correct)))

;; Python Language Reference
;; 2.1.8. Indentation (cont.)
;;
;; The indentation levels of consecutive lines are used to generate INDENT and
;; DEDENT tokens, using a stack, as follows.
;;
;; Before the first line of the file is read, a single zero is pushed on the
;; stack; this will never be popped off again. The numbers pushed on the stack
;; will always be strictly increasing from bottom to top. At the beginning of
;; each logical line, the line’s indentation level is compared to the top of the
;; stack. If it is equal, nothing happens. If it is larger, it is pushed on the
;; stack, and one INDENT token is generated. If it is smaller, it must be one of
;; the numbers occurring on the stack; all numbers on the stack that are larger
;; are popped off, and for each number popped off a DEDENT token is
;; generated. At the end of the file, a DEDENT token is generated for each
;; number remaining on the stack that is larger than zero.

;; (define-syntax (put-token)
;;   (syntax-rules ()
;;     ((_ tok)
;;      (begin (set! str (string-insert str (- pos 1) tok))
;;             (set pos (+ pos (string-length tok)))))))

(define (add-indent-tokens str)
  (define (next-line pos)
    (+ 1 (string-index str #\newline pos)))
  (define (put-token pos tok)
    (set! str (string-insert str (- pos 1) tok))
    (+ pos (string-length tok)))
  (define (indent pos spaces stack)
    (set! pos (put-token pos <indent>))
    (list (next-line pos) (cons spaces stack)))
  (define (dedent pos spaces stack)
    (set! pos (put-token pos <dedent>))
    (if (< spaces (cadr stack))
        (dedent pos spaces (cdr stack))
        (list (next-line pos) (cdr stack))))
  (define (dedent-all pos stack)
    (if (= 0 (car stack))
        str
        (begin
          (set! pos (put-token pos <dedent>))
          (dedent-all pos (cdr stack)))))
  (define blank-line (make-regexp "^ *$" regexp/newline))
  (define (skip-blank-lines pos)
    (let* ((nl (string-index str #\newline pos))
           (this-line (and nl (substring str pos nl)))
           (match (and nl (regexp-exec blank-line this-line))))
      (if match
         (skip-blank-lines (+ 1 (match:end (regexp-exec blank-line str pos))))
         pos)))
  (let recurse ((pos 0)
                (stack '(0)))
    (set! pos (skip-blank-lines pos))
    (let* ((non-space (string-first-non-space str pos))
           (spaces (- non-space pos)))
      (if (>= non-space (string-length str))
          (dedent-all pos stack)
          (apply
           recurse
           (cond ((> spaces (car stack))
                  (indent pos spaces stack))
                 ((< spaces (car stack))
                  (dedent pos spaces stack))
                 (else (list (next-line pos) stack))))))))

;; Newline tokens

;; (define (add-newline-tokens str)
;;   )

;; Triple-quoted strings

(define (convert-triple-quotes str)
  (define nl-re (make-regexp "\n"))
  (define (escape-newlines str)
    (regexp-substitute/global #f nl-re str
                              'pre "\\n" 'post))
  (define (convert-triples ch pos)
    (let* ((re (string-concatenate (list (make-string 3 ch)
                                         "([^\"]*?)"
                                         (make-string 3 ch))))
           (match (string-match re str)))
      (if match
          (regexp-substitute #f match
                             'pre (make-string 1 ch)
                             (escape-newlines (match:substring match 1))
                             (make-string 1 ch) 'post))))
  (for-each (lambda (a b)
              (set! str (convert-triples a b)))
            '(#\' #\") '(0 0))
  str)

;; The actual preprocessor

(define (preprocessor str)
  (add-indent-tokens (fix-tabs (convert-triple-quotes (kill-comments str)))))

;;; TODO Fixa så den slänger saker i slutet...
;;; TODO Ignorera tomma rader
