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

(use-modules (ice-9 pretty-print)
             (srfi srfi-1))

;;; Python reads program text as Unicode code points
;; TODO Proper Unicode Support

;;; 2.1.4. Encoding declarations
;; TODO

;;; String utility functions

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

(define (string-first-non-space str pos)
  (or (string-skip str #\space pos) pos))

;; Find first occurrence of char in str that is not escaped by a \
(define (find-unescaped str char pos)
  (let* ((found (string-index str char pos)))
    (if (and found (eqv? (string-ref str (- found 1)) #\\))
        (find-unescaped str char (+ found 1))
        found)))

;; Find first occurrence of char in str that is not inside '' or ""
(define (find-unquoted str char pos)
  (let* ((re (string-append "[\"'" (make-string 1 char) "]"))
         (m (string-match re str pos))
         (m-pos (and m (match:start m)))
         (m-ch  (and m (string-ref str m-pos))))
    (and m
         (if (eqv? m-ch char)
             m-pos
             (let ((next-pos (find-unescaped str m-ch (+ 1 m-pos))))
               (and next-pos
                    (find-unquoted str char (+ 1 next-pos))))))))

;; Token strings
(define <indent>  "#<INDENT>")
(define <dedent>  "#<DEDENT>")
(define <newline> "#<NEWLINE>")

;; Remove all comments ;; FIXME: disallow comments after \

;; FIXME Make sure to delete crap at end of string after move
(define* (kill-comments str)
  (define (first-unquoted-hash str pos)
    (find-unquoted str #\# pos))
  (let recurse ((pos 0) (str str))
    (let ((hash-pos (first-unquoted-hash str pos)))
      (if (not hash-pos)
          str
          (let ((eol (string-index str #\newline hash-pos)))
            (recurse hash-pos (string-append (substring str 0 hash-pos)
                                             (substring str eol (string-length str)))))))))

;; Python Language Reference
;; 2.1.8. Indentation
;;
;; "First, tabs are replaced (from left to right) by one to eight spaces such
;; that the total number of characters up to and including the replacement is a
;; multiple of eight (this is intended to be the same rule as used by Unix)."

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

;; Functions to skip blank lines

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

(define (add-indent-tokens str)  
  (define (put-token pos tok)
    (set! str (string-insert str pos tok))
    (+ pos (string-length tok)))

  ;; 2.1.6. Implicit line joining
  ;; Expressions in parentheses, square brackets or curly braces can be
  ;; split over more than one physical line without using backslashes.
  (define start-tokens '("[" "(" "{"))
  (define end-tokens '("]" ")" "{"))
  (define tokens (make-regexp "[][]"))
  (define (handle-token tok tok-stack)
    (let ((t (match:substring tok)))
      ;; We need the same amount of start and end tokens to have a
      ;; logical line.
      (cond ((member t start-tokens)
             (cons t tok-stack))
            ((member t end-tokens)
             (cdr tok-stack))
            (else tok-stack))))
  (define (next-logical-line pos)
    (let rec ((pos pos) (tok-stack '()))
      (let ((nl (string-index str #\newline pos))
            (tok (regexp-exec tokens str pos)))
        (if (and tok (< (match:start tok) nl))
            (rec (+ 1 (match:start tok))
                 (handle-token tok tok-stack))
            (if (> (length tok-stack) 0)
                (rec (+ 1 nl) tok-stack)
                (and nl
                     (put-token nl <newline>)))))))

  ;; 2.1.7 A logical line that contains only spaces, tabs, formfeeds and
  ;; possibly a comment, is ignored (i.e., no NEWLINE token is
  ;; generated).
  (define blank-line (make-regexp "^ *$" regexp/newline))
  (define (skip-blank-lines str pos)
    (let* ((nl (string-index str #\newline pos))
           (this-line (and nl (substring str pos nl)))
           (match (and nl (regexp-exec blank-line this-line))))
      (if match
          (skip-blank-lines str (+ 1 (match:end (regexp-exec blank-line str pos))))
          pos)))

  (define (indent pos spaces stack)
    (set! pos (put-token (- 2 pos) <indent>))
    (list (next-logical-line pos) (cons spaces stack)))

  (define (dedent pos spaces stack)
    (set! pos (put-token pos <dedent>))
    (if (< spaces (cadr stack))
        (dedent pos spaces (cdr stack))
        (list (next-logical-line pos) (cdr stack))))

  (define (dedent-all (- 2 pos) stack)
    (if (= 0 (car stack))
        str
        (begin
          (set! pos (put-token pos <dedent>))
          (dedent-all pos (cdr stack)))))

  (let recurse ((pos 0) (pos-stack '(0)))
    (or (and (not pos)
             (dedent-all pos pos-stack))
        (begin
          (set! pos (skip-blank-lines str pos))
          (let* ((non-space (string-first-non-space str pos))
                 (spaces (- non-space pos)))
            (if (< (string-length str) non-space)
                (dedent-all pos pos-stack)
                (apply
                 recurse
                 (cond ((> spaces (car pos-stack))
                        (indent pos spaces pos-stack))
                       ((< spaces (car pos-stack))
                        (dedent pos spaces pos-stack))
                       (else
                        (list (next-logical-line pos) pos-stack))))))))))

;; 2.1.5. Explicit line joining

;; Two or more physical lines may be joined into logical lines using
;; backslash characters (\), as follows: when a physical line ends in a
;; backslash that is not part of a string literal or comment, it is
;; joined with the following forming a single logical line, deleting the
;; backslash and the following end-of-line character.

(define (handle-line-continuations str)
  (let recurse ((pos 0))
   (let ((lc (find-unquoted str #\\ pos)))
     ;; FIXME: Should throw exception if next character is not a newline
     (if lc
      (string-delete str lc (+ 2 lc))
      str))))

;; Triple-quoted strings

(define (convert-triple-quotes str)
  (define nl-re (make-regexp "\n"))
  (define (escape-newlines str)
    (regexp-substitute/global #f nl-re str
                              'pre "\\n" 'post))
  (define (convert-triples str ch)
    (let* ((re (string-concatenate (list (make-string 3 ch)
                                         "([^\"]*?)"
                                         (make-string 3 ch))))
           (match #f))
      (while (begin (set! match (string-match re str)) match)
        (set! str (regexp-substitute #f match
                                     'pre (make-string 1 ch)
                                     (escape-newlines (match:substring match 1))
                                     (make-string 1 ch) 'post)))

      str))
  (for-each (lambda (ch)
              (set! str (convert-triples str ch)))
            '(#\' #\"))
  str)

(define (preprocessor str)
  (add-indent-tokens
   (fix-tabs
    (kill-comments
     (handle-line-continuations 
      (convert-triple-quotes
       str))))))

;; TODO: Handle: code with ;
;; TODO: Handle: if (true): pass
;; TODO: Handle: if (true): pass ; true

;;; Tests

(define (test-string-insert)
  (define test "abcd")
  (and (string=? (string-insert test 2 "XX") "abXXcd")))

(define (test-string-delete)
  (define test "0123456789")
  (and (string=? (string-delete test 4 7) "0123789")))

(define *py-ind* "
if foo:
    if bar:
        x = 42
else:
   print foo
")

(define *python-ex* (string-copy "

if len(sys.argv) == 1:
    print 'Usage: celsius temp1 temp2 ...'
    sys.exit(0)

# Loop over the arguments
for i in sys.argv[1:]:
    try: 
\tfahrenheit=float(string.atoi(i))
    except string.atoi_error:
 \tprint(repr(i), \"not a # numeric value\")
    else: #test
        celsius=(fahrenheit-32)*5.0/9.0
        print('%i\\260F = %i\\260C' % (int(fahrenheit), \\
 int(celsius+.5)))

month_names = ['Januari', 'Februari', 'Maart',      # These are the
               'April',   'Mei',      'Juni',       # Dutch names
               'Juli',    'Augustus', 'September',  # for the months
               'Oktober', 'November', 'December']   # of the year

def triple_quotes():
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

(define *python-ex2* (string-copy "

month_names = ['Januari', 'Februari', 'Maart',      # These are the
               'April',   'Mei',      'Juni',       # Dutch names
               'Juli',    'Augustus', 'September',  # for the months
               'Oktober', 'November', 'December']   # of the year

def triple_quotes():
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
