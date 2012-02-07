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

;; This module translates an AST produced by the python3 ast module into
;; a corresponding datastructure in scheme. It tries to have a similar
;; representation as the AST defined here:
;;   http://docs.python.org/py3k/library/ast.html


;; todo:

;; - rewrite the (ignore (peg "'somestring'")) pattern
;; - some way to express list of things with separators
;; - most of the definitions


(define-module (language python3 parse-ast)
  #:export (read-python3-ast

            ;; Remove these later used for debugging
            <mod> <stmt> <fundef> <import> <arguments>
            )
  #:use-module (ice-9 peg))

(define (read-python3-ast str)
  (match-pattern <mod> str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common parsers

(define-peg-pattern <none> all
  (ignore (peg "'None'")))

(define-peg-pattern <ident> all
  (peg "[a-zA-z][a-zA-Z0-9_]*"))

;; In the python documentation mod can also be "Interactive",
;; "Expression" and "Suite". We probably dont need them so leave them
;; out for now.
(define-peg-pattern <mod> all
  (and ;; (ip "'Module(body=['")
       (ignore (peg "'Module(body=['"))
       (* (and <stmt> (ignore (peg "', '"))))
       (? <stmt>)
       (ignore (peg "'])' !."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statements

(define-peg-pattern <stmt> body
  (or <fundef>
      ;; <classdef>
      <return-void>
      <return>

      ;; <delete>
      ;; <assign>
      ;; <aug-assgn>

      ;; <for>
      ;; <while>
      ;; <if>
      ;; <with>

      ;; <raise>
      ;; <try-except>
      ;; <try-finally>
      ;; <assert>

      <import>
      ;; <import-from>

      ;; <global>
      ;; <non-local>
      ;; <expr>

      <pass>
      ;; <break>
      ;; <continue>

      ;; what is the attributes field at the end of mod?
      ;; <attributes>
      ))

(define-peg-pattern <fundef> all
  (and (ignore (peg "'FunctionDef(name=' [']")) ;; ['] only way to parse ' ?
       <ident>
       (ignore (peg "['] ', args='"))
       <arguments>
       (ignore (peg "', body=['"))
       (* (and <stmt> (peg "', '")))
       (? <stmt>)
       (ignore (peg "'], decorator_list=['"))
       (* (and <expr> (peg "', '")))
       (? <expr>)
       (ignore (peg "'], returns='"))
       (or <expr> <none>)
       (ignore (peg "')'"))))

(define-peg-pattern <return-void> all
  (ignore (peg "'Return(value=None)'")))

(define-peg-pattern <return> all
  (and (ignore (peg "'Return(value='"))
       <expr> ;; void return aka value=None is handled above
       (ignore (peg "')'"))))

(define-peg-pattern <import> all
  (and (ignore (peg "'Import(names=['"))
       ;; (ignore (peg "'Import(names=['"))
       (* (and <alias> (ignore (peg "', '"))))
       (? <alias>)
       (ignore (peg "'])'"))))

(define-peg-pattern <pass> all
  (ignore (peg "'Pass()'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expressions

(define-peg-pattern <expr> all
  (or ;; <bool-op>
      ;; <bin-op>
      ;; <unary-op>
      ;; <lambda>
      ;; <if-exp>
      ;; <dict>
      ;; <set>
      ;; <list-comp>
      ;; <set-comp>
      ;; <dict-comp>
      ;; <generator-exp>
      ;; <yield>
      ;; <compare>
      ;; <call>
      ;; <num>
      ;; <str>
      ;; <bytes>
      ;; <ellipsis>

      ;; ;; here is a comment in the python ast documentation about bools
      ;; ;; and if they are missing in the formal grammar. Maybe need to
      ;; ;; add these later.

      ;; <attribute>
      ;; <subscript>
      ;; <starred>
      ;; <name>
      ;; <list>
      ;; <tuple>

      ;; ;; again what is the attributes field in the ast documentation!?
      ;; <attributes>
      ))

(define-peg-pattern <expr-context> all
  (peg "'Load' / 'Store' / 'Del' / 'AugLoad' / 'AugStore' / 'Param'"))

;; (define-peg-pattern <slice> all)

(define-peg-pattern <bool-op> all
  (peg "'And' / 'Or'"))

(define-peg-pattern <operator> all
  (or (peg "'Add' / 'Sub' / 'Mult' / 'Div' / 'Mod' / 'Pow' / 'LShift'")
      (peg "'RShift' / 'BitOr' / 'BitXor' / 'BitAnd' / 'FloorDiv'")))

;; probably need to change this to parse "UnaryOp(unaryop op, expr operand)"
(define-peg-pattern <unary-op> all
  (peg "'Invert' / 'Not' / 'UAdd' / 'USub'"))

(define-peg-pattern <cmp-op> all
  (peg "'Eq' / 'NotEq' / 'Lt' / 'LtE' / 'Gt' / 'GtE' / 'Is' / 'IsNot' / 'In' / 'NotIn'"))

;; (define-peg-pattern <comprehension> all)

;; (define-peg-pattern <excepthandler> all)

(define-peg-pattern <arguments> all
  (and (ignore (peg "'arguments(args=['"))
       (* (and <arg> (ignore (peg "', '"))))
       (? <arg>)
       (ignore (peg "'], vararg='"))
       ;; parse None before ident since None is a valid ident
       (or <none> <ident>)
       (ignore (peg "', varargannotation='"))
       (or <expr> <none>)
       (ignore (peg "', kwonlyargs=['"))
       (* (and <arg> (peg "', '")))
       (? <arg>)
       (ignore (peg "'], kwarg='"))
       (or <none> <ident>)
       (ignore (peg "', kwargannotation='"))
       (or <expr> <none>)
       (ignore (peg "', defaults=['"))
       (* (and <expr> (peg "', '")))
       (? <expr>)
       (ignore (peg "'], kw_defaults=['"))
       (* (and <expr> (peg "', '")))
       (? <expr>)
       (ignore (peg "'])'"))))

(define-peg-pattern <arg> all
  (and (ignore (peg "'arg(arg=' [']"))
       <ident>
       (ignore (peg "['] ', annotation='"))
       (or <expr> <none>)
       (ignore (peg "')'"))))

;; (define-peg-pattern <keyword> all)

(define-peg-pattern <alias> all
  (and (ignore (peg "'alias(name=' [']"))
       <ident>
       (ignore (peg "['] ', asname='"))
       <ident>
       (ignore (peg "')'"))))
