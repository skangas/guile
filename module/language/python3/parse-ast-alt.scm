(use-modules (ice-9 peg))

;; Defining some useful non-terminals for the future
(define-peg-string-patterns
  "LP < '('
   RP < ')'
   LB < '['
   RB < ']'
   NL < '\n'/('\n' '\r')/('\r' '\n')
   WS < ' '/'\t'
   ASSIGN < '='
   LST-SEP < WS* ',' WS*")

;; Define all the builtin types that the python ast has
(define-peg-string-patterns
  "identifier-name <-- ([a-z]/[A-Z]/'_') ([a-z]/[A-Z]/[0-9]/'_')*
   int             <-- [0-9]+")
(define-peg-pattern
  identifier all (and "'" identifier-name "'"))
(define-peg-pattern 
  string all (or (and "\"\"\"" (* (and (not-followed-by "\"\"\"") peg-any)) "\"\"\"")
                 (and "'"  (* (or "\\'"  (and (not-followed-by NL) (not-followed-by "'") peg-any))) "'")
                 (and "\"" (* (or "\\\"" (and (not-followed-by NL) (not-followed-by "\"") peg-any))) "\"")))
;; ------------

(define-peg-string-patterns
  "mod-module-head <- 'Module'
   mod-inter-head  <- 'Interactive'
   mod-expr-head   <- 'Expression'
   mod-suite-head  <- 'Suite'")

(define-peg-pattern 
  <mod> all (or (and mod-module-head (and LP (or <stmt-body>) RP))
                (and mod-inter-head (and LP (or <stmt-body>) RP))
                (and mod-suite-head (and LP (or <stmt-body>) RP))
                (and mod-expr-head (and LP (or (and value-head <expr-value>) RP)))))

(define-peg-pattern <stmt> all (or "FunctionDef"
                                   "ClassDef"
                                   "Return"
                                   "Delete"
                                   "Assign"
                                   "AugAssign"
                                   "For"
                                   "While"
                                   "If"
                                   "With"
                                   "Raise"
                                   "TryExcept"
                                   "TryFinally"
                                   "Assert"
                                   "Import"
                                   "ImportFrom"
                                   "Global"
                                   "Nonlocal"
                                   "Expr"
                                   "Pass"
                                   "Break"
                                   "Continue"))
(define-peg-pattern <stmt-list> body (and LB (and (* (and <stmt> LST-SEP)) (? <stmt>)) RB))
(define-peg-pattern <stmt-body> body (and body-head <stmt-list>))

(define-peg-string-patterns
  "arg-head   < 'arg='
   body-head  < 'body='
   value-head < 'value='
  ")
(define-peg-pattern <keyword-arg> body (and arg-head identifier))
(define-peg-pattern <keyword-value> body (and value-head <expr-value>))
(define-peg-pattern <keyword> body (and "keyword(" <keyword-arg> LST-SEP <keyword-value> ")"))
(define-peg-pattern <keyword-list> body (and LB (or (and <keyword> LST-SEP) <keyword>) RB))
(define-peg-pattern <call-args> body (or (and "func=" <expr-value>)
                                         (and "args=" <expr-list>)
                                         (and "keywords=" <keyword-list>)
                                         (and "starargs=" (or <expr-list> "None"))
                                         (and "kwargs=" (or <expr-list> "None"))))
(define-peg-string-patterns
  "expr-context <-- 'Load' / 'Store' / 'Del' / 'AugLoad' / 'AugStore' / 'Param'
   boolop   <-- 'And' / 'Or'
   operator <-- 'Add' / 'Sub' / 'Mult' / 'Div' / 'Mod' / 'Pow' / 'LShift' / 'RShift' / 'BitOr' / 'BitXor' / 'BitAnd' / 'FloorDiv'
   unaryop  <-- 'Invert' / 'Not' / 'UAdd' / 'USub'
   cmpop    <-- 'Eq' / 'NotEq' / 'Lt' / 'LtE' / 'Gt' / 'GtE' / 'Is' / 'IsNot' / 'In' / 'NotIn'")
(define-peg-pattern <expr-value> body (or "BoolOp"
                                          "BinOp"
                                          "UnaryOp"
                                          "Lambda"
                                          "IfExp"
                                          "Dict"
                                          "Set"
                                          "ListComp"
                                          "SetComp"
                                          "DictComp"
                                          "GeneratorExp"
                                          "Yield"
                                          "Compare"
                                          (and "Call" LP (and (* (and <call-args> LST-SEP)) (or <call-args>)) RP)
                                          (and "Num" LP (ignore "n=") int RP)
                                          "Str"
                                          "Bytes"
                                          "Ellipsis"
                                          "Attribute"
                                          "Subscript"
                                          "Starred"
                                          (and "Name" LP (and "id=" identifier) LST-SEP (and "ctx=" expr-context LP RP) RP)
                                          "List"
                                          "Tuple"))
(define-peg-pattern <expr-list> body (and LB (and (* (and <expr-value> LST-SEP)) (or <expr-value>)) RB))

;; Testa implementationen
(let ((string-test (match-pattern string "'te\\'star'")))
  (if string-test
    (begin (display "string-test ")
           (display (peg:tree string-test))
           (display "\n"))
    (display "Could not parse string\n")))
(let ((call-expr-test (match-pattern <expr-value> "Call(func=Name(id='do_stuff', ctx=Load()), args=[Num(n=1), Num(n=2)], keywords=[keyword(arg='c', value=Num(n=3))], starargs=None, kwargs=None)")))
  (if call-expr-test
    (begin (display "call-expr-test ")
           (display (peg:tree call-expr-test))
           (display "\n"))
    (display "Could not match call-expr-test\n")))
(let ((module-test (match-pattern <mod> "Module(body=[FunctionDef, ClassDef])")))
  (if module-test
    (begin (display "module-test ")
           (display (peg:tree module-test))
           (display "\n"))
    (display "Could not match module definition")))
