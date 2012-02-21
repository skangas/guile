(use-modules (ice-9 peg) (ice-9 peg codegen))

;; Defining some useful non-terminals for the future
(define-peg-string-patterns
  "LP < '('
   RP < ')'
   LB < '['
   RB < ']'
   NL < '\n'/('\n' '\r')/('\r' '\n')
   WS < ' '/'\t'
   LST-SEP < WS* ',' WS*")
(define-peg-pattern APOS none "'")
(define-peg-pattern QM none "\"")

;; Define all the builtin types that the python ast has
(define-peg-string-patterns
  "identifier-name <- ([a-z]/[A-Z]/'_') ([a-z]/[A-Z]/[0-9]/'_')*
   none-value      <- 'None'
   int             <-- [0-9]+ ")
(define-peg-pattern identifier-key body (and identifier-name (ignore "=")))
(define-peg-pattern <identifier> all (and APOS identifier-name APOS))
(define-peg-pattern <identifier-list> body (and LB (or (and <identifier> LST-SEP) <identifier>) RB))
(define-peg-pattern 
  string all (or (and "\"\"\"" (* (and (not-followed-by "\"\"\"") peg-any)) "\"\"\"")
                 (and "'"  (* (or "\\'"  (and (not-followed-by NL) (not-followed-by "'") peg-any))) "'")
                 (and "\"" (* (or "\\\"" (and (not-followed-by NL) (not-followed-by "\"") peg-any))) "\"")))

;; ------------

(define-peg-pattern <keyword> body (and (ignore "keyword") LP
                                        (and identifier-key <identifier>) LST-SEP
                                        (and identifier-key <expr>)
                                        RP))
(define-peg-pattern <keyword-list> body (and LB (or (and <keyword> LST-SEP) <keyword>) RB))

(define-peg-pattern <comprehension> body (and (ignore "comprehension") LP 
                                              (and identifier-key <expr>) LST-SEP
                                              (and identifier-key <expr>) LST-SEP
                                              (and identifier-key <expr-list>)
                                              RP))
(define-peg-pattern <comprehension-list> body (and LB (or (and <comprehension> LST-SEP) <comprehension>) RB))

(define-peg-pattern <arg> body (and (ignore "arg") LP
                                    (and identifier-key <identifier>) LST-SEP
                                    (and identifier-key (or <expr> none-value))))
(define-peg-pattern <arg-list> body (and LB (or (and <arg> LST-SEP) <arg>) RB))

(define-peg-pattern <alias> body (and (ignore "alias") LP
                                      (and identifier-key <identifier>) LST-SEP
                                      (and identifier-key (or <expr> none-value))))
(define-peg-pattern <alias-list> body (and LB (or (and <alias> LST-SEP) <alias>) RB))

(define-peg-pattern <excepthandler> body (and (ignore "ExceptHandler") LP
                                              (and identifier-key (or <expr> none-value)) LST-SEP
                                              (and identifier-key (or <identifier> none-value)) LST-SEP
                                              (and identifier-key <stmt-list>)
                                              RP))
(define-peg-pattern <excepthandler-list> body (and LB (or (and <excepthandler> LST-SEP) <excepthandler>) RB))

(define-peg-pattern <arguments> body (and (ignore "arguments") LP
                                          (and identifier-key <arg-list>) LST-SEP
                                          (and identifier-key (or <identifier> none-value)) LST-SEP
                                          (and identifier-key (or <expr> none-value)) LST-SEP
                                          (and identifier-key <arg-list>) LST-SEP
                                          (and identifier-key (or <identifier> none-value)) LST-SEP
                                          (and identifier-key (or <expr> none-value)) LST-SEP
                                          (and identifier-key <expr-list>) LST-SEP
                                          (and identifier-key <expr-list>) RP))

(define-peg-pattern slice body (and "Slice" LP
                                    (and identifier-key (or <expr> none-value)) LST-SEP
                                    (and identifier-key (or <expr> none-value)) LST-SEP
                                    (and identifier-key  (or <expr> none-value)) RP))
(define-peg-pattern extslice body (and "ExtSlice" LP (and identifier-key <slice-list>) RP))
(define-peg-pattern indexslice body (and "Index" LP (and identifier-key <expr>) RP))
(define-peg-pattern <slice> body (or slice extslice indexslice))
(define-peg-pattern <slice-list> all (and LB (and (* (and <slice> LST-SEP)) (or <slice>)) RB))

(define-peg-string-patterns
  "expr-context-names <- 'Load' / 'Store' / 'Del' / 'AugLoad' / 'AugStore' / 'Param'
   operator-names     <- 'Add' / 'Sub' / 'Mult' / 'Div' / 'Mod' / 'Pow' / 'LShift' / 'RShift' / 'BitOr' / 'BitXor' / 'BitAnd' / 'FloorDiv'
   unaryop-names      <- 'Invert' / 'Not' / 'UAdd' / 'USub'
   boolop-names       <- 'And' / 'Or'
   cmpop-names        <- 'Eq' / 'NotEq' / 'Lt' / 'LtE' / 'Gt' / 'GtE' / 'Is' / 'IsNot' / 'In' / 'NotIn'")
(define-peg-pattern <unaryop> all (and unaryop-names LP RP))
(define-peg-pattern <operator> all (and operator-names LP RP))
(define-peg-pattern <boolop> all (and boolop-names LP RP))
(define-peg-pattern <expr-context> all (and expr-context-names LP RP))

(define-peg-pattern <cmpop> all (and cmpop-names)) 
(define-peg-pattern <cmpop-list> all (and LB (and (* (and <cmpop> LST-SEP)) (or <cmpop>)) RB))

(define-peg-string-patterns
  "mod-module-head <- 'Module'
   mod-inter-head  <- 'Interactive'
   mod-expr-head   <- 'Expression'
   mod-suite-head  <- 'Suite'")

(define-peg-pattern 
  <mod> all (or (and mod-module-head (and LP (or <stmt-body>) RP))
                (and mod-inter-head (and LP (or <stmt-body>) RP))
                (and mod-suite-head (and LP (or <stmt-body>) RP))
                (and mod-expr-head (and LP (or (and identifier-key <expr>) RP)))))
(define-peg-pattern <stmt-body> body (and identifier-key <stmt-list>))

(define-peg-pattern <stmt> all (or (and "FunctionDef" LP
                                        (and identifier-key <identifier>) LST-SEP
                                        (and identifier-key <arguments>) LST-SEP
                                        (and identifier-key <stmt-list>) LST-SEP
                                        (and identifier-key <expr-list>) LST-SEP
                                        (and identifier-key (or <expr> none-value)) RP) 
                                   (and "ClassDef" LP
                                        (and identifier-key <identifier>) LST-SEP
                                        (and identifier-key <expr-list>) LST-SEP
                                        (and identifier-key <keyword-list>) LST-SEP
                                        (and identifier-key (or <expr> none-value)) LST-SEP
                                        (and identifier-key (or <expr> none-value)) LST-SEP
                                        (and identifier-key <stmt-list>) LST-SEP
                                        (and identifier-key <expr-list>)
                                        RP)
                                   (and "Return" LP
                                        (and identifier-key (or <expr> none-value))
                                        RP)
                                   (and "Delete" LP
                                        (and identifier-key <expr-list>)
                                        RP)
                                   (and "Assign" LP
                                        (and identifier-key <expr-list>) LST-SEP
                                        (and identifier-key <expr>)
                                        RP)
                                   (and "AugAssign" LP
                                        (and identifier-key <expr>) LST-SEP
                                        (and identifier-key <operator>) LST-SEP
                                        (and identifier-key <expr>)
                                        RP)
                                   (and "For" LP
                                        (and identifier-key <expr>) LST-SEP
                                        (and identifier-key <expr>) LST-SEP
                                        (and identifier-key (or <identifier> none-value)) LST-SEP
                                        (and identifier-key (or <identifier> none-value)) LST-SEP
                                        (and identifier-key <stmt-list>) LST-SEP
                                        (and identifier-key <stmt-list>)
                                        RP)
                                   (and "While" LP
                                        (and identifier-key <expr>) LST-SEP
                                        (and identifier-key <stmt-list>) LST-SEP
                                        (and identifier-key <stmt-list>)
                                        RP)
                                   (and "If" LP
                                        (and identifier-key <expr>) LST-SEP
                                        (and identifier-key <stmt-list>) LST-SEP
                                        (and identifier-key <stmt-list>)
                                        RP)
                                   (and "With" LP
                                        (and identifier-key <expr>) LST-SEP
                                        (and identifier-key (or <expr> none-value)) LST-SEP
                                        (and identifier-key <stmt-list>)
                                        RP)
                                   (and "Raise" LP
                                        (and identifier-key (or <expr> none-value)) LST-SEP
                                        (and identifier-key (or <expr> none-value))
                                        RP)
                                   (and "TryExcept" LP
                                        (and identifier-key <stmt-list>) LST-SEP
                                        (and identifier-key <excepthandler-list>) LST-SEP
                                        (and identifier-key <stmt-list>)
                                        RP)
                                   (and "TryFinally" LP
                                        (and identifier-key <stmt-list>) LST-SEP
                                        (and identifier-key <stmt-list>)
                                        RP)
                                   (and "Assert" LP
                                        (and identifier-key <expr>) LST-SEP
                                        (and identifier-key (or <expr> none-value))
                                        RP)
                                   (and "Import" LP
                                        (and identifier-key <alias-list>)
                                        RP)
                                   (and "ImportFrom" LP
                                        (and identifier-key (or <identifier> none-value)) LST-SEP
                                        (and identifier-key <alias-list>) LST-SEP
                                        (and identifier-key int)
                                        RP)
                                   (and "Global" LP
                                        (and identifier-key <identifier-list>)
                                        RP)
                                   (and "Nonlocal" LP
                                        (and identifier-key <identifier-list>)
                                        RP)
                                   (and "Expr" LP
                                        (and identifier-key <expr>)
                                        RP)
                                   (and "Pass" LP
                                        RP)
                                   (and "Break" LP
                                        RP)
                                   (and "Continue" LP
                                        RP)
                                   (and "BoolOp" LP
                                        RP)))
(define-peg-pattern <stmt-list> body (and LB (and (* (and <stmt> LST-SEP)) (? <stmt>)) RB))

;(define (expand-expr-params params)
;  (if (null? params)
;    (if #f #f)
;    (begin
;      (let ((pc (car params)) (rest (cdr params)))
;        (if (null? rest)
;          (and (ignore (car pc)) (cadr pc))
;          (and (ignore (car pc)) (cadr pc) LST-SEP (expand-expr-params rest)))))))
; (define-syntax make-expr-peg
;  (syntax-rules ()
;    ((_ name params)
;     (and name LP (expand-expr-params params) RP))))
(define-peg-pattern <expr> all 
                    (or (and "BoolOp" LP 
                             (and identifier-key <boolop>) LST-SEP 
                             (and identifier-key <expr-list>)
                             RP)
                        (and "BinOp" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <operator>) LST-SEP
                             (and identifier-key <expr>)
                             RP)
                        (and "UnaryOp" LP
                             (and identifier-key <unaryop>) LST-SEP
                             (and identifier-key <expr>)
                             RP)
                        (and "Lambda" LP
                             (and identifier-key <arguments>) LST-SEP
                             (and identifier-key <expr>)
                             RP)
                        (and "IfExp" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <expr>) LST-SEP
                             RP)
                        (and "Dict" LP
                             (and identifier-key <expr-list>) LST-SEP
                             (and identifier-key <expr-list>)
                             RP)
                        (and "Set" LP
                             (and identifier-key <expr-list>)
                             RP)
                        (and "ListComp" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <comprehension-list>)
                             RP)
                        (and "SetComp" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <comprehension-list>)
                             RP)
                        (and "DictComp" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <comprehension-list>)
                             RP)
                        (and "GeneratorExp" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <comprehension-list>)
                             RP)
                        (and "Yield" LP
                             (and identifier-key (or <expr> none-value))
                             RP)
                        (and "Compare" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <cmpop-list>) LST-SEP
                             (and identifier-key <expr-list>)
                             RP)
                        (and "Call" LP 
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <expr-list>) LST-SEP
                             (and identifier-key <keyword-list>) LST-SEP
                             (and identifier-key (or <expr-list> none-value)) LST-SEP
                             (and identifier-key (or <expr-list> none-value))
                             ;;<call-args-list> 
                             RP)
                        (and "Num" LP 
                             (and identifier-key int) 
                             RP)
                        (and "Str" LP
                             (and identifier-key string)
                             RP)
                        (and "Bytes" LP
                             (and identifier-key string)
                             RP)
                        (and "Ellipsis" LP
                             RP)
                        (and "Attribute" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <identifier>) LST-SEP
                             (and identifier-key <expr-context>)
                             RP)
                        (and "Subscript" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <slice>) LST-SEP
                             (and identifier-key <expr-context>)
                             RP)
                        (and "Starred" LP
                             (and identifier-key <expr>) LST-SEP
                             (and identifier-key <expr-context>)
                             RP)
                        (and "Name" LP 
                             (and identifier-key <identifier>) LST-SEP
                             (and identifier-key <expr-context>) 
                             RP)
                        (and "List" LP
                             (and identifier-key <expr-list>) LST-SEP
                             (and identifier-key <expr-context>)
                             RP)
                        (and "Tuple" LP
                             (and identifier-key <expr-list>) LST-SEP
                             (and identifier-key <expr-context>)
                             RP)
                        ))
(define-peg-pattern <expr-list> all (and LB (and (* (and <expr> LST-SEP)) (or <expr>)) RB))

;; Testa implementationen

(define-syntax test
  (syntax-rules ()
    ((_ pattern str) 
     (let ((test-result (match-pattern pattern str))) 
       (if test-result
         (begin (format #t "Try to parse: ~A~%  ~A~%~%" str (peg:tree test-result)))
         (format #t "Could not parse ~A~%" str))))))
(test string "'te\\'star'")
(test <expr> "Call(func=Name(id='do_stuff', ctx=Load()),  args=[Num(n=1), Num(n=2)], keywords=[keyword(arg='c', value=Num(n=3))], starargs=None, kwargs=None)")
(test <mod>  "Module(body=[FunctionDef, ClassDef])")
