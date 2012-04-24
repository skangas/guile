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

(define-module (language python3 compile-tree-il)
  #:use-module (language python3 commons)
  #:use-module (language python3 impl)
  #:use-module (language tree-il)
  #:use-module (oop goops)
  #:use-module (system base pmatch)
  #:use-module (srfi srfi-1)
  #:export (compile-tree-il test))

;; Syntax-rules stolen from (language ecmascript compile-tree-il) :)
(define-syntax-rule (-> (type arg ...))
  `(type ,arg ...))

(define-syntax-rule (@implv sym)
  (-> (@ '(language python3 impl) 'sym)))

(define-syntax-rule (@impl sym arg ...)
  (-> (call (@implv sym) arg ...)))

(define (econs name gensym env)
  (acons name gensym env))

(define (lookup name env)
  "Looks up a given name in a given environment."
  (assq-ref env name))

(define (add2env env args values)
  "Adds a list of symbols to the supplied environment."
  (append (pzip args values) env))

(define (lex1 sym)
  "A shorthand for lexical references where the symbol is the same as
the gensym."
  `(lexical ,sym ,sym))

(define (til-list a)
  "Makes a list of values into a tree-il list."
  `(primcall list ,@a))

(define (map-gensym argnames)
  (map (lambda (x) (gensym (string-append
                            (symbol->string x) "$")))
       argnames))

(define (compile-tree-il exp env opts)
    "Compiles a given python3 expressions in a given environment into a
corresponding tree-il expression."
  (let ((ret (comp exp '())))
    (values
     (parse-tree-il ret)
     env
     env)))

(define* (comp x e #:optional toplevel)
  "Compiles a given python3 expressions in a given environment into a
corresponding tree-il expression."
  (pmatch x
    ;; module code
    ((<module> ,stmts)
     (comp-block #t stmts e))

    ;; stmt code
    ((<function-def> ,id ,args ,body ,decos ,ret)
     (do-assign id (comp-fun-body id args body e) e toplevel))
    ((<class-def> ,id ,bases ,keywords ,starargs ,kwargs ,body ,decos)
     (do-assign id
                (comp-class-def id bases keywords starargs kwargs body decos e)
                e toplevel))
    ((<return> ,exp)
     `(primcall return ,(comp exp e)))
    ((<assign> ,targets ,value)
     (do-assign targets (comp value e) e toplevel))
    ((<aug-assign> ,target ,op ,value)
     (comp `(<assign> (,target) (<bin-op> ,target ,op ,value))  e toplevel))
    ((<global> ,names)
     '(void)) ;; TODO: Check if any id in names is bound by a fundef.
    (<pass>
     '(void))
    ((<expr> ,exp)
     (if toplevel
         (comp exp e)
         `(begin ,(comp exp e) (void))))

    ;; expressions
    ((<bin-op> ,eleft ,op ,eright)
     (comp-bin-op op eleft eright e))
    ((<bool-op> ,op ,lst)
     (comp-bool-op op lst e))
    ((<unary-op> ,op ,arg)
     (comp-unary-op op arg e))
    ((<if> ,b ,e1 ,e2)
     `(if ,(comp b e)
          ,(comp-block #f e1 e)
          ,(comp-block #f e2 e)))
    ((<compare> ,eleft ,ops ,rest)
     (let ((cops (til-list (map (lambda (x) (comp-op x)) ops)))
           (vals (til-list (cons (comp eleft e)
                                 (map (lambda (x) (comp x e)) rest)))))
       (@impl compare cops vals)))
    ((<call> ,fun ,args ,kws, ,stararg ,kwargs)
     (let ((c-fun (comp fun e))
           (c-args (map (lambda (x) (comp x e)) args)))
       `(call ,c-fun ,@c-args)))
    ((<num> ,n)
     `(const ,n))
    ((<attribute> ,exp ,attr ,ctx)
     (@impl getattr (comp exp e) `(const ,attr)))
    ((<name> ,name ,ctx)
     (let ((ret (lookup name e)))
       (if ret
           `(lexical ,name ,ret)
           (-> (toplevel name)))))
    ((<tuple> ,exps ,ctx)
     (comp-list-or-tuple exps e))
    ((<list> ,exps ,ctx)
     (comp-list-or-tuple exps e))
    (,any
     (debug "not matched:" any))))

(define (comp-list-or-tuple exps env)
  "Compiles a list or tuple expression into a list of values."
  (til-list (map (lambda (x) (comp x env)) exps)))

(define (comp-block toplevel stmts env)
  "Compiles a block of statements."
  (define (get-ids targets)
    (pmatch targets
      (((<name> ,id <store>))
       (list id))))
  (if (null? stmts)
      '(void)
      `(begin ,@(map (lambda (stmt) (comp stmt env toplevel)) stmts))))

;; Handles all types of calls not involving kwargs and keyword
;; arguments.
(define (comp-fun-body id args body env)
  "Compiles a function body."
  (let* ((stararg (cadr args))
         (argnames (if (and (car args) stararg)
                       (append (map car (car args)) (list stararg))
                       (or (map car (car args)) stararg '())))
         (argconsts (map (lambda (x) `(const ,x)) argnames))
         (inits (map (lambda (x) (comp x env)) (seventh args)))
         (rest (gensym "rest$"))
         (argsym (gensym "args$"))
         ;; (kwargsym (gensym "kwargs$"))    kwargs not implemented yet
         (gensyms (map-gensym argnames))
         (lg (locals-and-globals body #:exclude argnames))
         (local-sym (gensym "locals$"))
         (locals (cons local-sym (car lg)))
         (local-syms (cons local-sym (map-gensym (cdr locals))))
         (globals (cadr lg)))
    `(lambda ()
       (lambda-case
        ((() #f ,rest
          (#f (#:args ,argsym ,argsym)) ;; (#:kwargs ,kwargsym ,kwargsym))
          ((const #f)) (,rest ,argsym))
         (let-values
             (primcall apply (primitive values)
                       (primcall append
                                 ,(@impl fun-match-arguments
                                         `(const ,id)
                                         `(primcall list ,@argconsts)
                                         `(const ,(if stararg #t #f))
                                         (lex1 rest)
                                         (lex1 argsym)
                                         ;; (lex1 kwargsym)
                                         `(primcall list ,@inits))
                                 (primcall cons
                                           ,(til-list (map (lambda (x) `(const ,x)) (cdr locals)))
                                           ,(til-list (replicate (1- (length locals)) '(const #nil))))))
           (lambda-case
            ((,(append argnames locals) #f #f () () ,(append gensyms local-syms))
             ,(comp-block #f body (add2env env (append argnames locals)
                                           (append gensyms local-syms)))))))))))

(define (comp-class-def id bases keywords starargs kwargs body decos env)
  (let ((bases '(const ())))
    (let lp ((stmts body) (out '()))
      (pmatch stmts
        (()
         (@impl make-python3-class `(const ,id) bases (til-list (reverse! out))))
        ((,stmt . ,rest)
         (pmatch stmt
           ((<assign> ,targets ,value)
            (let ((ids (get-targets targets)))
              (if (null? (cdr ids))
                  (lp rest (cons `(primcall cons (const ,(car ids))
                                            ,(comp value env)) out))
                  (error "not implemented"))))
           ((<function-def> ,id ,args ,body ,decos ,ret)
            (lp rest (cons `(primcall cons (const ,id)
                                      ,(comp-fun-body id args body env)) out)))
           ((<class-def> ,id ,bases ,keywords ,starargs ,kwargs ,body ,decos)
            ;; FIXME: probably need to update the environment
            (lp rest (cons `(primcall cons (const ,id)
                                      ,(comp-class-def id bases keywords starargs
                                                       kwargs body decos env))
                           out)))
           (,any
            (lp rest (cons `(primcall cons (const #f) ,(comp any env)) out)))))))))

(define (comp-op op)
  (define ops '((<gt> . >) (<lt> . <) (<gt-e> . >=) (<lt-e> . <=) (<eq> . equal?)))
  `(toplevel ,(lookup op ops)))

(define (comp-bin-op op e1 e2 env)
  (define ops '((<add> . +) (<sub> . -) (<mult> . *) (<div> . /)
                (<floor-div> . floor-quotient)
                (<mod> . euclidean-remainder)
                (<bit-xor> . logxor)
                (<bit-and> . logand)
                (<bit-or> . logior)))
  (let ((ce1 (comp e1 env))
        (ce2 (comp e2 env)))
    (pmatch op
      (<l-shift>
       `(call (toplevel ash) ,ce1 ,ce2))
      (<r-shift>
       `(call (toplevel ash) ,ce1 (call (toplevel -) ,ce2)))
      (,any
       `(call (toplevel ,(lookup op ops)) ,ce1 ,ce2)))))

(define (comp-bool-op op lst env)
  (define (and b a)
    `(if ,a ,b ,a))
  (define (or b a)
    `(if ,a ,a ,b))
  (let ((clst (map (lambda (x) (comp x env)) lst)))
    (pmatch op
      (<and>
       (reduce and (const #t) clst))
      (<or>
       (reduce or (const #f) clst)))))

(define (comp-unary-op op arg env)
  (pmatch op
    (<not>
     `(if ,(comp arg env) (const #f) (const #t)))))

;;;; The documentation for let-values in tree-il is incorrect. This is
;;;; an example for how it could be used.
;; (let-values (call (primitive apply)
;;                   (primitive values)
;;                   (call (primitive list) (const 3) (const 4)))
;;   (lambda-case (((a b) #f #f () () (a b)) (lexical b b))))

(define (do-assign targets val env toplevel)
  ;; TODO: Match lists etc as targets. VAL should then be an iterable.
  (define (doit id val)
    (if toplevel
        `(define ,id ,val)
        (let ((sym (lookup id env)))
          `(set! ,(if sym `(lexical ,id ,sym) `(toplevel ,id)) ,val))))
  (let ((ids (get-targets targets)))
    (if (null? (cdr ids))
        (doit (car ids) val))))

(define (get-targets targets)
  (pmatch targets
    (,id
     (guard (symbol? id))
     (list id))
    (((<name> ,id <store>))
     (list id))
    (,any
     (error (string-append "Not matched: " (object->string any))))))

(define* (locals-and-globals s #:key (exclude '()))
  "This method returns the local and global variables used in a list of
statements. The returned value is on the form (LOCALS GLOBALS). The
EXCLUDE keyword argument is used to exclude certain symbols from the
returned local variables."
  ;; TODO match while for etc
  (let lp ((stmts s) (locals '()) (globals '()))
    (pmatch stmts
      (((<global> ,vars) . ,rest)
       (lp rest locals (apply lset-adjoin eq? globals vars)))
      (((<assign> ((<name> ,var <store>)) ,val) . ,rest)
       (lp rest (lset-adjoin eq? locals var) globals))
      (((<if> ,test ,body ,orelse) . ,rest)
       (lp (append body orelse rest) locals globals))
      (((<function-def> ,id ,args ,body ,decos ,ret) . ,rest)
       (lp rest (lset-adjoin eq? locals id) globals))
      ((,any . ,rest)
       (lp rest locals globals))
      (()
       (list (lset-difference eq? locals (append exclude globals)) globals)))))

(define (test str)
  (let ((code ((@ (language python3 parse) read-python3) (open-input-string str))))
    (display-ln code)
    (let ((tree-il (compile-tree-il code '() '())))
      (unparse-tree-il tree-il))))
