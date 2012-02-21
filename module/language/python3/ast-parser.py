#!/usr/bin/python3

### Python 3 for Guile

## Copyright (C) 2012 Stefan Kangas.
## Copyright (C) 2012 Per Reimers.
## Copyright (C) 2012 David Spångberg.
## Copyright (C) 2012 Krister Svanlund.

#### This library is free software; you can redistribute it and/or
#### modify it under the terms of the GNU Lesser General Public
#### License as published by the Free Software Foundation; either
#### version 3 of the License, or (at your option) any later version.
####
#### This library is distributed in the hope that it will be useful,
#### but WITHOUT ANY WARRANTY; without even the implied warranty of
#### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.    See the GNU
#### Lesser General Public License for more details.
####
#### You should have received a copy of the GNU Lesser General Public
#### License along with this library; if not, write to the Free Software
#### Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# todo:

# - most of the definitions
# - cleanup. Maybe use yield or return instead of print in every method?
# - tests

"""\
This module prints out an AST (in scheme syntax) of a given python file
to standard output. The produced AST has a similar representation as the
AST defined here:
    http://docs.python.org/py3k/library/ast.html

Given zero arguments, this module reads python source code from standard
input, otherwise it's only argument is the path to a python file. The
code is parsed and then translated into a representation of the python
ast in scheme.\
"""
import os, sys, ast

def main(argv):
    if len(argv) < 2:
      pyfile = sys.stdin
      astree = ast.parse(pyfile.read())
    elif argv[1] == "--help" or argv[1] == "-h":
      print(__doc__)
      exit(1)
    else:
      pyfile = argv[1]
      with open(pyfile, 'r') as f:
        astree = ast.parse(f.read(), pyfile)

    print_mod(astree)

# In the python documentation mod can also be "Interactive",
# "Expression" and "Suite". We probably dont need them so leave them
# out for now.
def print_mod(mod):
    printl("(<mod> (")

    print_list(print_stmt, mod.body)

    print("))")

##################################################
## Statements

def print_stmt(stmt):
    t = type(stmt)

    if t == ast.FunctionDef:
        print_fundef(stmt)
    elif t == ast.ClassDef:
        print_classdef(stmt)
    elif t == ast.Return:
        print_return(stmt)
    elif t == ast.Delete:
        print_delete(stmt)
    elif t == ast.Assign:
        print_assign(stmt)
    elif t == ast.AugAssign:
        print_aug_assign(stmt)

    elif t == ast.For:
        print_for(stmt)
    elif t == ast.While:
        print_while(stmt)
    elif t == ast.If:
        print_if(stmt)
    elif t == ast.With:
        print_with(stmt)

    elif t == ast.Raise:
        print_raise(stmt)
    elif t == ast.TryExcept:
        print_try_except(stmt)
    elif t == ast.TryFinally:
        print_try_finally(stmt)
    elif t == ast.Assert:
        print_assert(stmt)

    elif t == ast.Import:
        print_import(stmt)
    # elif t == ast.ImportFrom:
    #     print_import_from(stmt)

    # elif t == ast.Global:
    #     print_global(stmt)
    # elif t == ast.Nonlocal:
    #     print_non-local(stmt)
    elif t == ast.Expr:
        printl("(<expr> ")
        print_expr(stmt.value)
        printl(")")
    elif t == ast.Pass:
        print_pass(stmt)
    # elif t == ast.Break:
    #     print_break(stmt)
    # elif t == ast.Continue:
    #     print_continue(stmt)

    else:
        raise NotImplementedError("No match :" + str(stmt))

# todo
def print_fundef(stmt):
    printl("(<fundef> ")
    printl(stmt.name, "")
    print_args(stmt.args)

    printl(" (")
    print_list(print_stmt, stmt.body)
    printl(") ")

    printl("(")
    print_list(print_expr, stmt.decorator_list)
    printl(") ")

    print_expr(stmt.returns)
    printl(")")

def print_classdef(stmt):
    printl("(<classdef>", stmt.name, "(")
    print_list(print_expr, stmt.bases)
    printl(") (")
    print_list(print_keyword, stmt.keywords)
    printl(") ")
    print_expr(stmt.starargs)
    printl(" ")
    print_expr(stmt.kwargs)
    printl(" (")
    print_list(print_stmt, stmt.body)
    printl(") (")
    print_list(print_expr, stmt.decorator_list)
    printl("))")

def print_return(ret):
    printl("(<return> ")
    print_expr(ret.value)
    printl(")")

def print_delete(d):
    printl("(<delete> (")
    print_list(print_expr, d.targets, sep=") (")
    printl("))")

def print_if(stmt):
    printl("(<if> ")
    print_expr(stmt.test)
    printl(" (")
    print_list(print_stmt, stmt.body)
    printl(") (")
    print_list(print_stmt, stmt.orelse)
    printl("))")

def print_import(imp):
    printl("<imp>")

def print_pass(_):
    printl("<pass>")

def print_assign(assig):
    printl("(<assign> (")
    print_list(print_expr, assig.targets)
    printl(") ")
    print_expr(assig.value)
    printl(")")

def print_aug_assign(stmt):
    def print_op(op):
        t = type(op)
        if t == ast.Add:
            printl("Add ")
        elif t == ast.Sub:
            printl("Sub ")
        elif t == ast.Mult:
            printl("Mult ")
        elif t == ast.Div:
            printl("Div ")
        elif t == ast.Mod:
            printl("Mod ")
        elif t == ast.Pow:
            printl("Pow ")
        elif t == ast.LShift:
            printl("LShift ")
        elif t == ast.RShift:
            printl("RShift ")
        elif t == ast.BitOr:
            printl("BitOr ")
        elif t == ast.BitXor:
            printl("BitXor ")
        elif t == ast.BitAnd:
            printl("BitAnd ")
        elif t == ast.FloorDiv:
            printl("FloorDiv ")
        else:
            raise NotImplementedError("No match :" + str(op))
    printl("(<aug-assign> ")
    print_expr(stmt.target)
    printl(" ")
    print_op(stmt.op)    
    print_expr(stmt.value)
    printl(")")

def print_for(stmt):
    printl("(<for> ")
    print_expr(stmt.target)
    printl(" ")
    print_expr(stmt.iter)
    printl(" (")
    print_list(print_stmt, stmt.body)
    printl(") (")
    print_list(print_stmt, stmt.orelse)
    printl(")")

def print_while(stmt):
    printl("(<while> ")
    print_expr(stmt.test)
    printl(" (")
    print_list(print_stmt, stmt.body)
    printl(") (")
    print_list(print_stmt, stmt.orelse)
    printl(")")
    
def print_with(stmt):
    printl("(<with> ")
    print_expr(stmt.context_expr)
    printl(" ")
    print_expr(stmt.optional_vars)
    printl(" ")
    print_list(print_stmt, stmt.body)
    printl(")")

def print_raise(stmt):
    printl("(<raise> ")
    print_expr(stmt.exc)
    printl(" ")
    print_expr(stmt.cause)
    printl(")")

def print_try_except(stmt):
    printl("(<try-except> (")
    print_list(print_stmt, stmt.body)
    printl(") (")
    print_list(print_excepthandler, stmt.handlers)
    printl(") (")
    print_list(print_stmt, stmt.orelse)
    printl("))")
    
def print_try_finally(stmt):
    printl("(<try-finally> (")
    print_list(print_stmt, stmt.body)
    printl(") (")
    print_list(print_stmt, stmt.finalbody)
    printl("))")  
    
def print_excepthandler(eh):
    printl("(")
    print_expr(eh.type)
    printl(" ")
    printl(str(eh.name)) # Possibly None
    printl(" (")
    print_list(print_stmt, eh.body)
    printl("))")
    
def print_assert(stmt):
    printl("(<assert>")
    print_expr(eh.test)
    printl(" ")
    print_expr(eh.msg)
    printl(")")

##################################################
## expressions

def print_expr(expr):
    t = type(expr)

    if t == ast.BoolOp:
        print_boolop(expr)
    elif t == ast.BinOp:
        print_binop(expr)
    # elif t == ast.UnaryOp:
    #     print_UnaryOp(expr)
    # elif t == ast.Lambda:
    #     print_Lambda(expr)
    # elif t == ast.IfExp:
    #     print_IfExp(expr)
    elif t == ast.Dict:
        print_dict(expr)
    # elif t == ast.Set:
    #     print_Set(expr)
    # elif t == ast.ListComp:
    #     print_ListComp(expr)
    # elif t == ast.SetComp:
    #     print_SetComp(expr)
    # elif t == ast.DictComp:
    #     print_DictComp(expr)
    # elif t == ast.GeneratorExp:
    #     print_GeneratorExp(expr)
    # elif t == ast.Yield:
    #     print_Yield(expr)
    elif t == ast.Compare:
        print_compare(expr)
    elif t == ast.Call:
        print_call(expr)
    elif t == ast.Num:
        print_num(expr)
    elif t == ast.Str:
        print_str(expr)
    # elif t == ast.Bytes:
    #     print_Bytes(expr)
    # elif t == ast.ellipsis:
    #     print_Ellipsis(expr)
    # other literals? bools?

    # # the following expression can appear in assignment context
    elif t == ast.Attribute:
        print_attribute(expr)
    # elif t == ast.Subscript:
    #     print_Subscript(expr)
    # elif t == ast.Starred:
    #     print_Starred(expr)
    elif t == ast.Name:
        print_name(expr)
    elif t == ast.List:
        print_list_or_tuple(expr) # print_list already taken
    elif t == ast.Tuple:
        print_list_or_tuple(expr)

    # special case for None
    elif expr == None:
        printl("None")
    else:
        raise NotImplementedError("No match :" + str(expr))

def print_boolop(expr):
    def print_op(op):
        t = type(op)
        if t == ast.And:
            printl("And ")
        elif t == ast.Or:
            printl("Or ")
        else:
            raise NotImplementedError("No match :" + str(op))
    printl("(<bool-op> ")
    print_op(expr.op)
    printl("(")
    print_list(print_expr, expr.values)
    printl("))")

def print_binop(expr):
    def print_op(op):
        t = type(op)
        if t == ast.Add:
            printl("Add ")
        elif t == ast.Sub:
            printl("Sub ")
        elif t == ast.Mult:
            printl("Mult ")
        elif t == ast.Div:
            printl("Div ")
        elif t == ast.Mod:
            printl("Mod ")
        elif t == ast.Pow:
            printl("Pow ")
        elif t == ast.LShift:
            printl("LShift ")
        elif t == ast.RShift:
            printl("RShift ")
        elif t == ast.BitOr:
            printl("BitOr ")
        elif t == ast.BitXor:
            printl("BitXor ")
        elif t == ast.BitAnd:
            printl("BitAnd ")
        elif t == ast.FloorDiv:
            printl("FloorDiv ")
        else:
            raise NotImplementedError("No match :" + str(op))

    printl("(<bin-op> ")
    print_op(expr.op)
    print_list(print_expr, [expr.left, expr.right])
    printl(")")

    # BinOp(expr left, operator op, expr right)
    # (<bin-op> op left right)

def print_expr_context(ctx):
    t = type(ctx)
    if t == ast.Load:
        printl("<load>")
    elif t == ast.Store:
        printl("<store>")
    elif t == ast.Del:
        printl("<del>")
    elif t == ast.AugLoad:
        printl("<aug-load>")
    elif t == ast.AugStore:
        printl("<aug-store>")
    elif t == ast.Param:
        printl("<param>")

def print_dict(expr):
    printl("(<dict> (")
    print_list(print_expr, expr.keys)
    printl(") (")
    print_list(print_expr, expr.values)
    printl("))")

def print_compare(expr):
    printl("(<cmp> ")
    print_expr(expr.left)
    printl(" (")
    print_list(print_cmpop, expr.ops)
    printl(") (")
    print_list(print_expr, expr.comparators)
    printl("))")

def print_call(expr):
    printl("(<call> ")
    print_expr(expr.func)
    printl(" (")
    print_list(print_expr, expr.args)
    printl(") (")
    print_list(print_keyword, expr.keywords)
    printl(") ")
    print_expr(expr.starargs)
    printl(" ")
    print_expr(expr.kwargs)
    printl(")")

def print_num(expr):
    printl("(<num>", expr.n)
    printl(")")

def print_str(expr):
    printl('(<str> "%s")' % expr.s)

def print_attribute(expr):
    printl("(<attribute> ")
    print_expr(expr.value)
    printl("", expr.attr, "")
    print_expr_context(expr.ctx)
    printl(")")

def print_name(expr):
    printl("(<name>", expr.id, "")
    print_expr_context(expr.ctx)
    printl(")")

# do we need the <list> constructor?
def print_list_or_tuple(expr):
    if type(expr) == ast.List:
        printl("(<list> ")
    else:
            printl("(<tuple> ")
    print_list(print_expr, expr.elts)
    printl(" ")
    print_expr_context(expr.ctx)
    printl(")")


##################################################
## other

def print_cmpop(comp):
    t = type(comp)

    if t == ast.Eq:
        printl("Eq")
    elif t == ast.NotEq:
        printl("NotEq")
    elif t == ast.Lt:
        printl("Lt")
    elif t == ast.LtE:
        printl("LtE")
    elif t == ast.Gt:
        printl("Gt")
    elif t == ast.GtE:
        printl("GtE")
    elif t == ast.Is:
        printl("Is")
    elif t == ast.IsNot:
        printl("IsNot")
    elif t == ast.In:
        printl("In")
    elif t == ast.NotIn:
        printl("NotIn")
    else:
        raise NotImplementedError("No match :" + str(expr))

def print_args(args):
    printl("(<arguments> (")

    print_list(print_arg, args.args)

    printl(")", args.vararg, "")
    print_expr(args.varargannotation)
    printl(" (")

    print_list(print_arg, args.kwonlyargs)

    printl(")", args.kwarg, "")
    print_expr(args.kwargannotation)

    printl(" (")
    print_list(print_expr, args.defaults)
    printl(")")

    printl(" (")
    print_list(print_expr, args.kw_defaults)

    printl("))")

def print_arg(arg):
    printl("(<arg>", arg.arg, arg.annotation)
    printl(")")

def print_keyword(kw):
    printl("(<keyword>", kw.arg, "")
    print_expr(kw.value)
    printl(")")

def printl(*str, **kwargs):
    print(*str, end="", **kwargs)

def print_list(fun, elems, sep=" "):
    first = True
    for e in elems:
        if first:
            first = False
        else:
            printl(sep)
        fun(e)

if __name__ == '__main__':
    main(sys.argv)
    # if len(sys.argv) > 1:
    #   main(sys.argv)
    # else:
    #   print(__doc__)

