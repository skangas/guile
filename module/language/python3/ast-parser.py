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
    print(universal(astree))

def universal(x):
    if hasattr(x, "_fields"):
        usename = type(x).__name__[0].isupper()
        name = "<"
        for c in type(x).__name__:
            if name == "<" or c.islower():
                name += c.lower()
            elif c.isupper():
                name += "-" + c.lower()
            else:
                raise NotImplementedError(
                    "Class name %s contains illegal char %s" 
                        % (type(x).__name__, c))
        name += ">"
        fields = [universal(x.__getattribute__(field)) for field in x._fields]
        if not fields:
            return name
        elif usename:
            return "(%s %s)" % (name, " ".join(fields))
        else:
            return "(%s)" % (" ".join(fields))
    elif type(x) == list:
        fields = [universal(field) for field in x]
        return "(%s)" % (" ".join(fields))
    else:
        return str(x)
  
if __name__ == '__main__':
    main(sys.argv)
    # if len(sys.argv) > 1:
    #   main(sys.argv)
    # else:
    #   print(__doc__)

