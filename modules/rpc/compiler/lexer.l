;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.
;;; Copyright (C) 2008  Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Guile-RPC.
;;;
;;; GNU Guile-RPC is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guile-RPC is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Comment regexp stolen from http://ostermiller.org/findcomment.html .
comment        /\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*\*+/

space          [ \n\9]
identifier     [a-zA-Z][a-zA-Z0-9_]*
dec-constant   -?([0-9]|[1-9][0-9]+)
hex-constant   0x[0-9a-fA-F]+
oct-constant   0[0-7]+

%%

{dec-constant}   (cons 'constant (string->number yytext))
{hex-constant}   (cons 'constant (string->number (string-drop yytext 2) 16))
{oct-constant}   (cons 'constant (string->number yytext 8))

{identifier}     (cond ((member yytext
                                '("enum" "struct" "union"
                                  "case" "switch" "default"
                                  "const" "opaque" "void"
                                  "unsigned" "int" "hyper"
                                  "float" "double" "quadruple"
                                  "bool" "string"
                                  "typedef"))
                        (list (string->symbol yytext)))
                       (else
                        (cons 'identifier yytext)))
{comment}

\{               (list 'left-brace)
\}               (list 'right-brace)
\[               (list 'left-square)
\]               (list 'right-square)
<                (list 'left-angle)
>                (list 'right-angle)
\(               (list 'left-parenthesis)
\)               (list 'right-parenthesis)

\;               (list 'semi-colon)
:                (list 'colon)
=                (list 'equal)
,                (list 'comma)
\*               (list 'star)
{space}

<<EOF>>          '*eoi*
<<ERROR>>        (list 'error)


;;; Local Variables:
;;; mode: scheme
;;; coding: latin-1
;;; End:
