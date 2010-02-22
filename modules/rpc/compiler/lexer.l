;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.  -*- coding: utf-8 -*-
;;; Copyright (C) 2008, 2010  Free Software Foundation, Inc.
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

space          [ \n\12\9]
identifier     [a-zA-Z][a-zA-Z0-9_]*
dec-constant   -?([0-9]|[1-9][0-9]+)
hex-constant   0x[0-9a-fA-F]+
oct-constant   0[0-7]+

%%

{dec-constant}   (location yyline yycolumn
                           'constant (string->number yytext))
{hex-constant}   (location yyline yycolumn
                           'constant
                           (string->number (string-drop yytext 2) 16))
{oct-constant}   (location yyline yycolumn
                           'constant (string->number yytext 8))

{identifier}     (cond ((member yytext
                                '("enum" "struct" "union"
                                  "case" "switch" "default"
                                  "const" "opaque" "void"
                                  "unsigned" "int" "hyper"
                                  "float" "double" "quadruple"
                                  "bool" "string"
                                  "typedef"
                                  "program" "version"))
                        (location yyline yycolumn
                                  (string->symbol yytext)))
                       (else
                        (location yyline yycolumn
                                  'identifier yytext)))
{comment}

;; Sun's `rpcgen' recognizes comments on a single line starting with `%'.
;; It is non-standard (per RFC 4506), but it's widely used in Sun's `.x'
;; header files.
^{space}*%.*$    (if (*strip-%-comments?*)
                     (yycontinue)
                     (location yyline yycolumn
                               'error))

\{               (location yyline yycolumn 'left-brace)
\}               (location yyline yycolumn 'right-brace)
\[               (location yyline yycolumn 'left-square)
\]               (location yyline yycolumn 'right-square)
<                (location yyline yycolumn 'left-angle)
>                (location yyline yycolumn 'right-angle)
\(               (location yyline yycolumn 'left-parenthesis)
\)               (location yyline yycolumn 'right-parenthesis)

\;               (location yyline yycolumn 'semi-colon)
:                (location yyline yycolumn 'colon)
=                (location yyline yycolumn 'equal)
,                (location yyline yycolumn 'comma)
\*               (location yyline yycolumn 'star)
{space}

<<EOF>>          '*eoi*
<<ERROR>>        (location yyline yycolumn 'error)
