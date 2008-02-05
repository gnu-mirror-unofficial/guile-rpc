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

(define-module (rpc compiler parser)
  :autoload    (rpc compiler lexer) (lexer-init)
  :use-module  (text parse-lalr)
  :use-module  (srfi srfi-1)

  :export (xdr-language->sexp %debug-xdr-parser?))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module provides a parser for the XDR Language (RFC 4506, Section 6),
;;; which allows the definition of XDR data types.
;;;
;;; Code:


;;;
;;; Parser.
;;;

(define xdr-parser
  ;; The XDR Language parser.

  (lalr-parser
   ;; Terminal symbols.
   (comment
    left-brace right-brace left-angle right-angle left-square right-square
    left-parenthesis right-parenthesis
    semi-colon colon comma star equal
    case switch default
    enum struct union const opaque void
    unsigned int hyper float double quadruple bool string
    typedef
    identifier constant)

   ;; Starting point
   (specification (definition specification) : (cons $1 $2)
                  (*eoi*) : '())

   ;; Constant and type definitions

   (constant-def (const identifier equal constant semi-colon) :
                 (list 'define-constant $2 $4))


   (type-def (typedef declaration semi-colon) :
               (cons 'define-type $2)
             (enum identifier enum-body semi-colon) :
               (list 'define-type $2 $3)
             (struct identifier struct-body semi-colon) :
               (list 'define-type $2 $3)
             (union identifier union-body semi-colon) :
               (list 'define-type $2 $3))

   (definition (type-def) : $1
               (constant-def) : $1)


   ;; Production rules.
   (declaration (type-specifier identifier) :
                  (list $2 $1)
                (type-specifier identifier left-square value right-square) :
                  (list $2 (list 'fixed-length-array-type $1 $4))
                (type-specifier identifier left-angle value right-angle) :
                  (list 'variable-length-array-type $2 $4)
                (opaque identifier left-square value right-square) :
                  (list 'declaration $2 $4)
                (opaque identifier left-angle value right-angle) :
                $1
                (opaque identifier left-angle right-angle) :
                $1
                (string identifier left-angle value right-angle) :
                $1
                (string identifier left-angle right-angle) :
                $1
                (type-specifier star identifier) :
                $1
                (void) :
                "void")

   (value (constant)   : $1
          (identifier) : $1)

   (type-specifier (int) : "int"
                   (unsigned int) : "unsigned int"
                   (hyper) : "hyper"
                   (unsigned hyper) : "unsigned hyper"
                   (float) : "float"
                   (double) : "double"
                   (quadruple) : "quadruple"
                   (bool) : "bool"
                   (enum-type-spec) : $1
                   (struct-type-spec) : $1
                   (union-type-spec) : $1
                   (identifier) : $1)

   ;; Enums

   (enum-type-spec (enum enum-body) :
                     $2)

   (name-value-list (identifier equal value) :
                      (list (list $1 $3))
                    (identifier equal value
                                comma name-value-list) :
                      (cons (list $1 $3) $5))
   (enum-body (left-brace name-value-list right-brace) :
                (cons 'enum $2))


   ;; Structs

   (struct-type-spec (struct struct-body) :
                       $2)

   (struct-body (left-brace declaration-list right-brace) :
                  (cons 'struct $2))

   (declaration-list (declaration semi-colon) :
                       (list $1)
                     (declaration semi-colon declaration-list) :
                       (cons $1 $3))


   ;; Unions

   (union-type-spec (union union-body) : $1)

   (union-body (switch left-parenthesis declaration right-parenthesis
                left-brace case-spec-list switch-default
                right-brace) :
                 (list 'union (cons* 'case $3 (append $6 (list $7))))
               (switch left-parenthesis declaration right-parenthesis
                left-brace case-spec-list
                right-brace) :
                 (list 'union (cons* 'case $3 $6)))

   (switch-default (default colon declaration semi-colon) :
                     (list 'else $3))

   (case-spec (case-list declaration semi-colon) :
                (list $1 $2))

   (case-list (case value colon) : (list $2)
              (case value colon case-list) : (cons $2 $4))

   (case-spec-list (case-spec) : (list $1)
                   (case-spec case-spec-list) : (cons $1 $2))))


;;;
;;; User interface.
;;;

(define %debug-xdr-parser?
  ;; Set to `#t' to debug the parser.
  #f)

(define (xdr-language->sexp port)
  "Read a specification written in the XDR Language from @var{port} and
return the corresponding sexp-based representation."
  (define (%parse-error msg . args)
    (error msg args))

  ;; FIXME: This method is not reentrant.  See the "Usage2" node of the SILex
  ;; manual for better.
  (lexer-init 'port port)

  (let ((lexer (if %debug-xdr-parser?
                   (lambda ()
                     (let ((r (lexer)))
                       (format (current-error-port) "TOKEN: ~A~%" r)
                       r))
                   lexer)))
    (xdr-parser lexer %parse-error)))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; parser.scm ends here
