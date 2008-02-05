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
  :export (xdr-parser))


(define xdr-parser
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
    identifier constant
    eof)

   ;; Starting point
   (specification (definition) : $1
                  () : '())

   ;; Constant and type definitions

   (constant-def (const identifier equal constant semi-colon) :
                 (list 'constant-def $2 $4))


   (type-def (typedef declaration semi-colon) : $1
             (enum identifier enum-body semi-colon) : $1
             (struct identifier struct-body semi-colon) : $1
             (union identifier union-body semi-colon) : $1)

   (definition (type-def) : $1
               (constant-def) : $1)


   ;; Production rules.
   (declaration (type-specifier identifier) : $1
                (type-specifier identifier left-square value right-square) :
                $1
                (type-specifier identifier left-angle value right-angle) :
                $1
                (opaque identifier left-square value right-square) :
                $1
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
                $1)

   (value (constant)   : $1
          (identifier) : $1)

   (type-specifier (int) : $1
                   (unsigned int) : $1
                   (hyper) : $1
                   (unsigned hyper) : $1
                   (float) : $1
                   (double) : $1
                   (quadruple) : $1
                   (bool) : $1
                   (enum-type-spec) : $1
                   (struct-type-spec) : $1
                   (union-type-spec) : $1
                   (identifier) : $1)

   ;; Enums

   (enum-type-spec (enum enum-body) :                     (cons 'enum $1))

   (name-value-list (identifier equal value) :            (cons $1 $3)
                    (identifier equal value
                                comma name-value-list) :  (cons $1 $3)
                    ():                                   '())
   (enum-body (left-brace name-value-list right-brace) :
              (list $3))


   ;; Structs

   (struct-type-spec (struct struct-body) : $1)

   (struct-body (left-brace declaration-list right-brace) : $1)

   (declaration-list (declaration semi-colon) : $1
                     (declaration semi-colon declaration-list) : $1)


   ;; Unions

   (union-type-spec (union union-body) : $1)

   (union-body (switch left-parenthesis declaration right-parenthesis
                left-brace case-spec-list optional-switch-default
                right-brace) : $1)

   (optional-switch-default (default colon declaration semi-colon) : $1
                            () : '())

   (case-spec (case-list declaration semi-colon) : $1)

   (case-list (case value colon) : $1
              (case value colon case-list) : $1)

   (case-spec-list (case-spec) : $1
                   (case-spec case-spec-list) : $1)



   ;; The end

   (done (eof) : '*eoi*)
   ))

(print-states)

(lexer-init 'port (open-input-string (string-append
                                      ;;"enum blurps { x = 2, z = 3 };"
                                      "typedef int foo_t;"
                                      "const x = 2;"
                                      )
                                     ;;"123"
                                     ))
(define result
  (xdr-parser (lambda ()
                (let ((r (lexer)))
                  (format (current-error-port) "TOKEN: ~A~%" r)
                  r))
              (lambda (a b)
                (error "parse error" (list a b)))))

(format (current-error-port) "result: ~a~%" result)


;;; Local Variables:
;;; coding: latin-1
;;; End:
