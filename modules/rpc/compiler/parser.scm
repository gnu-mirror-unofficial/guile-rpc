;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.  -*- coding: utf-8 -*-
;;; Copyright (C) 2008, 2010, 2012  Free Software Foundation, Inc.
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
  :use-module  (system base lalr)
  :use-module  (srfi srfi-1)
  :use-module  (srfi srfi-34)
  :use-module  (srfi srfi-35)
  :use-module  (srfi srfi-39)

  :export (rpc-language->sexp *parser-options*

           location-line location-column location-file
           sexp-location

           &compiler-error compiler-error? compiler-error:location
           lexer-error? parser-error? parser-error:token

           %debug-rpc-parser?))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module provides a parser for the XDR/RPC Language (RFC 4506, Section
;;; 6, and RFC 1831, Section 11), which allows the definition of XDR data
;;; types and RPC programs.  A number of Sun extensions are also made
;;; available through the `*parser-options*' SRFI-39 parameter.
;;;
;;; Code:


;;;
;;; Error conditions.
;;;

(define-condition-type &compiler-error &error
  ;; We start the error hierarchy here because `(rpc compiler)' depends on
  ;; this module.
  compiler-error?
  (location  compiler-error:location))

(define-condition-type &lexer-error &compiler-error
  lexer-error?)

(define-condition-type &parser-error &compiler-error
  parser-error?
  (token   parser-error:token))



;;;
;;; Location tracking.
;;;

(define (sexp-location sexp)
  ;; Return the external representation (an alist) of the location of SEXP.
  (and (pair? sexp)
       (source-properties sexp)))

(define (make-location-accessor field)
  (lambda (location)
    (and (pair? location)
         (let ((p (assq field location)))
           (and (pair? p) (cdr p))))))

(define location-line   (make-location-accessor 'line))
(define location-column (make-location-accessor 'column))
(define location-file   (make-location-accessor 'filename))

(define (location sexp)
  ;; Return the location information of SEXP, using our the lexer/parser
  ;; internal format.
  (let ((line   (source-property sexp 'line))
        (column (source-property sexp 'column)))
    (vector line column)))

(define (export-location location)
  ;; Export LOCATION from its internal representation as produced by the
  ;; `location' procedure to a `location?' object for external consumption.
  (let ((line   (vector-ref location 0))
        (column (vector-ref location 1)))
    `((line . ,line) (column . ,column))))

(define (preserve-location location sexp)
  ;; Copy location information from SOURCE-SEXP, an expression returned by
  ;; the lexer, to SEXP, and return SEXP.
  (set-source-properties! sexp (export-location location))
  sexp)



;;;
;;; Parser.
;;;

(define *parser-options*
  ;; A list of symbols denoting options for the parser.  The empty list means
  ;; strict RFC 4506 conformance.
  (make-parameter '()))


(define (make-rpc-parser)
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
    identifier constant
    program version)

   ;; Starting point
   (specification (definition specification) : (cons $1 $2)
                  (*eoi*) : '())

   ;; Constant and type definitions

   (constant-def (const identifier equal constant semi-colon) :
                   (preserve-location (cadr $2)
                                      (list 'define-constant
                                            (car $2) (car $4))))


   (type-def (typedef declaration semi-colon) :
               (preserve-location (location $2)
                                  (cons 'define-type $2))
             (enum identifier enum-body semi-colon) :
               (preserve-location (cadr $2)
                                  (list 'define-type (car $2)
                                        (preserve-location (car $1) $3)))
             (struct identifier struct-body semi-colon) :
               (preserve-location (cadr $2)
                                  (list 'define-type (car $2)
                                        (preserve-location (car $1) $3)))
             (union identifier union-body semi-colon) :
               (preserve-location (cadr $2)
                                  (list 'define-type (car $2)
                                        (preserve-location (car $1) $3))))

   (definition (type-def) : $1
               (constant-def) : $1
               (program-def) : $1)


   ;; Production rules.
   (declaration (type-specifier identifier) :
                  (preserve-location (cadr $2)
                                     (list (car $2) $1))
                (type-specifier identifier left-square value right-square) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (list 'fixed-length-array $1 $4)))
                (type-specifier identifier left-angle value right-angle) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (list 'variable-length-array $1 $4)))
                (type-specifier identifier left-angle right-angle) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (list 'variable-length-array $1 #f)))
                (opaque identifier left-square value right-square) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (preserve-location
                                            (car $1)
                                            (list 'fixed-length-array "opaque" $4))))
                (opaque identifier left-angle value right-angle) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (preserve-location
                                            (car $1)
                                            (list 'variable-length-array "opaque" $4))))
                (opaque identifier left-angle right-angle) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (preserve-location
                                            (car $1)
                                            (list 'variable-length-array "opaque" #f))))
                (string identifier left-angle value right-angle) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (preserve-location (car $1)
                                                              (list 'string $4))))
                (string identifier left-angle right-angle) :
                  (preserve-location (cadr $2)
                                     (list (car $2)
                                           (preserve-location (car $1)
                                                              (list 'string #f))))
                (type-specifier star identifier) :
                  (preserve-location (cadr $3)
                                     (list (car $3)
                                           (preserve-location
                                            (car $2)
                                            `(union (case ("opted" "bool")
                                                      (("TRUE")
                                                       ("value" ,$1))
                                                      (("FALSE") "void"))))))
                (void) :
                  "void")

   (value (constant)   : (car $1)
          (identifier) : (car $1))

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
                   (identifier) : (car $1)

                   ;; non-standard extensions

                   (unsigned) :
                     ;; Sun's `rpcgen' recognizes "unsigned" as "unsigned int".
                     (if (memq 'allow-unsigned (*parser-options*))
                         "unsigned int"
                         (raise (condition (&parser-error
                                            (location (export-location (car $1)))
                                            (token    'unsigned)))))
                   (struct identifier) :
                     ;; Sun's `rpcgen' allows referring to struct types using
                     ;; "struct T" instead of just "T".
                     (if (memq 'allow-struct-type-specifier
                               (*parser-options*))
                         (car $2)
                         (raise (condition (&parser-error
                                            (location (export-location (car $1)))
                                            (token    'struct))))))

   ;; `type-specifier' extended with `string'
   (type-specifier* (type-specifier) : $1
                    (string) :
                      ;; Sun's `rpcgen' allows use of the `string' type as the
                      ;; type-specifier of an RPC parameter.
                      (if (memq 'allow-string-param-type-specifier
                                (*parser-options*))
                          "string"
                          (raise (condition (&parser-error
                                             (location (export-location (car $1)))
                                             (token    'string))))))

   ;; Enums

   (enum-type-spec (enum enum-body) :
                     (preserve-location (car $1) $2))

   (name-value-list (identifier equal value) :
                      (list (preserve-location (cadr $1)
                                               (list (car $1) $3)))
                    (identifier equal value
                     comma name-value-list) :
                      (cons (preserve-location (cadr $1)
                                               (list (car $1) $3))
                            $5))

   (enum-body (left-brace name-value-list right-brace) :
                (cons 'enum $2))


   ;; Structs

   (struct-type-spec (struct struct-body) :
                       (preserve-location (car $1) $2))

   (struct-body (left-brace declaration-list right-brace) :
                  (cons 'struct $2))

   (declaration-list (declaration semi-colon) :
                       (list $1)
                     (declaration semi-colon declaration-list) :
                       (cons $1 $3))


   ;; Unions

   (union-type-spec (union union-body) :
                      (preserve-location (car $1) $2))

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
                   (case-spec case-spec-list) : (cons $1 $2))


   ;; RPC Language (RFC 1831, Section 11.2).

   (program-def (program identifier left-brace
                 version-def-list right-brace
                 equal constant semi-colon) :
     (preserve-location (cadr $2)
                        (cons* 'define-program (car $2) (car $7) $4)))

   (version-def-list (version-def) : (list $1)
                     (version-def version-def-list) : (cons $1 $2))

   (version-def (version identifier left-brace
                 procedure-def-list right-brace
                 equal constant semi-colon) :
     (preserve-location (cadr $2)
                        (cons* 'version (car $2) (car $7) $4)))

   (procedure-def-list (procedure-def) : (list $1)
                       (procedure-def procedure-def-list) : (cons $1 $2))

   (procedure-def (type-specifier-or-void identifier
                   left-parenthesis type-specifier-list-or-void
                   right-parenthesis
                   equal constant semi-colon) :
     (preserve-location (cadr $2)
                        (list 'procedure (car $2) (car $7) $1
                              $4)))

   ;; The next two clauses work around a bug in RFC 1831, Section 11.2, which
   ;; does not allow `void' in lieu of an argument list or return type, even
   ;; though the example in Section 11.1 uses it.
   (type-specifier-list-or-void (type-specifier-list) : $1
                                (void) : '())
   (type-specifier-or-void (type-specifier*) : $1
                           (void) : "void")

   (type-specifier-list (type-specifier*) : (list $1)
                        (type-specifier* comma type-specifier-list) :
                          (cons $1 $3))))


;;;
;;; User interface.
;;;

(define %debug-rpc-parser?
  ;; Set to `#t' to debug the parser.
  #f)

(define (rpc-language->sexp port)
  "Read a specification written in the XDR Language from @var{port} and
return the corresponding sexp-based representation."
  (define (%parse-error msg token)
    (let* ((expr     (lexical-token-value token))
           (token    (lexical-token-category token))
           (location (export-location (car (last-pair expr)))))
      (raise (condition (&parser-error
                         (token    token)
                         (location location))))))

  ;; FIXME: This method is not reentrant.  See the "Usage2" node of the SILex
  ;; manual for better.
  (lexer-init 'port port)

  (let ((lexer (if %debug-rpc-parser?
                   (lambda ()
                     (let ((r (lexer)))
                       (format (current-error-port) "TOKEN: ~A~%" r)
                       r))
                   lexer)))
    ((make-rpc-parser) lexer %parse-error)))

;;; parser.scm ends here
