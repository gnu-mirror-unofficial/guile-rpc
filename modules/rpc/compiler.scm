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

(define-module (rpc compiler)
  :autoload    (rpc compiler parser) (xdr-language->sexp)
  :use-module  (rpc xdr types)
  :use-module  (rpc xdr)

  :use-module  (srfi srfi-1)
  :use-module  (srfi srfi-9)

  :export (xdr-language->scheme
           xdr-language->xdr-types))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module contains a compiler of specification written in the XDR
;;; Language.  It has two back-ends: one that compiles XDR specification into
;;; Scheme code (a list of S-exps) and another one that compiles to run-time
;;; XDR type objects.
;;;
;;; Code:


;;;
;;; Compilation context (environment).
;;;

(define-record-type <context>
  (make-context types constants)
  context?
  (types     context-types)
  (constants context-constants))

(define (cons-constant name value context)
  (make-context (context-types context)
                (alist-cons name value (context-constants context))))

(define (cons-type name value context)
  (make-context (alist-cons name value (context-types context))
                (context-constants context)))

(define (lookup-constant name context)
  (let ((result (assoc name (context-constants context))))
    (and (pair? result)
         (cdr result))))

(define (lookup-type name context)
  (let ((result (assoc name (context-types context))))
    (and (pair? result)
         (cdr result))))


;;;
;;; Compilation.
;;;


(define (make-xdr-language-translator initial-context

                                      make-constant-def
                                      make-type-def
                                      make-constant-ref
                                      make-type-ref

                                      make-enum
                                      make-struct
                                      make-union
                                      make-string
                                      make-fixed-length-array
                                      make-variable-length-array)
  (lambda (input)
    (define (constant-value expr c)
      (cond ((string? expr)
             (let ((value (lookup-constant expr c)))
               (if (not value)
                   (error "unbound constant" expr)
                   (make-constant-ref value))))
            ((number? expr)
             expr)
            (else
             (error "invalid constant type" c))))

    (define (make-constant-definition name expr c)
      (if (number? expr)
          (make-constant-def name expr)
          (error "invalid value in constant definition" expr)))

    (define (make-type-definition name expr c)
      (make-type-def
       name
       (cond ((string? expr)
              (let ((def (lookup-type expr c)))
                (if (not def)
                    (error "unbound type" expr)
                    (make-type-ref def))))
             ((and (list? expr) (pair? expr))
              (case (car expr)
                ((enum)
                 (let ((values (cdr expr)))
                   (make-enum name
                              (map (lambda (name+value)
                                     (let ((name  (car name+value))
                                           (value (cadr name+value)))
                                       (cons name
                                             (constant-value value c))))
                                   values))))
                ((struct)
                 (let ((types (map (lambda (name+type)
                                     (let* ((type-name (cadr name+type))
                                            (type      (lookup-type type-name
                                                                    c)))
                                       (if type
                                           (make-type-ref type)
                                           (error "type not found"
                                                  type-name))))
                                   (cdr expr))))
                   (make-struct types)))
                ((string)
                 (let ((max-length (cadr expr)))
                   (make-string max-length)))
                (else
                 (error "not implemented yet" (car expr)))))
            (else
             (error "invalid type definition form" expr)))))

    (let ((input (cond ((port? input)
                        (xdr-language->sexp input))
                       ((string? input)
                        (xdr-language->sexp (open-input-string input)))
                       ((list? input)
                        input)
                       (else
                        (error "invalid argument" input)))))

      (fold (lambda (expr c)
              (case (car expr)
                ((define-constant)
                 (let ((name  (cadr expr))
                       (value (caddr expr)))
                   (cons-constant (cadr expr)
                                  (make-constant-definition
                                   name value c)
                                  c)))
                ((define-type)
                 (let ((name (cadr expr))
                       (spec (caddr expr)))
                   (cons-type name
                              (make-type-definition name spec c)
                              c)))
                (else
                 (error "unrecognized expression" expr))))
            initial-context
            input))))


;;;
;;; Code generation back-end.
;;;

(define (schemify-name str)
  ;; Stolen from Guile-Reader 0.5.
  "Turn @var{str}, a C variable or function name, into a more ``Schemey''
form, e.g., one with dashed instead of underscores, etc."
  (string->symbol
   (string-map (lambda (chr)
                 (if (eq? chr #\_)
                     #\-
                     chr))
               (cond ((string-suffix? "_p" str)
                      (string-append (substring str 0
                                                (- (string-length str) 2))
                                     "?"))
                     ((string-suffix? "_x" str)
                      (string-append (substring str 0
                                                (- (string-length str) 2))
                                     "!"))
                     (else str)))))

(define type-name->symbol schemify-name)
(define constant-name->symbol schemify-name)
(define enum-value-name->symbol schemify-name)

(define (constant-definition-code name value)
  `(define ,(constant-name->symbol name)
     ,value))

(define (type-definition-code name typespec)
  `(define ,(type-name->symbol name)
     ,typespec))

(define (type-ref-code def)
  (define definition-name cadr)
  (definition-name def))

(define constant-ref-code type-ref-code)

(define (enum-code name values)
  (let ((values (map (lambda (n+v)
                       (let ((value (cdr n+v)))
                         (cons (enum-value-name->symbol (car n+v))
                               (if (symbol? value)
                                   (list 'unquote value)
                                   value))))
                     values)))
    `(make-xdr-enumeration ',(type-name->symbol name)
                           ,(list 'quasiquote values))))

(define (struct-code types)
  `(make-xdr-struct-type (list ,@types)))

(define (union-code . args)
  (error "not implemented yet" args))

(define (string-code max-length)
  `(make-xdr-string ,max-length))

(define (fixed-length-array-code . args)
  (error "not implemented yet" args))

(define (variable-length-array-code . args)
  (error "not implemented yet" args))

(define xdr-language->scheme
  (let* ((initial-context
          ;; The initial compilation context.
          (make-context
           '(("bool"             . (define xdr-boolean ...))
             ("int"              . (define xdr-integer ...))
             ("unsigned int"     . (define xdr-unsigned-integer ...))
             ("hyper"            . (define xdr-hyper-integer ...))
             ("unsigned hyper"   . (define xdr-unsigned-hyper-integer ...))
             ("float"            . (define xdr-float ...))
             ("double"           . (define xdr-double ...))
             ;; FIXME: We lack support for `quadruple'.
             )
           '()))
         (known-type? (lambda (name)
                        (lookup-type name initial-context)))
         (translator
          (make-xdr-language-translator initial-context

                                        constant-definition-code
                                        type-definition-code
                                        type-ref-code
                                        constant-ref-code

                                        enum-code
                                        struct-code
                                        union-code
                                        string-code
                                        fixed-length-array-code
                                        variable-length-array-code)))

    (lambda (input)
      (let ((output (translator input)))
        (and (context? output)

             ;; Output constant definitions first, then type definitions.
             (append (reverse (map cdr (context-constants output)))
                     (reverse (filter-map (lambda (name+def)
                                            (let ((name (car name+def)))
                                              (and (not (known-type? name))
                                                   (cdr name+def))))
                                          (context-types output)))))))))



;;;
;;; Run-time type generation back-end.
;;;

(define (->schemey-enum-alist values)
  (map (lambda (n+v)
         (cons (enum-value-name->symbol (car n+v))
               (cdr n+v)))
       values))

(define xdr-language->xdr-types
  (let* ((initial-context
          ;; The initial compilation context.
          (make-context
           `(("bool"             . ,xdr-boolean)
             ("int"              . ,xdr-integer)
             ("unsigned int"     . ,xdr-unsigned-integer)
             ("hyper"            . ,xdr-hyper-integer)
             ("unsigned hyper"   . ,xdr-unsigned-hyper-integer)
             ("float"            . ,xdr-float)
             ("double"           . ,xdr-double)
             ;; FIXME: We lack support for `quadruple'.
             )
           '()))
         (known-type? (lambda (name)
                        (lookup-type name initial-context)))

         (constant-definition   (lambda (name value) value))
         (type-definition       (lambda (name spec) spec))
         (type-ref              (lambda (spec) spec))
         (constant-ref          (lambda (value) value))
         (enum                  (lambda (name values)
                                  (let ((name   (type-name->symbol name))
                                        (values (->schemey-enum-alist values)))
                                    (make-xdr-enumeration name values))))
         (struct                make-xdr-struct-type)
         (union                 #f)
         (string                make-xdr-string)
         (fixed-length-array    #f)
         (variable-length-array #f)

         (translator
          (make-xdr-language-translator initial-context

                                        constant-definition
                                        type-definition
                                        type-ref
                                        constant-ref

                                        enum
                                        struct
                                        union
                                        string
                                        fixed-length-array
                                        variable-length-array)))
  (lambda (input)
    (let ((output (translator input)))
      (and (context? output)
           (filter-map (lambda (name+type)
                         (let ((name (car name+type)))
                           (and (not (known-type? name))
                                (cdr name+type)
                                name+type)))
                       (context-types output)))))))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; compiler.scm ends here
