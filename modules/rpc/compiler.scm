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
  :use-module  (rpc compiler parser)
  :use-module  (rpc xdr types)
  :use-module  (rpc xdr)

  :use-module  (srfi srfi-1)
  :use-module  (srfi srfi-9)
  :use-module  (srfi srfi-34)
  :use-module  (srfi srfi-35)

  ;; Andrew K. Wright's pattern matching system.
  :use-module  (ice-9 match)

  :re-export   (&compiler-error compiler-error? compiler-error:location)
  :export (rpc-language->scheme-client rpc-language->scheme-server
           rpc-language->xdr-types

           &compiler-unknown-type-error &compiler-unknown-constant-error
           compiler-unknown-type-error? compiler-unknown-constant-error?
           compiler-unknown-type-error:type-name
           compiler-unknown-constant-error:constant-name))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module contains a compiler of specification written in the XDR/RPC
;;; Language.  It has two back-ends: one that compiles XDR/RPC specification
;;; into Scheme code (a list of S-exps) and another one that compiles to
;;; run-time XDR type objects.
;;;
;;; Code:


;;;
;;; Error conditions.
;;;

(define-condition-type &compiler-unknown-type-error &compiler-error
  compiler-unknown-type-error?
  (type-name    compiler-unknown-type-error:type-name))

(define-condition-type &compiler-unknown-constant-error &compiler-error
  compiler-unknown-constant-error?
  (constant-name compiler-unknown-constant-error:constant-name))


;;;
;;; Workarounds.
;;;

;; Work around missing export in `(ice-9 match)' in Guile 1.8.4 and earlier.
(if (not (defined? 'match:andmap))
    (define match:andmap (@@ (ice-9 match) match:andmap)))



;;;
;;; Compilation context (environment).
;;;

(define-record-type <context>
  (make-context types constants programs)
  context?
  (types     context-types)
  (constants context-constants)
  (programs  context-programs))

(define (cons-constant name value context)
  (make-context (context-types context)
                (alist-cons name value (context-constants context))
                (context-programs context)))

(define (cons-type name value context)
  (make-context (alist-cons name value (context-types context))
                (context-constants context)
                (context-programs context)))

(define (cons-program name value context)
  (make-context (context-types context)
                (context-constants context)
                (alist-cons name value (context-programs context))))

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

(define %self-reference
  (cons 'self 'reference))

(define (self-reference? x)
  ;; Return true if X denotes a reference to type being defined, a so-called
  ;; "self-reference".
  (eq? x %self-reference))


(define (make-rpc-language-translator initial-context

                                      make-constant-def
                                      make-type-def
                                      make-constant-ref
                                      make-type-ref

                                      make-enum
                                      make-struct
                                      make-union
                                      make-string
                                      make-fixed-length-array
                                      make-variable-length-array

				      make-program
				      make-version
				      make-procedure)
  (lambda (input)
    (define (constant-value expr c location)
      (cond ((string? expr)
             (let ((value (lookup-constant expr c)))
               (if (not value)
                   (raise (condition (&compiler-unknown-constant-error
                                      (location      location)
                                      (constant-name expr))))
                   (make-constant-ref value))))
            ((number? expr)
             expr)
            (else
             (error "invalid constant type" c))))

    (define (make-constant-definition name expr c)
      (if (number? expr)
          (make-constant-def name expr)
          (error "invalid value in constant definition" expr)))

    (define (type-ref expr c name location)
      ;; Evaluate EXPR, a type specifier, through the compiler back-end, and
      ;; return a "type reference", whatever that means to the back-end.
      ;; When called from a program definition, NAME can be used to specify
      ;; the typedef name of the type begin defined.
      (define (unbound-type type location)
        (raise (condition (&compiler-unknown-type-error
                           (location  location)
                           (type-name type)))))

      (match expr
        ((? string?)
         (let ((def (lookup-type expr c)))
           (cond ((not def)
                  (unbound-type expr location))
                 ((self-reference? def)
                  ;; Leave self-references as is.  It's up to the back-end to
                  ;; resolve them.
                  def)
                 (else
                  (make-type-ref def)))))

        (('enum values ..1)
         (let ((values (cdr expr)))
           (make-enum (or name (symbol->string (gensym "enum")))
                      (map (lambda (name+value)
                             (let ((name  (car name+value))
                                   (value (cadr name+value)))
                               (cons name
                                     (constant-value value c
                                                     (sexp-location
                                                      name+value)))))
                           values))))

        (('struct types ..1)
         (let ((types (map (let ((location (sexp-location expr)))
                             (lambda (name+type)
                               (type-ref (cadr name+type) c #f
                                         location)))
                           (cdr expr))))
           (make-struct types)))

        (('union ('case ((and (? string?) discriminant)
                         (and (? string?) type-name))
                   case-list ...))
         (let* ((type-ref*  (lambda (arm)
                              ;; Arm types can be either `"void"' or
                              ;; `("field-name" <typespec>)'.
                              (match arm
                                ("void" (type-ref "void" c #f
                                                  (sexp-location expr)))
                                (((? string?) typespec)
                                 (type-ref typespec c #f
                                           (sexp-location expr)))
                                (else
                                 (error "wrong arm type" arm)))))
                (default    (let ((last (car (last-pair case-list))))
                              (and (eq? (car last) 'else)
                                   (cadr last))))
                (value/type (append-map (lambda (case-spec)
                                          (match case-spec
                                            (('else _) '())
                                            (((values ..1) type)
                                             (let ((t (type-ref* type)))
                                               (map (lambda (v)
                                                      (cons v t))
                                                    values)))
                                            (else
                                             (error "wrong case spec"
                                                    case-spec))))
                                        case-list)))

           (make-union (let ((discr-type (lookup-type type-name c)))
                         ;; FIXME: Should use `type-ref'
                         (if discr-type
                             (make-type-ref discr-type)
                             (unbound-type type-name
                                           (sexp-location (cadr (cdr expr))))))
                       value/type
                       (and default (type-ref* default)))))

        (('string (and (or (? number?) (? string?) (? not)) max-length))
         (let ((max-length (and max-length
                                (constant-value max-length c
                                                (sexp-location expr)))))
           (make-string max-length)))

        (('fixed-length-array (and (? string?) type-name)
                              (and (or (? number?) (? string?)) length))
         ;; FIXME: Should be using `type-ref' instead
         (let ((type   (lookup-type type-name c))
               (length (and length (constant-value length c
                                                   (sexp-location expr)))))
           (if type
               (make-fixed-length-array (make-type-ref type) length)
               (unbound-type type-name (sexp-location expr)))))

        (('variable-length-array (and (? string?) type-name)
                                 (and (or (? number?) (? string?) (? not))
                                      max-length))
         ;; FIXME: Should be using `type-ref' instead
         (let ((type       (lookup-type type-name c))
               (max-length (and max-length
                                (constant-value max-length c
                                                (sexp-location expr)))))
           (if type
               (make-variable-length-array (make-type-ref type) max-length)
               (unbound-type type-name (sexp-location expr)))))

        (else
         (error "unsupported type form" expr))))

    (define (make-type-definition name expr c location)
      ;; Process EXPR, a type definition for NAME.
      (make-type-def name (type-ref expr c name location)))

    (define (make-program-definition expr c)
      ;; Process EXPR, an RPC program definition.
      (define (version-procs proc-defs)
        (map (lambda (def)
               (define loc
                 (sexp-location def))

               (match def
                 (('procedure (and (? string?) name)
                              (and (? integer?) number)
                              ret-type
                              (arg-types ...))
                  (let ((ret-type  (type-ref ret-type c #f loc))
                        (arg-types (map (lambda (t)
                                          (type-ref t c #f loc))
                                        arg-types)))
                    (make-procedure name number ret-type arg-types)))))
             proc-defs))

      (match expr
        (('define-program (and (? string?) program-name)
                          (and (? integer?) program-number)
                          (and ('version _ ...) versions) ..1)
         (let ((versions (map (lambda (v)
                                (match v
                                  (('version (and (? string?) name)
                                             (and (? integer?) number)
                                             (and ('procedure _ ..1) procs)
                                             ..1)
                                   (make-version name number (version-procs procs)))))
                              versions)))
           (make-program program-name program-number versions)))))

    (let ((input (cond ((port? input)
                        (rpc-language->sexp input))
                       ((string? input)
                        (rpc-language->sexp (open-input-string input)))
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
                              (make-type-definition name spec
                                                    (cons-type name
                                                               %self-reference
                                                               c)
                                                    (sexp-location expr))
                              c)))
                ((define-program)
                 (let ((name (cadr expr)))
                   (cons-program name
                                 (make-program-definition expr c)
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
  (define type-name
    (type-name->symbol name))

  (define (resolve-self-references tree)
    ;; Turn self-references to NAME into a thunk that returns NAME.
    (let loop ((tree tree))
      (cond ((list? tree)
             (map loop tree))
            ((self-reference? tree)
             `(lambda () ,type-name))
            (else tree))))

  `(define ,type-name
     ,(resolve-self-references typespec)))

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

(define (union-code discr-type value/type default)
  `(make-xdr-union-type ,discr-type
                        ,(list 'quasiquote
                               (map (lambda (v+t)
                                      (let ((v (car v+t)))
                                        (cons (if (number? v)
                                                  v
                                                  (enum-value-name->symbol v))
                                              (list 'unquote (cdr v+t)))))
                                    value/type))
                        ,default))

(define (string-code max-length)
  `(make-xdr-string ,max-length))

(define (fixed-length-array-code type length)
  ;; FIXME: This is extremely inefficient.
  (if (eq? type 'xdr-opaque)
      `(make-xdr-fixed-length-opaque-array ,length)
      `(make-xdr-struct-type (make-list ,length ,type))))

(define (variable-length-array-code type max-length)
  (if (eq? type 'xdr-opaque)
      `(make-xdr-variable-length-opaque-array ,max-length)
      `(make-xdr-vector-type ,type ,max-length)))

(define (rpc-program-code/client program-name program-number versions)
  ;; Return client-side Scheme code for the given program, i.e., definitions
  ;; of procedures returned by `make-synchronous-rpc-call'.

  (define (make-procedure-name proc-name)
    (schemify-name (string-append program-name "-" proc-name)))

  (append-map (lambda (version)
                (let ((version-number (cadr version))
                      (procs          (caddr version)))
                  (map (lambda (proc)
                         (let ((name      (car proc))
                               (number    (cadr proc))
                               (ret-type  (caddr proc))
                               (arg-types (cadddr proc)))
                           `(define ,(make-procedure-name name)
                              (make-synchronous-rpc-call
                               ,program-number ,version-number ,number
                               ,ret-type
                               ,(cond ((null? arg-types)
                                       'xdr-void)
                                      ((null? (cdr arg-types))
                                       (car arg-types))
                                      (else
                                       `(make-xdr-struct-type
                                         (list ,@arg-types))))))))
                       (reverse procs))))
              (reverse versions)))

(define (rpc-program-code/server program-name program-number versions)
  ;; Return server-side Scheme code for the given program.  FIXME: We need to
  ;; find a convention allowing the user to specify "handlers".
  (let ((program-name
         (schemify-name (string-append program-name "-program"))))

    (define (make-proc-code proc)
      (let ((number    (cadr proc))
            (ret-type  (caddr proc))
            (arg-types (cadddr proc)))
        `(make-rpc-procedure ,number
                             ,ret-type
                             ,(cond ((null? arg-types)
                                     'xdr-void)
                                    ((null? (cdr arg-types))
                                     (car arg-types))
                                    (else
                                     `(make-xdr-struct-type
                                       (list ,@arg-types))))
                             FIXME:need-a-way-to-specify-handler)))

    `(define ,program-name
       (make-rpc-program ,program-number
          (list ,@(map (lambda (version)
                         (let ((version-number (cadr version))
                               (procs          (caddr version)))
                           `(make-rpc-program-version ,version-number
                              (list ,@(map make-proc-code procs)))))
                       versions))))))

(define (rpc-version-code . args)
  args)

(define (rpc-procedure-code . args)
  args)

(define (make-rpc-language->scheme rpc-program-code)
  (let* ((initial-context
          ;; The initial compilation context.
          (make-context
           '(("void"             . (define xdr-void ...))
             ("opaque"           . (define xdr-opaque ...)) ;; pseudo-type
             ("bool"             . (define xdr-boolean ...))
             ("int"              . (define xdr-integer ...))
             ("unsigned int"     . (define xdr-unsigned-integer ...))
             ("hyper"            . (define xdr-hyper-integer ...))
             ("unsigned hyper"   . (define xdr-unsigned-hyper-integer ...))
             ("float"            . (define xdr-float ...))
             ("double"           . (define xdr-double ...))
             ;; FIXME: We lack support for `quadruple'.
             )
           '()
           '()))
         (known-type? (lambda (name)
                        (lookup-type name initial-context)))
         (translator
          (make-rpc-language-translator initial-context

                                        constant-definition-code
                                        type-definition-code
                                        type-ref-code
                                        constant-ref-code

                                        enum-code
                                        struct-code
                                        union-code
                                        string-code
                                        fixed-length-array-code
                                        variable-length-array-code

                                        rpc-program-code
                                        rpc-version-code
                                        rpc-procedure-code)))

    (lambda (input)
      (let ((output (translator input)))
        (and (context? output)

             ;; Output constant definitions first, then type definitions, the
             ;; program/procedure definitions.
             (append (reverse (map cdr (context-constants output)))
                     (reverse (filter-map (lambda (name+def)
                                            (let ((name (car name+def)))
                                              (and (not (known-type? name))
                                                   (cdr name+def))))
                                          (context-types output)))
                     (reverse (concatenate
                               (map cdr (context-programs output))))))))))

(define rpc-language->scheme-client
  (make-rpc-language->scheme rpc-program-code/client))

(define rpc-language->scheme-server
  (make-rpc-language->scheme rpc-program-code/server))



;;;
;;; Run-time type generation back-end.
;;;

(define (resolve-type type self)
  ;; Resolve type, i.e., replacing self-references with SELF or invoking it
  ;; with SELF.
  (cond ((self-reference? type)
         self)
        ((procedure? type)
         (type self))
        (else
         type)))

(define (->schemey-enum-alist values)
  (map (lambda (n+v)
         (cons (enum-value-name->symbol (car n+v))
               (cdr n+v)))
       values))

(define (->schemey-union-values-alist values self)
  (map (lambda (v+t)
         (let ((value (car v+t)))
           (cons (if (number? value)
                     value
                     (enum-value-name->symbol value))
                 (resolve-type (cdr v+t) self))))
       values))

(define rpc-language->xdr-types
  ;; To handle recursive types, the strategy here is to represent types using
  ;; one-argument procedures that are to be invoked at type-definition time
  ;; and passed a "self-referencing thunk".  A self-referencing thunk is a
  ;; nullary procedure that, when applied, returns a reference to the type
  ;; being defined.
  (let* ((initial-context
          ;; The initial compilation context.
          (make-context
           `(("void"             . ,xdr-void)
             ("opaque"           . opaque) ;; pseudo-type
             ("bool"             . ,xdr-boolean)
             ("int"              . ,xdr-integer)
             ("unsigned int"     . ,xdr-unsigned-integer)
             ("hyper"            . ,xdr-hyper-integer)
             ("unsigned hyper"   . ,xdr-unsigned-hyper-integer)
             ("float"            . ,xdr-float)
             ("double"           . ,xdr-double)
             ;; FIXME: We lack support for `quadruple'.
             )
           '()
           '()))
         (known-type? (lambda (name)
                        (lookup-type name initial-context)))

         (constant-definition   (lambda (name value) value))
         (type-definition       (lambda (name spec)
                                  ;; Pass SPEC a thunk that returns a
                                  ;; reference to the type being defined.
                                  ;; This thunk is then propagated as the
                                  ;; SELF argument to all type instantiating
                                  ;; procedures below.
                                  (letrec ((type (if (procedure? spec)
                                                     (spec (lambda () type))
                                                     spec)))
                                    type)))
         (type-ref              (lambda (spec) spec))
         (constant-ref          (lambda (value) value))
         (enum                  (lambda (name values)
                                  (let ((name   (type-name->symbol name))
                                        (values (->schemey-enum-alist values)))
                                    (lambda (self)
                                      (make-xdr-enumeration name values)))))
         (struct                (lambda (types)
                                  (lambda (self)
                                    (make-xdr-struct-type
                                     (map (lambda (type)
                                            (resolve-type type self))
                                          types)))))
         (union                 (lambda (discr values default)
                                  (lambda (self)
                                    (let ((values (->schemey-union-values-alist
                                                   values self)))
                                      (make-xdr-union-type
                                       (resolve-type discr self)
                                       values
                                       (and default (resolve-type default self)))))))
         (string                (lambda (max-length)
                                  (lambda (self)
                                    (make-xdr-string max-length))))
         (fixed-length-array    (lambda (type length)
                                  (lambda (self)
                                    (let ((type (resolve-type type self)))
                                      (if (eq? type 'opaque)
                                          (make-xdr-fixed-length-opaque-array
                                           length)
                                          (make-xdr-struct-type
                                           (make-list length type)))))))
         (variable-length-array (lambda (type limit)
                                  (lambda (self)
                                    (let ((type (resolve-type type self)))
                                      (if (eq? type 'opaque)
                                          (make-xdr-variable-length-opaque-array
                                           limit)
                                          (make-xdr-vector-type type limit))))))

         (translator
          (make-rpc-language-translator initial-context

                                        constant-definition
                                        type-definition
                                        type-ref
                                        constant-ref

                                        enum
                                        struct
                                        union
                                        string
                                        fixed-length-array
                                        variable-length-array

                                        ;; FIXME: RPC program handling not
                                        ;; implemented.
                                        #f #f #f)))
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
