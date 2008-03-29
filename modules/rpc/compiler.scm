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
  :use-module  (srfi srfi-39)

  ;; Andrew K. Wright's pattern matching system.
  :use-module  (ice-9 match)

  :re-export   (&compiler-error compiler-error? compiler-error:location)
  :export (rpc-language->scheme-client rpc-language->scheme-server
           rpc-language->xdr-types

           *compiler-options*

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
  (make-context types fwdrefs constants programs)
  context?
  (types     context-types)
  (fwdrefs   context-forward-type-refs)
  (constants context-constants)
  (programs  context-programs))

(define (cons-constant name value context)
  (make-context (context-types context)
                (context-forward-type-refs context)
                (alist-cons name value (context-constants context))
                (context-programs context)))

(define (cons-type name value context)
  ;; Return a new context based on CONTEXT with the addition of type VALUE
  ;; named NAME.  In addition, pending forward references to NAME are
  ;; resolved and added to CONTEXT.
  (let* ((refs (context-forward-type-refs context))
         (fwd  (assoc name refs))
         (ctx  (make-context (alist-cons name value (context-types context))
                             (if (pair? fwd)
                                 (alist-delete name refs string=?)
                                 refs)
                             (context-constants context)
                             (context-programs context))))
    (if (pair? fwd)
        (fold (lambda (forward-ref ctx)
                ;; Resolve the forward reference.  FORWARD-REF is a procedure
                ;; that's passed the new context that includes the name it
                ;; was referring to.
                (forward-ref ctx))
              ctx
              (cdr fwd))
        ctx)))

(define (cons-forward-type-ref name proc context)
  ;; Add to CONTEXT a forward type reference to NAME.  PROC will be invoked
  ;; either when NAME is added through `cons-type' or when compilation is
  ;; over and NAME was not defined.
  (make-context (context-types context)
                (let ((name+procs (assoc name
                                         (context-forward-type-refs context))))
                  (alist-cons name
                              (if (pair? name+procs)
                                  (cons proc (cdr name+procs))
                                  (list proc))
                              (context-forward-type-refs context)))
                (context-constants context)
                (context-programs context)))

(define (cons-program name value context)
  (make-context (context-types context)
                (context-forward-type-refs context)
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

(define *compiler-options*
  ;; A list of symbols denoting options for the parser.  The empty list means
  ;; strict RFC 4506 conformance.
  (make-parameter '()))


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
                         discriminant-type)
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

           (make-union (type-ref discriminant-type c #f
                                 (or (sexp-location discriminant-type)
                                     (sexp-location (cadr (cadr expr)))))
                       value/type
                       (and default (type-ref* default)))))

        (('string (and (or (? number?) (? string?) (? not)) max-length))
         (let ((max-length (and max-length
                                (constant-value max-length c
                                                (sexp-location expr)))))
           (make-string max-length)))

        (('fixed-length-array typespec
                              (and (or (? number?) (? string?)) length))
         (let ((type   (type-ref typespec c #f
                                 (or (sexp-location typespec)
                                     (sexp-location expr))))
               (length (and length (constant-value length c
                                                   (sexp-location expr)))))
           (make-fixed-length-array type length)))

        (('variable-length-array typespec
                                 (and (or (? number?) (? string?) (? not))
                                      max-length))
         (let ((type       (type-ref typespec c #f
                                     (or (sexp-location typespec)
                                         (sexp-location expr))))
               (max-length (and max-length
                                (constant-value max-length c
                                                (sexp-location expr)))))
           (make-variable-length-array type max-length)))

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

      (define (iterate context)
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
                     (define (make-new-context c)
                       (cons-type name
                                  (make-type-definition name spec
                                                        (cons-type name
                                                                   %self-reference
                                                                   c)
                                                        (sexp-location expr))
                                  c))

                     (if (memq 'allow-forward-type-references
                               (*compiler-options*))
                         ;; Catch any unbound type (perhaps a forward
                         ;; reference) so we can try again later.
                         (guard (e ((compiler-unknown-type-error? e)
                                    (let ((name
                                           (compiler-unknown-type-error:type-name
                                            e)))
                                      (cons-forward-type-ref name
                                                             make-new-context
                                                             c))))
                           (make-new-context c))
                         (make-new-context c))))
                  ((define-program)
                   (let ((name (cadr expr)))
                     (cons-program name
                                   (make-program-definition expr c)
                                   c)))
                  (else
                   (error "unrecognized expression" expr))))
              initial-context
              input))

      (let ((output (iterate initial-context)))
        (if (not (null? (context-forward-type-refs output)))
            (map (lambda (forward-ref)
                   ;; This is an unresolved (forward) type reference.  We
                   ;; invoke it with the context we got, which should raise an
                   ;; `unbound-type' exception (so `map' is not really needed).
                   (forward-ref output))
                 (append-map cdr (context-forward-type-refs output))))

        output))))


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
  ;; Return server-side Scheme code for the given program.  An example
  ;; invocation of the function that's produced looks like this:
  ;;
  ;;   (make-NFS-PROGRAM-server
  ;;     `(("NFS_VERSION"
  ;;        ("NFSPROC_PROCNULL" . ,stub)
  ;;        ("NFSPROC_LOOKUP"   . ,stub))))
  ;;
  ;; IOW, callers pass a list of version/procedures pair, where versions are
  ;; identified by their name; procedures are specified as a list of
  ;; name/handler pairs, where the name is the procedure's name.  Version and
  ;; procedure name lookup is performed beforehand, at instantiation time.
  (let ((make-program-name
         (schemify-name (string-append "make-" program-name "-server"))))

    (define proc-code
      `(let ((version-procs (cddr version)))
         (filter-map (lambda (user-proc)
                       (let* ((name    (car user-proc))
                              (handler (cdr user-proc))
                              (proc    (assoc name version-procs)))
                         (and proc
                              (let ((number    (car (cdr proc)))
                                    (ret-type  (cadr (cdr proc)))
                                    (arg-types (caddr (cdr proc))))
                                (make-rpc-procedure number ret-type
                                                    arg-types handler)))))
                     user-procs)))

    `((define (,make-program-name versions+procs)
        (define %program-versions
          ,(list 'quasiquote
                 (map (lambda (version+procs)
                        (let ((name   (car version+procs))
                              (number (cadr version+procs))
                              (procs  (caddr version+procs)))
                          `(,name ,number
                                  ,@(map (lambda (proc)
                                           (let ((name   (car proc))
                                                 (number (cadr proc))
                                                 (ret    (caddr proc))
                                                 (args   (cadddr proc)))
                                             `(,name ,number
                                                     ,(list 'unquote ret)
                                                     ,(map (lambda (arg)
                                                             (list 'unquote arg))
                                                           args))))
                                         procs))))
                      versions)))

       (make-rpc-program ,program-number
          (filter-map (lambda (version+procs)
                        (let* ((name    (car version+procs))
                               (version (assoc name %program-versions)))
                          (and version
                               (let ((number     (car (cdr version)))
                                     (user-procs (cdr version+procs)))
                                 (make-rpc-program-version number
                                                           ,proc-code)))))
                      versions+procs))))))


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

    ;; Program specifications may need to see type and constant definitions.
    ;; Thus, we can't just expect the caller to filter out all
    ;; `define-constant' and `define-type' from the input, hence the
    ;; TYPE-DEFS? and CONSTANT-DEFS? booleans, which determine whether to
    ;; include resp. type and constant definitions in the output.
    (lambda (input type-defs? constant-defs?)
      (let ((output (translator input)))
        (and (context? output)

             ;; Output constant definitions first, then type definitions, the
             ;; program/procedure definitions.
             (append (if constant-defs?
                         (reverse (map cdr (context-constants output)))
                         '())
                     (if type-defs?
                         (reverse (filter-map (lambda (name+def)
                                                (let ((name (car name+def)))
                                                  (and (not (known-type? name))
                                                       (cdr name+def))))
                                              (context-types output)))
                         '())
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
