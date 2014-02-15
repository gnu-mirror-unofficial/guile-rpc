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
           &compiler-duplicate-identifier-error
           compiler-unknown-type-error? compiler-unknown-constant-error?
           compiler-duplicate-identifier-error?
           compiler-unknown-type-error:type-name
           compiler-unknown-constant-error:constant-name
           compiler-duplicate-identifier-error:name
           compiler-duplicate-identifier-error:previous-location))

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

(define-condition-type &compiler-duplicate-identifier-error &compiler-error
  compiler-duplicate-identifier-error?
  (name               compiler-duplicate-identifier-error:name)
  (previous-location  compiler-duplicate-identifier-error:previous-location))


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

(define (cons-constant name value loc context)
  (if (free-identifier? name context)
      (make-context (context-types context)
                    (context-forward-type-refs context)
                    (alist-cons name (list value loc)
                                (context-constants context))
                    (context-programs context))
      (raise (condition (&compiler-duplicate-identifier-error
                         (location          loc)
                         (name              name)
                         (previous-location (identifier-location name
                                                                 context)))))))

(define (cons-type name value override? loc context)
  ;; Return a new context based on CONTEXT with the addition of type VALUE
  ;; named NAME.
  (if (or override? (free-identifier? name context))
      (make-context (alist-cons name (list value loc)
                                (context-types context))
                    (context-forward-type-refs context)
                    (context-constants context)
                    (context-programs context))
      (raise (condition (&compiler-duplicate-identifier-error
                         (location          loc)
                         (name              name)
                         (previous-location (identifier-location name
                                                                 context)))))))

(define (resolve-forward-type-refs name make-type-def context)
  ;; Return a context where pending forward references to NAME are resolved
  ;; and added to CONTEXT.
  (let* ((refs (context-forward-type-refs context))
         (fwd  (assoc name refs))
         (c    (make-context (context-types context)
                             (if (pair? fwd)
                                 (alist-delete name refs string=?)
                                 refs)
                             (context-constants context)
                             (context-programs context))))
    (if (pair? fwd)
        (fold (lambda (name+tag+proc c)
                ;; Resolve the forward reference.
                (let ((name (car   name+tag+proc))
                      (tag  (cadr  name+tag+proc))
                      (proc (caddr name+tag+proc)))
                  (cond ((eq? tag 'type)
                         (cons-type name (make-type-def name (proc c))
                                    #f
                                    #f ;; FIXME: no location info
                                    c))
                        ((eq? tag 'program)
                         (proc c))
                        (else
                         (error "internal error: unsupported forward-ref tag"
                                tag)))))
              c
              (cdr fwd))
        c)))

(define (cons-forward-type-ref name tag from proc context)
  ;; Add to CONTEXT a forward reference to type NAME, which is referred to by
  ;; FROM; if TAG is `type', then FROM is a type, if TAG is `program' then
  ;; FROM is a program.  PROC will be invoked and passed a context containing
  ;; the definition of NAME when it is available; it should return a
  ;; "type-ref", i.e., something returned by the back-end's `make-type-ref'.

  ;; For `resolve-forward-type-refs' to be efficient, we store forward type
  ;; refs in a double alist of the following form:
  ;;
  ;;   (("DEP1" . ("OBJ1" TAG <PROC>) ("OBJ2" TAG <PROC>))
  ;;    ("DEP2" . ("OBJ3" TAG <PROC>) ("OBJ4" TAG <PROC>)))
  ;;
  ;; `DEP1' and `DEP2' are types depended on but not yet defined (the NAME
  ;; argument); `OBJ1', etc., are types or programs depending on the given
  ;; type.  For instance, `OBJ1' and `OBJ2' both depends on `DEP1', and
  ;; whether they are programs or types is determined by their TAG.
  (make-context (context-types context)
                (let ((name+refs (assoc name
                                        (context-forward-type-refs context))))
                  (alist-cons name
                              (if (pair? name+refs)
                                  (cons (list from tag proc) (cdr name+refs))
                                  (list (list from tag proc)))
                              (alist-delete name
                                            (context-forward-type-refs
                                             context)
                                            string=?)))
                (context-constants context)
                (context-programs context)))

(define (cons-program name value loc context)
  (if (free-identifier? name context)
      (make-context (context-types context)
                    (context-forward-type-refs context)
                    (context-constants context)
                    (alist-cons name (list value loc)
                                (context-programs context)))
      (raise (condition (&compiler-duplicate-identifier-error
                         (location          loc)
                         (name              name)
                         (previous-location (identifier-location name
                                                                 context)))))))

(define (make-lookup-procedure accessor)
  (lambda (name context)
    (let ((result (assoc name (accessor context) string=?)))
      (and (pair? result)
           (cadr result)))))

(define lookup-constant
  (make-lookup-procedure context-constants))

(define lookup-type
  (make-lookup-procedure context-types))

(define lookup-program
  (make-lookup-procedure context-programs))

(define (free-identifier? name context)
  ;; Per RFC 4506 and RFC 1831, type, constant and program identifiers all
  ;; share the same name space.  This procedures returns true if NAME is
  ;; already used.

  ;; XXX: We could use a single alist for all three kinds of objects but it
  ;; would consume slightly more memory (to record the type of each object)
  ;; and this procedure would still be O(N), with N the number of currently
  ;; used identifiers.  This could be improved by using VList-based hash
  ;; lists or similar.
  (not (or (lookup-type name context)
           (lookup-constant name context)
           (lookup-program name context))))

(define (identifier-location name context)
  ;; Return the location of the definition of NAME or `#f' if it's not bound
  ;; in CONTEXT.
  (let ((name+value+loc (let loop ((accessors (list context-types
                                                    context-constants
                                                    context-programs)))
                          (if (null? accessors)
                              #f
                              (let ((get (car accessors)))
                                (or (assoc name (get context))
                                    (loop (cdr accessors))))))))
    (and (pair? name+value+loc)
         (caddr name+value+loc))))


;;;
;;; Compilation.
;;;

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

    (define (instantiate-forward-type-refs c)
      ;; Instantiate forward type references left in C and return the new
      ;; context, whose `fwdrefs' field is empty.  If all forward refs in C
      ;; are mutually referenced (e.g., type A depends on B and B depends on
      ;; A), then it will work, using thunks to denote recursive type
      ;; references.  Otherwise, `unbound-type' exceptions will be raised.
      ;;
      ;; The trick here is to resolve each type in a (temporary) context
      ;; where each of its dependencies is added as a partial definition,
      ;; i.e., a `make-type-def' whose TYPE argument is a thunk that, when
      ;; applied, returns the actual dependency.

      (define (make-resolution-context source new-context)
        ;; Return a context suitable for the resolution of
        ;; mutually-referencing types left in C.  The returned context is
        ;; based on the SOURCE context where forward refs in SOURCE are
        ;; instantiated as thunks that refer to NEW-CONTEXT.
        (fold (lambda (dep+refs c)
                (fold (lambda (name+tag+proc c)
                        (let ((name (car   name+tag+proc))
                              (tag  (cadr  name+tag+proc))
                              (proc (caddr name+tag+proc)))
                          (if (eq? tag 'type)
                              (cons-type name
                                         (make-type-def name
                                                        (lambda ()
                                                          (proc (new-context))))
                                         #f
                                         #f ;; FIXME: no location info
                                         c)
                              c)))
                      c
                      (cdr dep+refs)))
              source
              (context-forward-type-refs source)))

      (letrec
          ((new-context
            (fold (let ((c* (make-resolution-context c
                                                     (lambda () new-context))))
                    (lambda (dep+refs c)
                      (fold (lambda (name+tag+proc c)
                              (let ((name (car   name+tag+proc))
                                    (tag  (cadr  name+tag+proc))
                                    (proc (caddr name+tag+proc)))
                                (if (eq? tag 'type)
                                    (cons-type name
                                               (make-type-def name (proc c*))
                                               #f
                                               #f ;; FIXME: no location info
                                               c)
                                    ;; requeue the forward type ref (a program)
                                    (cons-forward-type-ref (car dep+refs)
                                                           tag name proc
                                                           c))))
                            c
                            (cdr dep+refs))))
                  (make-context (context-types c)
                                '() ;; no forward references
                                (context-constants c)
                                (context-programs c))
                  (context-forward-type-refs c))))
        new-context))

    (define (instantiate-remaining-forward-type-refs c)
      ;; Instantiate forward type refs from C in a simple way, which assumes
      ;; that they are not mutually recursive.  This is the case when the
      ;; only remaining forward refs are programs.
      (fold (lambda (dep+refs c)
              (resolve-forward-type-refs (car dep+refs) make-type-def
                                         c))
            c
            (context-forward-type-refs c)))

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
                                    (sexp-location expr)
                                    c)))
                  ((define-type)
                   (let ((name (cadr expr))
                         (spec (caddr expr)))
                     (define (make-type c)
                       ;; Construct the type specified by SPEC in a context
                       ;; where NAME is bound to a "self-referencing thunk".
                       (letrec ((type
                                 (type-ref spec
                                           (cons-type name
                                                      (make-type-def
                                                       name
                                                       (lambda () type))
                                                      #t ;; override existing def
                                                      (sexp-location expr)
                                                      c)
                                           name (sexp-location expr))))
                         type))

                     ;; Catch any unbound type (perhaps a forward reference)
                     ;; so we can try again later.
                     (guard (e ((compiler-unknown-type-error? e)
                                (let ((missing
                                       (compiler-unknown-type-error:type-name
                                        e)))
                                  (cons-forward-type-ref missing 'type name
                                                         make-type
                                                         c))))
                       (let ((c (cons-type name
                                           (make-type-def name (make-type c))
                                           #f
                                           (sexp-location expr)
                                           c)))
                         (resolve-forward-type-refs name make-type-def c)))))
                  ((define-program)
                   ;; FIXME: Programs are allowed to refer to types not yet
                   ;; defined, thus we should also use the forward-ref trick
                   ;; here.
                   (let ((name (cadr expr)))
                     (define (make-program c)
                       (cons-program name
                                     (make-program-definition expr c)
                                     (sexp-location expr)
                                     c))

                     (guard (e ((compiler-unknown-type-error? e)
                                (let ((missing
                                       (compiler-unknown-type-error:type-name
                                        e)))
                                  (cons-forward-type-ref missing 'program
                                                         name make-program
                                                         c))))
                       (make-program c))))
                  (else
                   (error "unrecognized expression" expr))))
              initial-context
              input))

      ;; Process the initial context, then instantiate remaining types
      ;; containing forward type references (mutual references), and finally
      ;; instantiate programs that contained forward type references.
      (let* ((stage1 (iterate initial-context))
             (stage2 (instantiate-forward-type-refs stage1))
             (stage3 (instantiate-remaining-forward-type-refs stage2)))
        (if (null? (context-forward-type-refs stage3))
            stage3
            (error "internal error: there shouldn't be any forward ref left"
                   stage3))))))


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

  `(define ,type-name
     ,typespec))

(define (type-ref-code def)
  ;; Return a reference to the type defined by DEF.  DEF is an sexp of the
  ;; form `(define foo ...)'
  (define definition-name cadr)

  (let ((def-body (caddr def)))
    ;; If the definition body is a thunk, then it's a forward reference.
    (if (procedure? def-body)
        `(lambda () ,(definition-name def))
        (definition-name def))))

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
                               ,(cond ((null? arg-types)
                                       'xdr-void)
                                      ((null? (cdr arg-types))
                                       (car arg-types))
                                      (else
                                       `(make-xdr-struct-type
                                         (list ,@arg-types))))
                               ,ret-type))))
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
                                (make-rpc-procedure number
                                                    (cond ((null? arg-types)
                                                           xdr-void)
                                                          ((null? (cdr arg-types))
                                                           (car arg-types))
                                                          (else
                                                           (apply
                                                            make-xdr-struct-type
                                                            arg-types)))
                                                    ret-type
                                                    handler)))))
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
          ;; The initial compilation context.  The `#f's denote source
          ;; location information (lack thereof).
          (make-context
           '(("void"             (define xdr-void ...) #f)
             ("opaque"           (define xdr-opaque ...) #f) ;; pseudo-type
             ("bool"             (define xdr-boolean ...) #f)
             ("int"              (define xdr-integer ...) #f)
             ("unsigned int"     (define xdr-unsigned-integer ...) #f)
             ("hyper"            (define xdr-hyper-integer ...) #f)
             ("unsigned hyper"   (define xdr-unsigned-hyper-integer ...) #f)
             ("float"            (define xdr-float ...) #f)
             ("double"           (define xdr-double ...) #f)
             ;; FIXME: We lack support for `quadruple'.
             ("string"           (define xdr-string ...) #f)
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
                         (reverse (map cadr (context-constants output)))
                         '())
                     (if type-defs?
                         (reverse (filter-map (lambda (name+def)
                                                (let ((name (car name+def)))
                                                  (and (not (known-type? name))
                                                       (cadr name+def))))
                                              (context-types output)))
                         '())
                     (reverse (concatenate
                               (map cadr (context-programs output))))))))))

(define rpc-language->scheme-client
  (make-rpc-language->scheme rpc-program-code/client))

(define rpc-language->scheme-server
  (make-rpc-language->scheme rpc-program-code/server))



;;;
;;; Run-time type generation back-end.
;;;

(define (->schemey-enum-alist values)
  (map (lambda (n+v)
         (cons (enum-value-name->symbol (car n+v))
               (cdr n+v)))
       values))

(define (->schemey-union-values-alist values)
  (map (lambda (v+t)
         (let ((value (car v+t)))
           (cons (if (number? value)
                     value
                     (enum-value-name->symbol value))
                 (cdr v+t))))
       values))

(define rpc-language->xdr-types
  ;; To handle recursive types, the strategy here is to represent types using
  ;; one-argument procedures that are to be invoked at type-definition time
  ;; and passed a "recursive-reference thunk".  A recursive-reference thunk
  ;; (named by analogy with `letrec') is a nullary procedure that, when
  ;; applied, returns a circular reference, e.g., a reference to the type
  ;; being defined or a reference to a type dependent on the type being
  ;; defined.
  (let* ((initial-context
          ;; The initial compilation context.  The `#f's denote source
          ;; location information (lack thereof).
          (make-context
           `(("void"             ,xdr-void #f)
             ("opaque"           opaque #f) ;; pseudo-type
             ("bool"             ,xdr-boolean #f)
             ("int"              ,xdr-integer #f)
             ("unsigned int"     ,xdr-unsigned-integer #f)
             ("hyper"            ,xdr-hyper-integer #f)
             ("unsigned hyper"   ,xdr-unsigned-hyper-integer #f)
             ("float"            ,xdr-float #f)
             ("double"           ,xdr-double #f)
             ;; FIXME: We lack support for `quadruple'.
             )
           '()
           '()
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
         (struct                (lambda (types)
                                  (make-xdr-struct-type types)))
         (union                 (lambda (discr values default)
                                  (let ((values (->schemey-union-values-alist
                                                 values)))
                                    (make-xdr-union-type discr
                                                         values
                                                         default))))
         (string                (lambda (max-length)
                                  (make-xdr-string max-length)))
         (fixed-length-array    (lambda (type length)
                                  (if (eq? type 'opaque)
                                      (make-xdr-fixed-length-opaque-array
                                       length)
                                      (make-xdr-struct-type
                                       (make-list length type)))))
         (variable-length-array (lambda (type limit)
                                  (if (eq? type 'opaque)
                                      (make-xdr-variable-length-opaque-array
                                       limit)
                                      (make-xdr-vector-type type limit))))

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
                         (let ((name (car name+type))
                               (type (cadr name+type)))
                           (and (not (known-type? name))
                                type
                                (cons name type))))
                       (context-types output)))))))

;;; compiler.scm ends here
