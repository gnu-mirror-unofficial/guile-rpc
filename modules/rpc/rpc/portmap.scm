;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.  -*- coding: utf-8 -*-
;;; Copyright (C) 2007, 2008, 2010  Free Software Foundation, Inc.
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

(define-module (rpc rpc portmap)
  :use-module (rpc rpc)
  :use-module (rpc xdr)
  :use-module (rpc xdr types)

  :autoload   (ice-9 rdelim)        (read-line)
  :autoload   (ice-9 regex)         (make-regexp)
  :autoload   (srfi srfi-1)         (find)

  :export (%portmapper-port %portmapper-version-number
           %portmapper-program-number

           portmapper-null portmapper-set portmapper-unset
           portmapper-get-port portmapper-dump portmapper-call-it

           read-rpc-service-list lookup-rpc-service-name
           lookup-rpc-service-number))

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; This module provides a client interface to the portmapper RPC program, as
;;; defined in Section 3 of RFC 1833.
;;;
;;; Code:


;;;
;;; Protocol constants.
;;;

(define %portmapper-port           111)
(define %portmapper-program-number 100000)
(define %portmapper-version-number 2)

(define %portmapper-null-proc-number      0)
(define %portmapper-set-proc-number       1)
(define %portmapper-unset-proc-number     2)
(define %portmapper-get-port-proc-number  3)
(define %portmapper-dump-proc-number      4)
(define %portmapper-call-it-proc-number   5)


;;;
;;; Data types.
;;;

(define pmap-struct-type
  (make-xdr-struct-type (list xdr-unsigned-integer    ;; pm_prog
                              xdr-unsigned-integer    ;; pm_vers
                              xdr-unsigned-integer    ;; pm_prot
                              xdr-unsigned-integer))) ;; pm_port

;; `pmap' list is defined as:
;;
;;   typedef union switch (bool_t) {
;;
;;        case TRUE: struct {
;;                struct pmap;
;;                pmaplist_t foo;
;;        };
;;
;;        case FALSE: struct {};
;;   } pmaplist_t;
;;
;; We create this self-referencing type using our so-called "deferred types",
;; i.e., a thunk that returns the self-reference.

(define pmap-list-type
  (letrec ((u (make-xdr-union-type xdr-boolean
                                   `((TRUE  . ,(make-xdr-struct-type
                                                (list pmap-struct-type
                                                      (lambda () u))))
                                     (FALSE . ,xdr-void))
                                   #f)))
    u))

(define call-result-type
  (make-xdr-struct-type (list xdr-unsigned-integer                ;; port
                              xdr-variable-length-opaque-array))) ;; result

(define call-args-type
  (make-xdr-struct-type (list xdr-unsigned-integer                ;; pm_prog
                              xdr-unsigned-integer                ;; pm_vers
                              xdr-unsigned-integer                ;; pm_prot
                              xdr-variable-length-opaque-array))) ;; args


;;;
;;; Client side.
;;;

(define portmapper-null
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-null-proc-number
                             xdr-void xdr-void))

(define portmapper-set
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-set-proc-number
                             pmap-struct-type xdr-boolean))

(define portmapper-unset
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-unset-proc-number
                             pmap-struct-type xdr-boolean))

(define portmapper-get-port
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-get-port-proc-number
                             pmap-struct-type xdr-unsigned-integer))

(define portmapper-dump
  (let ((call   (make-synchronous-rpc-call %portmapper-program-number
                                           %portmapper-version-number
                                           %portmapper-dump-proc-number
                                           xdr-void pmap-list-type))
        (->list (lambda (result)
                  ;; Turn a decoded `pmap-list-type' representation into a
                  ;; Schemey list.
                  (let loop ((input result)
                             (output '()))
                    (if (eq? (car input) 'TRUE)
                        (loop (car (cddr input))
                              (cons (cadr input) output))
                        (reverse output))))))
    (lambda (args xid endpoint)
      (->list (call args xid endpoint)))))

(define portmapper-call-it
  (make-synchronous-rpc-call %portmapper-program-number
                             %portmapper-version-number
                             %portmapper-call-it-proc-number
                             call-args-type call-result-type))



;;;
;;; Reading `/etc/rpc'.
;;;

(define (read-rpc-service-list port)
  "Return a list of name-program pairs read from @var{port} (e.g., the
@file{/etc/rpc} file), showing the connection between an RPC program
human-readable name and its program number."
  (define comment-rx (make-regexp "^[[:blank:]]*#.*$" regexp/extended))
  (define empty-rx (make-regexp "^[[:blank:]]*$" regexp/extended))
  (define service-rx
    (make-regexp
     "^[[:blank:]]*([[:graph:]]+)[[:blank:]]+([[:digit:]]+)([[:blank:]].*)?$"
     regexp/extended))

  (let loop ((line (read-line port))
             (result '()))
    (if (eof-object? line)
        result
        (loop (read-line port)
              (cond ((or (regexp-exec comment-rx line)
                         (regexp-exec empty-rx line))
                     result)
                    ((regexp-exec service-rx line)
                     =>
                     (lambda (match)
                       (cons (cons (match:substring match 1)
                                   (string->number
                                    (match:substring match 2)))
                             result)))
                    (else
                     (error "RPC service parse error" line)))))))

(define (lookup-rpc-service-name service-list program)
  "Lookup RPC program numbered @var{program} in @var{service-list} (a list as
returned by @code{read-rpc-service-list}) and return its human-readable name."
  (let ((result (find (lambda (pair)
                        (equal? (cdr pair) program))
                      service-list)))
    (if (pair? result)
        (car result)
        #f)))

(define (lookup-rpc-service-number service-list program)
  "Lookup RPC program named @var{program} in @var{service-list} (a list as
returned by @code{read-rpc-service-list}) and return its RPC program number."
  (let ((result (find (lambda (pair)
                        (string=? (car pair) program))
                      service-list)))
    (if (pair? result)
        (cdr result)
        #f)))


;;; portmap.scm ends here

;;; arch-tag: beb01ebb-2e39-4f9e-ab3f-1e6f5ec52bea
