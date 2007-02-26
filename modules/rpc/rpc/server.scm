;;; Guile-RPC --- A Scheme implementation of ONC RPC.
;;; Copyright (C) 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;; Guile-RPC is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; Guile-RPC is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Guile-RPC; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(define-module (rpc rpc server)
  :use-module (rpc rpc)
  :autoload   (rpc rpc types) (rpc-message)
  :autoload   (rpc xdr)       (xdr-type-size xdr-decode)
  :autoload   (srfi srfi-1)   (find)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-34)
  :use-module (srfi srfi-35)
  :use-module (r6rs bytevector)

  :export (procedure-call-information
           rpc-call-xid rpc-call-program rpc-call-version
           rpc-call-procedure rpc-call-credentials rpc-call-verifier

           make-rpc-program make-rpc-program-version make-rpc-procedure
           rpc-program-number rpc-program-versions rpc-program-lookup-version
           rpc-program-version-number rpc-program-version-procedures
           rpc-program-version-lookup-procedure
           rpc-procedure-number rpc-procedure-handler
           rpc-procedure-argument-xdr-type rpc-procedure-result-xdr-type

           lookup-called-procedure handle-procedure-call
           handle-procedure-lookup-error

           ;; error conditions
           &rpc-server-error rpc-server-error?
           &onc-rpc-version-mismatch-error onc-rpc-version-mismatch-error?
           onc-rpc-version-mismatch-error:peer-version
           &rpc-invalid-call-message-error rpc-invalid-call-message-error?
           rpc-invalid-call-message-error:message

           &rpc-procedure-lookup-error rpc-procedure-lookup-error?
           &rpc-invalid-program-error &rpc-invalid-version-error
           &rpc-invalid-procedure-error
           rpc-invalid-program-error? rpc-invalid-version-error?
           rpc-invalid-procedure-error?
           rpc-invalid-program-error:program
           rpc-invalid-version-error:program
           rpc-invalid-version-error:version
           rpc-invalid-procedure-error:procedure
           rpc-invalid-procedure-error:version
           rpc-invalid-procedure-error:program))

;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Facilities to handle RPC call messages on the server-side.
;;;
;;; Code:


;;;
;;; Error conditions.
;;;

(define-condition-type &rpc-server-error &rpc-error
  rpc-server-error?)

(define-condition-type &onc-rpc-version-mismatch-error &rpc-server-error
  onc-rpc-version-mismatch-error?
  (peer-version  onc-rpc-version-mismatch:peer-version))

(define-condition-type &rpc-invalid-call-message-error &rpc-server-error
  rpc-invalid-call-message-error?
  (message rpc-invalid-call-message-error:message))


;; Procedure lookup errors.

(define-condition-type &rpc-procedure-lookup-error &rpc-server-error
  rpc-procedure-lookup-error?)

(define-condition-type &rpc-invalid-program-error
                       &rpc-procedure-lookup-error
  rpc-invalid-program-error?
  (program  rpc-invalid-program-error:program))

(define-condition-type &rpc-invalid-version-error
                       &rpc-procedure-lookup-error
  rpc-invalid-version-error?
  (version  rpc-invalid-version-error:version)
  (program  rpc-invalid-version-error:program))

(define-condition-type &rpc-invalid-procedure-error
                       &rpc-procedure-lookup-error
  rpc-invalid-program-error?
  (procedure  rpc-invalid-procedure-error:procedure)
  (program    rpc-invalid-procedure-error:program)
  (version    rpc-invalid-procedure-error:version))



;;;
;;; Procedure call message processing.
;;;

(define-record-type <rpc-call>
  (%make-rpc-call xid program version procedure credentials verifier)
  rpc-call?
  (xid          rpc-call-xid)
  (program      rpc-call-program)
  (version      rpc-call-version)
  (procedure    rpc-call-procedure)
  (credentials  rpc-call-credentials)
  (verifier     rpc-call-verifier))

(define (procedure-call-information call-msg)
  "Return an @code{<rpc-call>} object that denotes the procedure call
requested in @var{call-msg}.  If @var{call-msg} is not an appropriate RPC
call message, an error condition is raised."
  (let ((xid (car call-msg))
        (call (cadr call-msg)))
    (if (eq? (car call) 'CALL)
        (let ((rpc-version (cadr call))
              (call-args (cddr call)))
          (if (equal? %onc-rpc-version rpc-version)
              (apply %make-rpc-call (cons xid call-args))
              (raise (condition (&onc-rpc-version-mismatch-error
                                 (peer-version rpc-version))))))
        (raise (condition (&rpc-invalid-call-message-error
                           (message call-msg)))))))


;;;
;;; Describing RPC "programs".
;;;

(define-record-type <rpc-program>
  (%make-rpc-program number versions lookup-version)
  rpc-program?
  (number         rpc-program-number)
  (versions       rpc-program-versions)
  (lookup-version %rpc-program-lookup-version))

(define-record-type <rpc-program-version>
  (%make-rpc-program-version number procedures lookup-procedure)
  rpc-program-version?
  (number           rpc-program-version-number)
  (procedures       rpc-program-version-procedures)
  (lookup-procedure %rpc-program-version-lookup-procedure))

(define-record-type <rpc-procedure>
  (make-rpc-procedure number argument-xdr-type result-xdr-type handler)
  rpc-procedure?
  (number             rpc-procedure-number)
  (argument-xdr-type  rpc-procedure-argument-xdr-type)
  (result-xdr-type    rpc-procedure-result-xdr-type)
  (handler            rpc-procedure-handler))


(define (compile-numbered-items get-number items)
  (let* ((numbers (map get-number items))
         (vector  (make-vector (+ 1 (apply max numbers)) #f)))
    (for-each (lambda (item number)
                (vector-set! vector number item))
              items
              numbers)
    (lambda (number)
      (if (>= number (vector-length vector))
          #f
          (vector-ref vector number)))))

(define (make-rpc-program number versions)
  "Return a new object describing the RPC program identified by @var{number}
and consisting of the versions listed in @var{versions}."
  (%make-rpc-program number versions
                     (compile-numbered-items rpc-program-version-number
                                             versions)))

(define (make-rpc-program-version number procedures)
  "Return a new object describing the RPC program version identified by
@var{number} and consisting of the procedures listed in @var{procedures}."
  (%make-rpc-program-version number procedures
                             (compile-numbered-items rpc-procedure-number
                                                     procedures)))

(define (rpc-program-lookup-version program version)
  "Lookup @var{version}, a number denoting an RPC program version, into
@var{program}.  Return an @code{<rpc-program-version>} object on success,
@code{#f} otherwise."
  ((%rpc-program-lookup-version program) version))

(define (rpc-program-version-lookup-procedure version procedure)
  "Lookup @var{procedure}, a number denoting an RPC procedure number, into
@var{version}.  Return an @code{<rpc-procedure>} object on success, @code{#f}
otherwise."
  ((%rpc-program-version-lookup-procedure version) procedure))



;;;
;;; Processing requests.
;;;

(define (lookup-called-procedure call programs)
  "Return the procedure addressed by @var{call} and available in
@var{programs} (a list of @code{<rpc-program>} objects).  If the procedure
could not be found, an error condition is raised:
 @code{&rpc-invalid-program-error}, @code{&rpc-invalid-version-error} or
 @code{&rpc-invalid-procedure-error}."
  (let* ((program-num (rpc-call-program call))
         (program (find (lambda (p)
                          (= program-num (rpc-program-number p)))
                        programs)))
    (if program
        (let* ((version-num (rpc-call-version call))
               (version (rpc-program-lookup-version program version-num)))
          (if version
              (let* ((proc-num (rpc-call-procedure call))
                     (proc (rpc-program-version-lookup-procedure version
                                                                 proc-num)))
                (if proc
                    proc
                    (raise (condition (&rpc-invalid-procedure-error
                                       (procedure proc-num)
                                       (version   version)
                                       (program   program))))))
              (raise (condition (&rpc-invalid-version-error
                                 (program   program)
                                 (version   version-num))))))
        (raise (condition (&rpc-invalid-program-error
                           (program program-num)))))))

(define (handle-procedure-lookup-error c call send-result)
  "Handle procedure lookup error condition @var{c} for @var{call} by sending
the appropriate reply message via @var{send-result}."
  (cond ((rpc-invalid-program-error? c)
         (let* ((reply      (make-rpc-message (rpc-call-xid call)
                                              'REPLY 'MSG_ACCEPTED
                                              'PROG_UNAVAIL))
                (reply-size (xdr-type-size rpc-message reply))
                (raw-reply  (make-bytevector reply-size)))
           (xdr-encode! raw-reply 0 rpc-message reply)
           (send-result raw-reply 0 reply-size)))

        ((rpc-invalid-version-error? c)
         (let* ((prog        (rpc-invalid-version-error:program c))
                (versions    (map rpc-program-version-number
                                  (rpc-program-versions prog)))
                (min-version (apply min versions))
                (max-version (apply max versions))
                (reply       (make-rpc-message (rpc-call-xid call)
                                               'REPLY 'MSG_ACCEPTED
                                               'PROG_MISMATCH
                                               min-version
                                               max-version))
                (reply-size  (xdr-type-size rpc-message reply))
                (raw-reply   (make-bytevector reply-size)))
           (xdr-encode! raw-reply 0 rpc-message reply)
           (send-result raw-reply 0 reply-size)))

        ((rpc-invalid-procedure-error? c)
         (let* ((prog        (rpc-invalid-procedure-error:program c))
                (version     (rpc-invalid-procedure-error:version c))
                (reply       (make-rpc-message (rpc-call-xid call)
                                               'REPLY 'MSG_ACCEPTED
                                               'PROC_UNAVAIL))
                (reply-size  (xdr-type-size rpc-message reply))
                (raw-reply   (make-bytevector reply-size)))
           (xdr-encode! raw-reply 0 rpc-message reply)
           (send-result raw-reply 0 reply-size)))))

(define (handle-procedure-call call programs input-port send-result)
  "Handle procedure call @var{call} using @var{programs}, reading input from
@var{input-port} and sending the result via @var{send-result}, a
three-argument procedure that is passed a bytevector, offset and octet
count."
  (guard (c ((rpc-procedure-lookup-error? c)
             (handle-procedure-call c call send-result)))

    (let* ((proc (lookup-called-procedure call programs))
           (handler        (rpc-procedure-handler proc))
           (input-type     (rpc-procedure-argument-xdr-type proc))
           (result         (handler (xdr-decode input-type input-port)))
           (result-type    (rpc-procedure-result-xdr-type proc))
           (result-size    (xdr-type-size result-type result))

           (reply-prologue (make-rpc-message (rpc-call-xid call)
                                             'REPLY 'MSG_ACCEPTED
                                             'SUCCESS))
           (prologue-size  (xdr-type-size rpc-message reply-prologue))
           (total-size     (+ result-size prologue-size))
           (raw-result     (make-bytevector total-size)))
      (xdr-encode! raw-result
                   (xdr-encode! raw-result 0
                                rpc-message reply-prologue)
                   result-type result)
      (send-result raw-result 0 total-size))))

;;; server.scm ens here

;;; arch-tag: 60ab6723-b30d-4c60-809c-f1e54c6c6ac9
