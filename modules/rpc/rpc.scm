;;; Guile-RPC --- Implementation of R6RS standard libraries.
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

(define-module (rpc rpc)
  :use-module (rpc xdr)
  :use-module (rpc rpc types)
  :use-module (r6rs bytevector)
  :use-module (r6rs i/o ports)
  :autoload   (rpc rpc transports) (tcp-send-rpc-message
                                    tcp-skip-record-header)
  :export (;; messaging
           make-rpc-message

           assert-accepted-reply

           ;; high-level
           make-synchronous-rpc-call ))

;;; Commentary:
;;;
;;; An implementation of ONC RPC (RFC 1831).
;;;
;;; Code:


;;;
;;; Creating RPC messages (Section 8).
;;;

(define %dummy-opaque-auth
  ;; An `rpc-opaque-auth' datum.
  (list 'AUTH_NONE '#()))

(define (make-rpc-call-message program version procedure)
  ;; Return an `rpc-call-body' datum.
  (list 2 program version procedure
        %dummy-opaque-auth %dummy-opaque-auth))

(define (make-rpc-mismatch-info-message low-version high-version)
  ;; Return an `rpc-mismatch-info' datum.
  (list low-version high-version))

(define (make-rpc-accepted-reply-message accept-stat . args)
  ;; Return an `rpc-accepted-reply' datum.
  (list %dummy-opaque-auth
        (cons accept-stat
              (case accept-stat
                ((PROG_MISMATCH)
                 (cons 'PROG_MISMATCH
                       (apply make-rpc-mismatch-info-message args)))
                ((SUCCESS PROG_UNAVAIL PROC_UNAVAIL GARBAGE_ARGS SYSTEM_ERR)
                 'void)
                (else
                 (error "invalid accept-stat" accept-stat))))))

(define (make-rpc-reply-message xid reply-stat . args)
  ;; Return an `rpc-reply-body' datum.
  (let ((body (case reply-stat
                ((MSG_ACCEPTED)
                 (apply make-rpc-accepted-reply-message args))
                ((MSG_DENIED)
                 (apply make-rpc-denied-reply-message args))
                (else
                 (error "invalid reply-stat" reply-stat)))))
    (cons reply-stat body)))

(define (make-rpc-message xid type . args)
  "Return an @code{rpc-message} datum.  @var{type} should be either
@code{CALL} or @code{REPLY} (the two values of the @code{rpc-message-type}
enumeration)."
  (let ((body
         (case type
           ((CALL)
            (apply make-rpc-call-message args))
           ((REPLY)
            (apply make-rpc-reply-message xid args)))))
    (list xid ;; the xid
          (cons type ;; the discriminant
                body))))


;;;
;;; Decoding RPC messages.
;;;

(define (assert-accepted-reply rpc-msg xid)
  "Return true if @var{rpc-msg} (an RPC message as returned by a previous
@code{(xdr-decode rpc-message port)} call) is a valid reply for the
invocation labeled with transaction ID @var{xid} indicating that the it was
accepted.  If @var{xid} is @code{#t}, any reply transaction ID is accepted
and it is returned (provided the rest of the message denotes an accepted
message).  On failure, an appropriate error condition is raised."
  (let* ((reply-xid (car rpc-msg))
         (reply-msg (cadr rpc-msg)))
    (if (or (eq? xid #t) (equal? xid reply-xid))
        (if (eq? (car reply-msg) 'REPLY)
            (if (eq? (cadr reply-msg) 'MSG_ACCEPTED)
                ;; XXX: skip the opaque auth
                (if (eq? (car (cadddr reply-msg)) 'SUCCESS)
                    reply-xid
                    (error "accepted message failed" reply-msg))
                (error "message denied" reply-msg))
            (error "invalid reply message" reply-msg))
        (error "invalid reply xid" rpc-msg))))


;;;
;;; Client-side helpers.
;;;

(define (make-synchronous-rpc-call program version procedure
                                   arg-type result-type)
  "Return a procedure that may be applied to an argument list, transaction
ID, and I/O port, to make a synchronous RPC call to the remote procedure
numbered @var{procedure} in @var{program}, version @var{version}.  On
success, the invocation result is eventually returned.  Otherwise, an error
condition is raised."
  (lambda (args xid endpoint)
    (let* ((call-msg     (make-rpc-message xid 'CALL program version
                                           procedure))
           (call-msg-len (xdr-type-size rpc-message call-msg))
           (args-msg-len (xdr-type-size arg-type args))
           (msg-len      (+ call-msg-len args-msg-len))
           (msg          (make-bytevector msg-len)))

      (xdr-encode! msg 0 rpc-message call-msg)
      (xdr-encode! msg call-msg-len arg-type args)
      (tcp-send-rpc-message endpoint msg 0 msg-len)
      (force-output endpoint)

      (format #t "request sent (~a = ~a + ~a)!~%" msg-len
              call-msg-len args-msg-len)

      ;; Wait for an answer
      (tcp-skip-record-header endpoint)

      (let* ((reply-msg (xdr-decode rpc-message endpoint)))
        (and (assert-accepted-reply reply-msg xid)
             (xdr-decode result-type endpoint))))))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; rpc.scm ends here
