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

(define-module (rpc rpc transports)
  :use-module (r6rs bytevector)
  :use-module (r6rs i/o ports)
  :use-module (srfi srfi-60)
  :export (tcp-send-rpc-message
           tcp-skip-record-header))


;;;
;;; TCP transport and record marking (RFC 1831, Section 10).
;;;


(define %record-header-endianness (endianness big))

(define (tcp-send-rpc-message port bv offset len)
  "Send the RPC message of @var{len} octets encoded at offset @var{offset} in
@var{bv} (a bytevector) to @var{port}."
  (define %max-record-length
    (- (expt 2 31) 1))

  (let* ((fragment-count (+ 1 (quotient len %max-record-length)))
         (last-fragment  (- fragment-count 1))
         (record-header (make-bytevector 4)))
    (let loop ((fragment 0))
      (if (<= fragment last-fragment)
          (let* ((start (* fragment %max-record-length))
                 (count (min %max-record-length (- len start))))
            (bytevector-u32-set! record-header 0
                                 (if (= fragment last-fragment)
                                     (bitwise-ior count #x80000000)
                                     count)
                                 %record-header-endianness)
            (put-bytevector port record-header)
            (put-bytevector port bv (+ offset start) count)
            (loop (+ 1 fragment)))))))


;; FIXME: In order to implement this, we'd need to provide a custom binary
;; input port type so that XDR decoders can keep using binary input
;; procedures without being aware of fragmentation issues.

(define (tcp-skip-record-header port)
  (let* ((raw    (get-bytevector-n port 4))
         (header (bytevector-u32-ref raw 0 %record-header-endianness)))
    (if (bit-set? 31 header)
        #t ;; last record fragment
        (error "reading fragmented messages is not handled yet" raw))))


;;; arch-tag: 21cfcc4a-a169-4472-a446-9a2230da9dcc
