;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.
;;; Copyright (C) 2007, 2010  Free Software Foundation, Inc.
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

;;;
;;; Record-marking benchmarking.
;;;

(use-modules (benchmark)
             (rnrs bytevectors)
             (rnrs io ports)
             (srfi srfi-11)
             (rpc rpc transports))

(define (make-record-marked-bytevector)
  (let-values (((port get-content)
                (open-bytevector-output-port)))
    (let ((source      (make-bytevector 4))
          (send-record (make-rpc-record-sender 103)))
      (let loop ((i 500000))
        (if (<= i 0)
            (get-content)
            (begin
              (send-record port source 0 (bytevector-length source))
              (loop (- i 1))))))))

(define input
  (open-bytevector-input-port (make-record-marked-bytevector)))


(iterate 30
  (get-bytevector-all (rpc-record-marking-input-port input)))

;;; arch-tag: b674b0e9-b5f6-4ce1-9125-d1473804f5d3
