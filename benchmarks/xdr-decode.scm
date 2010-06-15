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
;;; XDR decoding benchmarking.
;;;

(use-modules (benchmark)
             (rpc xdr)
             (rpc xdr types)
             (srfi srfi-4)
             (rnrs bytevectors)
             (rnrs io ports))

(define vec
  (make-u8vector 12312))

(define vector-type
  (make-xdr-vector-type xdr-integer #f))

(define bv
  (make-bytevector (xdr-type-size vector-type vec)))

(xdr-encode! bv 0 vector-type vec)


(iterate 100
  (xdr-decode vector-type (open-bytevector-input-port bv)))

;;; arch-tag: 89a21d6d-7e23-4c67-a5b9-de0da4ec31d7
