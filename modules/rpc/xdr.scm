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

(define-module (rpc xdr)
  :autoload   (srfi srfi-1) (find)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-11)
  :use-module (r6rs bytevector)

  :export (xdr-basic-type xdr-basic-type?
           xdr-basic-type-name xdr-basic-type-size
           xdr-basic-type-encoder xdr-basic-type-decoder

           xdr-vector-type xdr-vector-type?
           xdr-vector-base-type

           xdr-struct-type xdr-struct-type?
           xdr-struct-base-types

           xdr-type-size xdr-encode! xdr-decode

           xdr-fixed-length-opaque-array
           xdr-variable-length-opaque-array
           xdr-enumeration
           xdr-integer
           xdr-unsigned-integer
           xdr-hyper-integer xdr-unsigned-hyper-integer
           xdr-double))

;;; Author: Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;; Commentary:
;;;
;;; A partial implementation of RFC 4506.
;;;
;;; Code:



(define-record-type <xdr-basic-type>
  ;; Basic fixed-size XDR types.
  (xdr-basic-type name size type-pred encoder decoder)
  xdr-basic-type?
  (name         xdr-basic-type-name)
  (size         xdr-basic-type-size)         ;; encoded size (in octets)
  (type-pred    xdr-basic-type-type-pred)    ;; input type predicate
  (encoder      xdr-basic-type-encoder)
  (decoder      xdr-basic-type-decoder))

(define-record-type <xdr-vector-type>
  ;; Variable-length arrays (Section 4.13).
  (xdr-vector-type base-type)
  xdr-vector-type?
  (base-type xdr-vector-base-type))

(define-record-type <xdr-struct-type>
  ;; Fixed-length arrays and structures (Sections 4.12 and 4.14).
  (%xdr-struct-type base-types size)
  xdr-struct-type?
  (base-types   xdr-struct-base-types) ;; list of base types
  (size         xdr-struct-size))      ;; only if fixed-length



;;;
;;; Type size.
;;;

(define (xdr-struct-type base-types)
  "Return a new XDR struct type and pre-compute its size if possible."
  (let loop ((types base-types)
             (size  0))
    (if (null? types)
        (%xdr-struct-type base-types size)
        (let ((type (car types)))
          (cond ((xdr-basic-type? type)
                 (loop (cdr types)
                       (+ size (xdr-basic-type-size type))))
                ((xdr-struct-type? type)
                 (let ((s (xdr-struct-size type)))
                   (if (number? s)
                       (loop (cdr types)
                             (+ size s))
                       (%xdr-struct-type base-types #f))))
                ((xdr-vector-type? type)
                 (%xdr-struct-type base-types #f)))))))

(define (xdr-type-size type value)
  "Return the size (in octets) of @var{type} when applied to @var{value}."
  (define (vector-map proc v)
    (let ((len (vector-length v)))
      (let loop ((i 0)
                 (result '()))
        (if (>= i len)
            result
            (loop (+ 1 i)
                  (cons (proc (vector-ref v i))
                        result))))))

  (let loop ((type type)
             (value value))
    (cond ((xdr-basic-type? type)
           (xdr-basic-type-size type))
          ((xdr-vector-type? type)
           (let ((base (xdr-vector-base-type type)))
             (apply + 4 ;; 4 octets to encode the length
                    (vector-map (lambda (value)
                                  (loop base value))
                                value))))
          ((xdr-struct-type? type)
           (or (xdr-struct-size type)
               (let ((types (xdr-struct-base-types type)))
                 (apply + (map loop types value)))))
          (else
           (error "unhandled type" type)))))


;;;
;;; Encoding.
;;;

(define (xdr-encode! bv index type value)
  "Encode @var{value}, using XDR type @var{type}, into bytevector @var{bv} at
@var{index}."

  (define (type-error value)
    (error "type error while XDR-encoding" value))

  (let loop ((type  type)
             (value value)
             (index index))
    (cond ((xdr-basic-type? type)
           (let ((valid?  (xdr-basic-type-type-pred type))
                 (encode! (xdr-basic-type-encoder type)))
             (if (not (valid? value)) (type-error value))
             (encode! type value bv index)
             (+ index (xdr-basic-type-size type))))
          ((xdr-vector-type? type)
           (let ((base (xdr-vector-base-type type))
                 (len  (vector-length value)))
             ;; encode the vector length.
             (bytevector-u32-set! bv index len %xdr-endianness)

             (let liip ((value-index 0)
                        (index       (+ 4 index)))
               (if (< value-index len)
                   (liip (+ 1 value-index)
                         (loop base
                               (vector-ref value value-index)
                               index))
                   index))))
          ((xdr-struct-type? type)
           (let liip ((types  (xdr-struct-base-types type))
                      (values value)
                      (index  index))
             (if (null? values)
                 index
                 (liip (cdr types)
                       (cdr values)
                       (loop (car types)
                             (car values)
                             index)))))
          (else
           (error "unhandled type" type)))))

;   (let* ((bv (make-bytevector (xdr-type-size type value)))
;          (result (do-encode! bv)))
;     (if (not (= result (bytevector-length bv)))
;         (error "blurps" (cons result (vector-length bv))))
;     bv))


;;;
;;; Decoding.
;;;

(define (xdr-decode bv index type)
  "Decode from @var{bv} at index @var{index} a value of XDR type @var{type}."
  (let loop ((type  type)
             (index index))
    (cond ((xdr-basic-type? type)
           (let ((decode (xdr-basic-type-decoder type)))
             (values (decode type bv index)
                     (+ index (xdr-basic-type-size type)))))
          ((xdr-vector-type? type)
           (let* ((type (xdr-vector-base-type type))
                  (len  (bytevector-u32-ref bv index %xdr-endianness))
                  (vec  (make-vector len)))
             (let liip ((index       (+ index 4))
                        (value-index 0))
               (if (< value-index len)
                   (let-values (((value index)
                                 (loop type index)))
                     (vector-set! vec value-index value)
                     (liip index (+ 1 value-index)))
                   (values vec index)))))
          ((xdr-struct-type? type)
           (let liip ((types  (xdr-struct-base-types type))
                      (result '())
                      (index  index))
             (if (null? types)
                 (values (reverse! result) index)
                 (let-values (((value index)
                               (loop (car types) index)))
                   (liip (cdr types)
                         (cons value result)
                         index)))))
          (else
           (error "unhandled type" type)))))


;;;
;;; Actual base types.
;;;

(define %xdr-endianness
  ;; Section 3.
  (endianness big))

(define xdr-integer
  ;; Section 4.1.
  (xdr-basic-type 'int32 4
                  (lambda (value)
                    (and (integer? value)
                         (>= -2147483648)
                         (< 2147483648)))
                  (lambda (type value bv index)
                    (bytevector-s32-set! bv index value %xdr-endianness))
                  (lambda (type bv index)
                    (bytevector-s32-ref bv index %xdr-endianness))))

(define xdr-unsigned-integer
  ;; Section 4.2.
  (xdr-basic-type 'uint32 4
                  (lambda (value)
                    (and (integer? value)
                         (>= value 0)
                         (<= value 4294967296)))
                  (lambda (type value bv index)
                    (bytevector-u32-set! bv index value %xdr-endianness))
                  (lambda (type bv index)
                    (bytevector-u32-ref bv index %xdr-endianness))))

(define (xdr-enumeration name enum-alist)
  ;; Section 4.3.
  (xdr-basic-type (symbol-append 'enum- name) 4
                  (lambda (value)
                    (and (symbol? value)
                         (assq value enum-alist)))
                  (lambda (type value bv index)
                    (let ((value (cdr (assq value enum-alist))))
                      (bytevector-u32-set! bv index value
                                           %xdr-endianness)))
                  (lambda (type bv index)
                    (let* ((value
                            (bytevector-u32-ref bv index
                                                %xdr-endianness))
                           (sym (find (lambda (pair)
                                        (= value (cdr pair)))
                                      enum-alist)))
                      (if (not sym)
                          (error "received an invalid enumeration"
                                 (list value name)))
                      (car sym)))))

(define xdr-hyper-integer
  ;; Section 4.5.
  (xdr-basic-type 'int64 8
                  integer?
                  (lambda (type value bv index)
                    (bytevector-s64-set! bv index value %xdr-endianness))
                  (lambda (type bv index)
                    (bytevector-s64-ref  bv index %xdr-endianness))))

(define xdr-unsigned-hyper-integer
  ;; Section 4.5.
  (xdr-basic-type 'uint64 8
                  (lambda (value)
                    (and (integer? value)
                         (>= value 0)))
                  (lambda (type value bv index)
                    (bytevector-u64-set! bv index value %xdr-endianness))
                  (lambda (type bv index)
                    (bytevector-u64-ref  bv index %xdr-endianness))))

(define xdr-double
  ;; Double-precision floating point (Section 4.7).
  (xdr-basic-type 'double 8
                  (lambda (value)
                    (and (number? value)
                         (inexact? value)))
                  (lambda (type value bv index)
                    (bytevector-ieee-double-set! bv index value
                                                 %xdr-endianness))
                  (lambda (type bv index)
                    (bytevector-ieee-double-ref bv index
                                                %xdr-endianness))))

(define xdr-single-opaque
  ;; The XDR `opaque' type cannot be used on its own.
  (xdr-basic-type 'opaque 1
                  (lambda (value)
                    (and (integer? value)
                         (>= value 0)
                         (<= value 255)))
                  (lambda (type value bv index)
                    (bytevector-u8-set! bv index value))
                  (lambda (type bv index)
                    (bytevector-u8-ref bv index))))

(define (xdr-fixed-length-opaque-array size)
  ;; Section 4.9.
  (xdr-struct-type (make-list size xdr-single-opaque)))

(define xdr-variable-length-opaque-array
  ;; Section 4.10.
  (xdr-vector-type xdr-single-opaque))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; xdr.scm ends here
