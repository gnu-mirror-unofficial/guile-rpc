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
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-11)
  :autoload   (srfi srfi-34) (raise)
  :use-module (srfi srfi-35)
  :use-module (r6rs bytevector)

  :export (make-xdr-basic-type xdr-basic-type?
           xdr-basic-type-name xdr-basic-type-size
           xdr-basic-type-encoder xdr-basic-type-decoder

           make-xdr-vector-type xdr-vector-type?
           xdr-vector-base-type

           make-xdr-struct-type xdr-struct-type?
           xdr-struct-base-types

           make-xdr-union-type xdr-union-type?
           xdr-union-discriminant-type
           xdr-union-discriminant/type-alist
           xdr-union-default-type
           xdr-union-arm-type

           xdr-type-size xdr-encode! xdr-decode
           %xdr-endianness

           &xdr-error xdr-error?
           &xdr-type-error xdr-type-error? xdr-type-error:type
           &xdr-input-type-error xdr-input-type-error?
           xdr-input-type-error:value
           &xdr-unknown-type-error xdr-unknown-type-error?
           &xdr-union-discriminant-error xdr-union-discriminant-error?))


;;; Author: Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;; Commentary:
;;;
;;; A framework to describe the data types defined in RFC 4506.
;;;
;;; Code:



;;;
;;; Major kinds of types.
;;;

(define-record-type <xdr-basic-type>
  ;; Basic fixed-size XDR types.
  (make-xdr-basic-type name size type-pred encoder decoder)
  xdr-basic-type?
  (name         xdr-basic-type-name)
  (size         xdr-basic-type-size)         ;; encoded size (in octets)
  (type-pred    xdr-basic-type-type-pred)    ;; input type predicate
  (encoder      xdr-basic-type-encoder)
  (decoder      xdr-basic-type-decoder))

(define-record-type <xdr-vector-type>
  ;; Variable-length arrays (Section 4.13).
  (make-xdr-vector-type base-type)
  xdr-vector-type?
  (base-type xdr-vector-base-type))

(define-record-type <xdr-struct-type>
  ;; Fixed-length arrays and structures (Sections 4.12 and 4.14).
  (%make-xdr-struct-type base-types size)
  xdr-struct-type?
  (base-types   xdr-struct-base-types) ;; list of base types
  (size         xdr-struct-size))      ;; only if fixed-length

(define-record-type <xdr-union-type>
  ;; Discriminated unions (Section 4.15).
  (%make-xdr-union-type discriminant-type discriminant/type-alist
                        default-type)
  xdr-union-type?
  (discriminant-type        xdr-union-discriminant-type)
  (discriminant/type-alist  xdr-union-discriminant/type-alist)
  (default-type             xdr-union-default-type))


;;;
;;; Error conditions.
;;;

(define-condition-type &xdr-error &error
  xdr-error?)

(define-condition-type &xdr-type-error &error
  xdr-type-error?
  (type      xdr-unknown-type-error:type))

(define-condition-type &xdr-unknown-type-error &xdr-type-error
  xdr-unknown-type-error?)

(define-condition-type &xdr-input-type-error &xdr-type-error
  xdr-input-type-error?
  (value     xdr-input-type-error:value))

(define-condition-type &xdr-union-discriminant-error &xdr-type-error
  xdr-union-discriminant-error?)



;;;
;;; Helpers.
;;;

(define (make-xdr-struct-type base-types)
  "Return a new XDR struct type and pre-compute its size if possible."
  (let loop ((types base-types)
             (size  0))
    (if (null? types)
        (%make-xdr-struct-type base-types size)
        (let ((type (car types)))
          (cond ((xdr-basic-type? type)
                 (loop (cdr types)
                       (+ size (xdr-basic-type-size type))))
                ((xdr-struct-type? type)
                 (let ((s (xdr-struct-size type)))
                   (if (number? s)
                       (loop (cdr types)
                             (+ size s))
                       (%make-xdr-struct-type base-types #f))))
                (else
                 ;; Cannot determine struct size statically.
                 (%make-xdr-struct-type base-types #f)))))))

(define (make-xdr-union-type discr-type discr/type-alist default-type)
  "Return a new XDR discriminated union type, using @var{discr-type} as the
discriminant type (which must be a 32-bit basic type) and
@var{discr/type-alist} to select the ``arm'' type depending on the
discriminant value.  If no suitable value is found in @var{discr/type-alist}
and @var{default-type} is not @code{#f}, then default type is used as the arm
type."
  (if (and (xdr-basic-type? discr-type)
           (= 4 (xdr-basic-type-size discr-type))
           (list? discr/type-alist))
      (%make-xdr-union-type discr-type discr/type-alist
                            default-type)
      (raise (condition
              (&xdr-union-discriminant-error (type discr-type))))))

(define (xdr-union-arm-type union discriminant)
  "Return the type that should be used for @var{union}'s arm given
 @var{discriminant} (a Scheme value)."
  (let ((arm-type (assoc discriminant
                         (xdr-union-discriminant/type-alist union))))
    (if (pair? arm-type)
        (cdr arm-type)
        (or (xdr-union-default-type union)
            (raise (condition
                    (&xdr-input-type-error (type  union)
                                           (value discriminant))))))))


;;;
;;; Type size.
;;;

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
          ((xdr-union-type? type)
           (let ((discr (car value)))
             (+ 4 (loop (xdr-union-arm-type type discr)
                        (cdr value)))))
          (else
           (raise (condition (&xdr-unknown-type-error (type type))))))))


;;;
;;; Encoding.
;;;

(define %xdr-endianness
  ;; Section 3: All items (except opaque arrays) are encoded using this
  ;; endianness.
  (endianness big))


(define (xdr-encode! bv index type value)
  "Encode @var{value}, using XDR type @var{type}, into bytevector @var{bv} at
@var{index}."

  (define (type-error type value)
    (raise (condition (&xdr-input-type-error (type  type)
                                             (value value)))))

  (let loop ((type  type)
             (value value)
             (index index))
    (cond ((xdr-basic-type? type)
           (let ((valid?  (xdr-basic-type-type-pred type))
                 (encode! (xdr-basic-type-encoder type)))
             (if (not (valid? value)) (type-error type value))
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
          ((xdr-union-type? type)
           (let ((discr (car value))
                 (arm   (cdr value)))
             (loop (xdr-union-discriminant-type type) discr index)
             ;; We can safely assume that the discriminant is 32-bit.
             (loop (xdr-union-arm-type type discr) arm (+ index 4))))
          (else
           (raise (condition (&xdr-unknown-type-error (type type))))))))


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
          ((xdr-union-type? type)
           (let-values (((discr index)
                         (loop (xdr-union-discriminant-type type) index)))
             ;; The value of the discriminant is not returned.
             (loop (xdr-union-arm-type type discr) index)))
          (else
           (raise (condition (&xdr-unknown-type-error (type type))))))))


;;; Local Variables:
;;; coding: latin-1
;;; End:

;;; xdr.scm ends here
