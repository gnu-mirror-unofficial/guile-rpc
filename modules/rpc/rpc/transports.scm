;;; GNU Guile-RPC --- A Scheme implementation of ONC RPC.  -*- coding: utf-8 -*-
;;; Copyright (C) 2007, 2009, 2010  Free Software Foundation, Inc.
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

(define-module (rpc rpc transports)
  :use-module (rnrs bytevectors)
  :use-module (rnrs io ports)
  :use-module (srfi srfi-60)
  :export (make-rpc-record-sender send-rpc-record
           rpc-record-marking-input-port))


;;; Author: Ludovic Courtès
;;;
;;; Commentary:
;;;
;;; Implementation of the record marking standard (RFC 1831, Section 10) for
;;; use with stream-oriented transports such as TCP.
;;;
;;; Code:


(define %record-header-endianness (endianness big))

(define %max-record-fragment-length
  ;; The maximum size of a record fragment.
  (- (expt 2 31) 1))

(define (make-rpc-record-sender fragment-len)
  "Return a procedure that sends data according to the record marking
standard, chopping its input bytevector into fragments of size
@var{fragment-len} octets."
  (lambda (port bv offset len)
    (let* ((fragment-count (+ 1 (quotient len fragment-len)))
           (last-fragment  (- fragment-count 1))
           (record-header (make-bytevector 4)))
      (let loop ((fragment 0))
        (if (<= fragment last-fragment)
            (let* ((start (* fragment fragment-len))
                   (count (min fragment-len (- len start))))
              (bytevector-u32-set! record-header 0
                                   (if (= fragment last-fragment)
                                       (bitwise-ior count #x80000000)
                                       count)
                                   %record-header-endianness)
              (put-bytevector port record-header)
              (put-bytevector port bv (+ offset start) count)
              (loop (+ 1 fragment))))))))

(define send-rpc-record
  ;; Send the RPC message of @var{len} octets encoded at offset @var{offset}
  ;; in @var{bv} (a bytevector) to @var{port}.
  (make-rpc-record-sender %max-record-fragment-length))

(define (rpc-record-marking-input-port port)
  "Return a binary input port that proxies @var{port} in order to implement
decoding of the record marking standard (RFC 1831, Section 10)."

  (define in-fragment?   #f)
  (define last-fragment? #f)
  (define fragment-len    0)  ;; size of fragment being read
  (define octet-count     0)  ;; total number of octets read from the fragment

  ;; An awful imperative implementation of "record marking" (Section 10).

  (define (read! bv start count)

    ;;(format #t "gotta read ~a~%" count)
    (let record-marking-read ((start start)
                              (count count)
                              (total 0))

      ;;(format #t "looping: ~a ~a ~a~%" start count total)
      (if in-fragment?
          (let* ((remaining (- fragment-len octet-count))
                 (result    (get-bytevector-n! port bv start
                                               (min remaining count))))
            (if (eof-object? result)
                0
                (let ((last? last-fragment?))
                  (set! octet-count (+ octet-count count))
                  (if (>= octet-count fragment-len)
                      (set! in-fragment? #f))

                  (if (or last? (<= count result))
                      (+ total result)
                      (record-marking-read (+ start result) (- count result)
                                           (+ total result))))))
          (let (;;(zzz (format #t "getting record header...~%"))
                (raw (get-bytevector-n port 4)))
            (if (eof-object? raw)
                (begin
                  ;;(format #t "failed to get record header~%")
                  total)
                (let* ((header
                        (bytevector-u32-ref raw 0
                                            %record-header-endianness))
                       (len (bitwise-and #x7fffffff header)))
                  ;; enter the new fragment
                  (set! last-fragment? (bit-set? 31 header))
                  (set! fragment-len   len)
                  (set! octet-count    0)
                  (set! in-fragment?   #t)

                  ;;(format #t "starting new frag: len=~a last=~a~%"
                  ;;        fragment-len last-fragment?)
                  (record-marking-read start count total)))))))

    (make-custom-binary-input-port "record-marking input port"
                                   read! #f #f #f))

;;; transports.scm ends here
