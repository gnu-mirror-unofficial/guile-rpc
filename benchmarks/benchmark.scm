;;; Benchmarking tools.                                     -*- Scheme -*-
;;;
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

(define-module (benchmark)
  :use-module (statprof)
  :export (iterate))

(define profile?
  ;; Enable profiling via `statprof' when `GUILE_RPC_PROFILING' is defined.
  (not (not (getenv "GUILE_RPC_PROFILING"))))

(define-syntax iterate
  ;; Run BODY COUNT times.
  (syntax-rules ()
    ((_ count body ...)
     (if (not profile?)
         (let ((start (get-internal-run-time)))
           (let loop ((i count))
             (if (<= i 0)
                 (let ((end (get-internal-run-time)))
                   (format #t "time taken: ~a internal time units~%"
                           (- end start)))
                 (begin
                   body ...
                   (loop (1- i))))))
         (with-statprof #:loop count body ...)))))
