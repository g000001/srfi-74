;;;; srfi-74.lisp

(cl:in-package :srfi-74.internal)

; Octet-addressed binary objects

; Copyright (C) Michael Sperber (2005). All Rights Reserved.
;
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
;
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

; This uses SRFIs 23, 26, 60, and 66

(defvar *endianness/little* (list :little))
(defvar *endianness/big* (list :big))

(define-syntax endianness
  (syntax-rules (:little :big :native)
    ((endianness :little) *endianness/little*)
    ((endianness :big) *endianness/big*)
    ;; change this to the endianness of your architecture
    ((endianness :native)
     (if (member :little-endian cl:*features*)
         *endianness/little*
         *endianness/big*))))

(define-function blob? #'u8vector?)

(define-function (make-blob k)
  (make-u8vector k 0))

(define-function (blob-length b)
  (u8vector-length b))

(define-function (blob-u8-ref b k)
  (u8vector-ref b k))
(define-function (blob-u8-set! b k octet)
  (u8vector-set! b k octet))

(define-function (u8->s8 octet)
  (if (> octet 127)
      (- octet 256)
      octet))

(define-function (blob-s8-ref b k)
  (u8->s8 (u8vector-ref b k)))

(define-function (s8->u8 val)
  (if (negative? val)
      (+ val 256)
      val))

(define-function (blob-s8-set! b k val)
  (u8vector-set! b k (s8->u8 val)))

(define-function (index-iterate start count low-first?
                                unit proc)
  (if low-first?
      (let loop ((index 0)
		 (acc unit))
	(if (>= index count)
	    acc
	    (loop (+ index 1)
		  (funcall proc (+ start index) acc))))

      (let loop ((index (- (+ start count) 1))
		 (acc unit))
	(if (< index start)
	    acc
	    (loop (- index 1)
		  (funcall proc index acc))))))

(define-function (blob-uint-ref size endness blob index)
  (index-iterate index size
		       (eq? (endianness :big) endness)
		       0
		       (lambda (index acc)
			 (+ (u8vector-ref blob index) (arithmetic-shift acc 8)))))

(define-function (blob-sint-ref size endness blob index)
  (let ((high-byte (u8vector-ref blob
				 (if (eq? endness (endianness :big))
				     index
				     (- (+ index size) 1)))))

    (if (> high-byte 127)
	(- (+ 1
	      (index-iterate index size
			     (eq? (endianness :big) endness)
			     0
			     (lambda (index acc)
			       (+ (- 255 (u8vector-ref blob index))
				  (arithmetic-shift acc 8))))))
	(index-iterate index size
		       (eq? (endianness :big) endness)
		       0
		       (lambda (index acc)
			 (+ (u8vector-ref blob index) (arithmetic-shift acc 8)))))))

(define-function (make-uint-ref size)
  (cut #'blob-uint-ref size <> <> <>))

(define-function (make-sint-ref size)
  (cut #'blob-sint-ref size <> <> <>))

(define-function (blob-uint-set! size endness blob index val)
  (index-iterate index size (eq? (endianness :little) endness)
		 val
		 (lambda (index acc)
		   (u8vector-set! blob index (remainder acc 256))
		   (quotient acc 256)))
  (values))

(define-function (blob-sint-set! size endness blob index val)
  (if (negative? val)
      (index-iterate index size (eq? (endianness :little) endness)
		     (- -1 val)
		     (lambda (index acc)
		       (u8vector-set! blob index (- 255 (remainder acc 256)))
		       (quotient acc 256)))

      (index-iterate index size (eq? (endianness :little) endness)
		     val
		     (lambda (index acc)
		       (u8vector-set! blob index (remainder acc 256))
		       (quotient acc 256))))

  (values))

(define-function (make-uint-set! size)
  (cut #'blob-uint-set! size <> <> <> <>))
(define-function (make-sint-set! size)
  (cut #'blob-sint-set! size <> <> <> <>))

(define-function (ensure-aligned index base)
  (if (not (zero? (remainder index base)))
      (error "non-aligned blob access" index base)))

(define-function (make-ref/native base base-ref)
  (lambda (blob index)
    (ensure-aligned index base)
    (funcall base-ref (endianness :native) blob index)))

(define-function (make-set!/native base base-set!)
  (lambda (blob index val)
    (ensure-aligned index base)
    (funcall base-set! (endianness :native) blob index val)))

(define-function blob-u16-ref (make-uint-ref 2))
(define-function blob-u16-set! (make-uint-set! 2))
(define-function blob-s16-ref (make-sint-ref 2))
(define-function blob-s16-set! (make-sint-set! 2))
(define-function blob-u16-native-ref (make-ref/native 2 #'blob-u16-ref))
(define-function blob-u16-native-set! (make-set!/native 2 #'blob-u16-set!))
(define-function blob-s16-native-ref (make-ref/native 2 #'blob-s16-ref))
(define-function blob-s16-native-set! (make-set!/native 2 #'blob-s16-set!))

(define-function blob-u32-ref (make-uint-ref 4))
(define-function blob-u32-set! (make-uint-set! 4))
(define-function blob-s32-ref (make-sint-ref 4))
(define-function blob-s32-set! (make-sint-set! 4))
(define-function blob-u32-native-ref (make-ref/native 4 #'blob-u32-ref))
(define-function blob-u32-native-set! (make-set!/native 4 #'blob-u32-set!))
(define-function blob-s32-native-ref (make-ref/native 4 #'blob-s32-ref))
(define-function blob-s32-native-set! (make-set!/native 4 #'blob-s32-set!))

(define-function blob-u64-ref (make-uint-ref 8))
(define-function blob-u64-set! (make-uint-set! 8))
(define-function blob-s64-ref (make-sint-ref 8))
(define-function blob-s64-set! (make-sint-set! 8))
(define-function blob-u64-native-ref (make-ref/native 8 #'blob-u64-ref))
(define-function blob-u64-native-set! (make-set!/native 8 #'blob-u64-set!))
(define-function blob-s64-native-ref (make-ref/native 8 #'blob-s64-ref))
(define-function blob-s64-native-set! (make-set!/native 8 #'blob-s64-set!))

; Auxiliary stuff

(define-function (blob-copy! source source-start target target-start count)
  (u8vector-copy! source source-start target target-start count))

(define-function (blob-copy b)
  (u8vector-copy b))

(define-function (blob=? b1 b2)
  (u8vector=? b1 b2))

(define-function (blob->u8-list b)
  (u8vector->list b))
(define-function (blob->s8-list b)
  (mapcar #'u8->s8 (u8vector->list b)))

(define-function (u8-list->blob l)
  (list->u8vector l))
(define-function (s8-list->blob l)
  (list->u8vector (mapcar #'s8->u8 l)))

(define-function (make-blob->int-list blob-ref)
  (lambda (size endness b)
    (let ((ref (cut blob-ref size endness b <>))
	  (length (blob-length b)))
      (let loop ((i 0) (r '()))
	(if (>= i length)
	    (reverse r)
	    (loop (+ i size)
		  (cons (funcall ref i) r)))))))

(define-function blob->uint-list (make-blob->int-list #'blob-uint-ref))
(define-function blob->sint-list (make-blob->int-list #'blob-sint-ref))

(define-function (make-int-list->blob blob-set!)
  (lambda (size endness l)
    (let* ((blob (make-blob (* size (length l))))
	   (set! (cut blob-set! size endness blob <> <>)))
      (let loop ((i 0) (l l))
	(if (null? l)
	    blob
	    (begin
	      (funcall set! i (car l))
	      (loop (+ i size) (cdr l))))))))

(define-function uint-list->blob (make-int-list->blob #'blob-uint-set!))
(define-function sint-list->blob (make-int-list->blob #'blob-sint-set!))

;;; eof
