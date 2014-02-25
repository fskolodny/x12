;;;; x12.lisp

(in-package #:x12)

;;; "X12" goes here. Hacks and glory await!
(defvar s #\^)
(defvar r #\~)
(defvar f #\*)
(defclass paired ()
  (
   (pairs :accessor pairs :initarg :pairs :initform nil)
   (transmittal-id :accessor transmittal-id :initform "")
   (trailer-code :accessor trailer-code :allocation :class)
   )
  )
(defclass gs/ge (paired)
  (
   (trailer-code :initform "GE")
   )
  )
(defclass isa/iea (paired)
  (
   (record-delimiter :accessor record-delimiter :initarg :record-delimiter :initform #\~)
   (field-delimiter :accessor field-delimiter :initarg :field-delimiter :initform #\*)
   (subfield-delimiter :accessor subfield-delimiter :initarg :subfield-delimiter :initform #\^)
   (date)
   (time)
   (trailer-code :initform "IEA")
   )
  )
(defmethod print-object ((self paired) stream)
  (format stream "~a~{~a~}~a" (print-header self stream) (pairs self)
	  (print-trailer self stream))
  )
(defmethod print-header ((self paired) stream)
  )
(defmethod print-trailer ((self paired) stream)
  (format stream "~a~c~d~c~a~c" (trailer-code self) f (length (pairs self)) f
	  (transmittal-id self) r)
  )
(defmethod print-object ((self isa/iea) stream)
  (multiple-value-bind (second minute hour day month year dow dst tz)
      (get-decoded-time)
    (let (
	  (s (slot-value self 'subfield-delimiter))
	  (f (slot-value self 'field-delimiter))
	  (r (slot-value self 'record-delimiter))
	  )
      (setf (transmittal-id self)
	    (format nil "~d~2,'0d~2,'0d~2,'0d~2,'0d" (mod year 10) month day
		    hour minute)
	    (slot-value self 'date)
	    (format nil "~4,'0d~2,'0d~2,'0d" year month day)
	    (slot-value self 'time) (format nil "~2,'0d~2,'0d" hour minute)
	    )
      (call-next-method)
      )
    )
  )
(defmethod print-header ((self isa/iea) stream)
  (format stream
	  "ISA~c00~c~14@a00~11a~cZZ~c~15a~cZZ~c~15a~c~a~c~a~c~c~c~a~c0~cT~c~c~c"
	  f f f f f f "16489767" f f "EMDEON" f
	  (subseq (slot-value self 'date) 2) f (slot-value self 'time) f s f
	  (slot-value self 'transmittal-id) f f f #\< r)
  )
