(in-package #:x12)
(named-readtables:in-readtable rutils-readtable)

(defun read-5010-record (stream &optional a b)
  (declare (type stream stream)
           (ignorable a b)
           )
  (let* ((c (skip-whitespace stream))
         (buf (coerce (iter
                        (:for i :from 0 :to 9999)
                        (:until (= c r))
                        (:collect c)
                        (setf c (read-byte stream nil r))
                        )
                      'vector))
         (seq (split-sequence f buf))
         (class-name (octets-to-string (elt seq 0)))
         (class (or (find-class (intern class-name :x12))
                    (error "Class ~a not found." class-name)))
         (obj (apply #'make-instance class
                     (flatten (zip (iter
                                    (:for i :from 0 :to (length seq))
                                    (:collect (ensure-keyword i :format "F~d"))
                                    )
                                   seq
                                   )
                              )
                     )
           )
         )
    obj
    )
  )
(defun read-st/se-pair (temp-record stream)
  (declare (type stream stream))
  (let ((se-record nil)
        (st-record temp-record)
        )
    (make-instance 'st/se :header st-record
                   :children
                   (iter
                     (setf temp-record (read-5010-record stream))
                     (:while (not se-record))
                     (if (typep temp-record 'se)
                         (progn
                           (setf se-record temp-record)
                           (return children)
                           )
                         (:collect temp-record :into children))
                     )
                   :trailer se-record
                   )
    )
  )
(defun read-gs/ge-pair (temp-record stream)
  (declare (type stream stream))
  (let ((ge-record nil)
        (gs-record temp-record)
        )
    (make-instance 'gs/ge :header gs-record
                   :children
                   (iter
                     (setf temp-record (read-5010-record stream))
                     (:while (not ge-record))
                     (if (typep temp-record 'st)
                         (:collect (read-st/se-pair temp-record stream)
                           :into children)
                         (if (typep temp-record 'ge)
                             (progn
                               (setf ge-record temp-record)
                               (return children)
                               )
                             (error "Invalid class ~a found."
                                    (class-of temp-record)))
                         )
                     )
                   :trailer ge-record
                   )
    )
  )

(defun skip-whitespace (stream)
  (declare (type stream stream))
  (let ((c)
        )
    (iter
      (:for i :from 0 :to 999999)
      (setf c (read-byte stream nil r))
      (if (not (member c whitespace :test #'=))
          (return-from skip-whitespace c))
      )
    )
  )

(defun read-isa-record (stream)
  (declare (type stream stream))
  (let ((c (skip-whitespace stream))
        (record (make-array 106 :element-type 'octet))
        (cnt)
        )
    (setf (aref record 0) c
          cnt (read-sequence record stream :start 1))
    (unless (= cnt 106)
      (format t "Only read ~d bytes from stream~%" (1- cnt))
      (error "Cannot read ISA from stream ~a, too short only read ~d bytes"
             stream cnt)
      )
      record
    )
  )

(defmacro make-coded-value (code value)
  `(make-instance 'coded-value :code ,code :value ,value)
  )
(defun read-isa/iea-pair (stream)
  (declare (type stream stream))
  (let* (
         (record (read-isa-record stream))
         (r (aref record 105))
         (f (aref record 3))
         (s (aref record 82))
         (subsubfield-delimiter (aref record 104))
         (isa-record)
         (iea-record nil)
         (temp-record)
         )
    (unless (and (equalp (subseq record 0 3)
                         #v((char-code #\I) (char-code #\S) (char-code #\A)))
                 (equalp #v(3 6 17 20 31 34 50 53 69 76 81 83 89 99 101 103)
                         (coerce (iter
                                   (:for i :from 0 :to 105)
                                   (if (= (aref record i) f)
                                       (:collect i)))
                                 'vector)
                         )
                 )
      (error "~a record does not match specification for ISA" record))
    (setf isa-record 
          (make-instance 'isa
                         :authorization-info
                         (make-coded-value (subseq record 4 6)
                                           (subseq record 7 17))
                         :security-info
                         (make-coded-value (subseq record 18 20)
                                           (subseq record 21 31))
                         :interchange-sender
                         (make-coded-value (subseq record 32 34)
                                           (subseq record 35 50))
                         :interchange-receiver
                         (make-coded-value (subseq record 51 53)
                                           (subseq record 54 69))
                         :interchange-date (subseq record 70 76)
                         :interchange-time (subseq record 77 81)
                         :interchange-type (subseq record 84 89)
                         :control-id (subseq record 90 99)
                         :ack-needed (subseq record 100 101)
                         :usage-indicator (subseq record 102 103)))
    (make-instance 'isa/iea :header isa-record :record-delimiter r
                   :field-delimiter f :subfield-delimiter s
                   :children
                   (iter
                    (setf temp-record (read-5010-record stream))
                    (:while (not iea-record))
                    (if (typep temp-record 'gs)
                        (:collect (read-gs/ge-pair temp-record stream)
                                  :into kids)
                        (if (typep temp-record 'iea)
                            (progn
                              (setf iea-record temp-record)
                              (return kids)
                              )
                            (error "Bad class ~a." (class-of temp-record))
                            )
                        )
                    )
                   :trailer iea-record
                   )
    )
  )

