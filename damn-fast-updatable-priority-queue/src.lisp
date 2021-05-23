;;;; damn-fast-updatable-priority-queue.lisp

(defpackage #:damn-fast-updatable-priority-queue
  (:use #:cl)
  (:shadow #:map #:delete)
  (:local-nicknames (#:a #:alexandria))
  (:export #:queue #:make-queue #:copy-queue
           #:enqueue #:dequeue #:peek #:size #:trim #:map #:do-queue
           #:delete #:adjust-priority
           #:handle-priority #:handle-data
           #:queue-size-limit-reached
           #:queue-size-limit-reached-queue #:queue-size-limit-reached-object))

(in-package #:damn-fast-updatable-priority-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read-time variables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize-qualities*
    #+real-damn-fast-priority-queue
    ;; Good luck.
    `(optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0))
    #-real-damn-fast-priority-queue
    `(optimize (speed 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Type definitions

(deftype data-type () '(cons fixnum t))

(deftype data-vector-type () '(simple-array data-type (*)))

(deftype prio-type () '(unsigned-byte 32))

(deftype prio-vector-type () '(simple-array prio-type (*)))

(deftype extension-factor-type () '(integer 2 256))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structure definition

(declaim (inline %make %data-vector %prio-vector %size %extension-factor))

(defstruct (queue (:conc-name #:%) (:constructor %make)
                  (:predicate nil) (:copier nil))
  (data-vector (make-array 256 :element-type 'data-type
                               :initial-element (cons 0 nil))
   :type data-vector-type)
  (prio-vector (make-array 256 :element-type 'prio-type) :type prio-vector-type)
  (size 0 :type a:array-length)
  (extension-factor 2 :type extension-factor-type)
  (extend-queue-p t :type boolean))

(declaim (inline make-queue copy-queue))

(declaim (ftype (function
                 (&optional a:array-index extension-factor-type boolean)
                 (values queue &optional))
                make-queue))
(defun make-queue (&optional
                     (initial-storage-size 256)
                     (extension-factor 2)
                     (extend-queue-p t))
  (declare (type extension-factor-type extension-factor))
  (declare #.*optimize-qualities*)
  (%make :extension-factor extension-factor
         :data-vector (make-array initial-storage-size
                                  :element-type 'data-type
                                  :initial-element (cons 0 nil))
         :prio-vector (make-array initial-storage-size
                                  :element-type 'prio-type)
         :extend-queue-p extend-queue-p))

(defmethod print-object ((object queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (%size object))))

(declaim (ftype (function (queue) (values queue &optional)) copy-queue))
(defun copy-queue (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (%make :extension-factor (%extension-factor queue)
         :size (%size queue)
         :extend-queue-p (%extend-queue-p queue)
         :data-vector (cl:map 'data-vector-type
                              (lambda (a) (cons (car a) (cdr a)))
                           (%data-vector queue))
         :prio-vector (copy-seq (%prio-vector queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enqueueing

(declaim (inline heapify-upwards enqueue))

(declaim (ftype (function (data-vector-type prio-vector-type a:array-length)
                          (values null &optional))
                heapify-upwards))
(defun heapify-upwards (data-vector prio-vector index)
  (declare (type data-vector-type data-vector))
  (declare (type prio-vector-type prio-vector))
  (declare (type a:array-length index))
  (declare #.*optimize-qualities*)
  (do ((child-index index parent-index)
       (parent-index (ash (1- index) -1) (ash (1- parent-index) -1)))
      ((= child-index 0))
    (let ((child-priority (aref prio-vector child-index))
          (parent-priority (aref prio-vector parent-index)))
      (cond ((< child-priority parent-priority)
             (rotatef (aref prio-vector parent-index)
                      (aref prio-vector child-index))
             (rotatef (car (aref data-vector parent-index))
                      (car (aref data-vector child-index)))
             (rotatef (aref data-vector parent-index)
                      (aref data-vector child-index)))
            (t (return))))))

(declaim (ftype (function (queue t prio-type) (values cons &optional)) enqueue))
(defun enqueue (queue object priority)
  (declare (type queue queue))
  (declare (type prio-type priority))
  (declare #.*optimize-qualities*)
  (symbol-macrolet ((data-vector (%data-vector queue))
                    (prio-vector (%prio-vector queue)))
    (let* ((size (%size queue))
           (extension-factor (%extension-factor queue))
           (length (array-total-size data-vector))
           (datum (cons size object)))
      (when (>= size length)
        (unless (%extend-queue-p queue)
          (error 'queue-size-limit-reached :queue queue :element object))
        (let ((new-length (max 1 (mod (* length extension-factor)
                                      (ash 1 64)))))
          (declare (type a:array-length new-length))
          (when (<= new-length length)
            (error "Integer overflow while resizing array: new-length ~D is ~
                    smaller than old length ~D" new-length length))
          (setf data-vector (adjust-array data-vector new-length
                                          :initial-element (cons 0 nil))
                prio-vector (adjust-array prio-vector new-length))))
      (setf (aref data-vector size) datum
            (aref prio-vector size) priority)
      (heapify-upwards data-vector prio-vector (%size queue))
      (incf (%size queue))
      datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dequeueing

(declaim (inline heapify-downwards dequeue))

(declaim (ftype (function (data-vector-type prio-vector-type a:array-index fixnum)
                          (values null &optional))
                heapify-downwards))
(defun heapify-downwards (data-vector prio-vector size parent-index)
  (declare (type data-vector-type data-vector))
  (declare (type prio-vector-type prio-vector))
  (declare (type fixnum parent-index))
  (declare #.*optimize-qualities*)
  (loop
    (assert (<= 0 parent-index size))
    (let* ((left-index (+ (* parent-index 2) 1))
           (left-index-validp (< left-index size))
           (right-index (+ (* parent-index 2) 2))
           (right-index-validp (< right-index size)))
      (flet ((swap-left ()
               (rotatef (aref prio-vector parent-index)
                        (aref prio-vector left-index))
               (rotatef (car (aref data-vector parent-index))
                        (car (aref data-vector left-index)))
               (rotatef (aref data-vector parent-index)
                        (aref data-vector left-index))
               (setf parent-index left-index))
             (swap-right ()
               (rotatef (aref prio-vector parent-index)
                        (aref prio-vector right-index))
               (rotatef (car (aref data-vector parent-index))
                        (car (aref data-vector right-index)))
               (rotatef (aref data-vector parent-index)
                        (aref data-vector right-index))
               (setf parent-index right-index)))
        (declare (inline swap-left swap-right))
        (when (and (not left-index-validp)
                   (not right-index-validp))
          (return))
        (when (and left-index-validp
                   (<= (aref prio-vector parent-index)
                       (aref prio-vector left-index))
                   (or (not right-index-validp)
                       (<= (aref prio-vector parent-index)
                           (aref prio-vector right-index))))
          (return))
        (if (and right-index-validp
                 (<= (aref prio-vector right-index)
                     (aref prio-vector left-index)))
            (swap-right)
            (swap-left))))))

(declaim (ftype (function (queue) (values t boolean (or null data-type)
                                          &optional))
                dequeue))
(defun dequeue (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (if (= 0 (%size queue))
      (values nil nil nil)
      (let ((data-vector (%data-vector queue))
            (prio-vector (%prio-vector queue)))
        (multiple-value-prog1 (values (cdr (aref data-vector 0))
                                      t
                                      (aref data-vector 0))
          (decf (%size queue))
          (let ((old-data (aref data-vector (%size queue)))
                (old-prio (aref prio-vector (%size queue))))
            (setf (aref data-vector 0) old-data
                  (aref prio-vector 0) old-prio
                  (car (aref data-vector 0)) 0))
          (heapify-downwards data-vector prio-vector (%size queue) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Introspection and maintenance

(declaim (inline peek size trim map))

(declaim (ftype (function (queue) (values t boolean &optional)) peek))
(defun peek (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (if (= 0 (%size queue))
      (values nil nil)
      (values (cdr (aref (%data-vector queue) 0)) t)))

(declaim (ftype (function (queue) (values a:array-length &optional)) size))
(defun size (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (%size queue))

(declaim (ftype (function (queue) (values null &optional)) trim))
(defun trim (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (let ((size (%size queue)))
    (setf (%data-vector queue) (adjust-array (%data-vector queue) size)
          (%prio-vector queue) (adjust-array (%prio-vector queue) size))
    nil))

(declaim (ftype (function (queue (function (t) t)) (values null &optional))
                map))
(defun map (queue function)
  (dotimes (i (%size queue))
    (funcall function (cdr (aref (%data-vector queue) i)))))

(defmacro do-queue ((object queue &optional result) &body body)
  (multiple-value-bind (forms declarations) (a:parse-body body)
    (a:with-gensyms (i)
      (a:once-only (queue)
        `(dotimes (,i (%size ,queue) ,result)
           (let ((,object (cdr (aref (%data-vector ,queue) ,i))))
             ,@declarations
             (tagbody ,@forms)))))))

(declaim (inline valid-handle handle-priority handle-data))

(declaim (ftype (function (queue data-type) (values t &optional))
                valid-handle))
(defun valid-handle (queue handle)
  (declare (type queue queue)
           (type data-type handle))
  (let ((index (car handle)))
    (when (and (<= 0 index (1- (%size queue)))
               (eq handle (aref (%data-vector queue) index)))
      index)))

(declaim (ftype (function (queue data-type)
                          (values (or null prio-type) &optional))
                handle-priority))
(defun handle-priority (queue handle)
  (let ((index (valid-handle queue handle))
        (prio-vector (%prio-vector queue)))
    (when index
      (aref prio-vector index))))

(declaim (ftype (function (queue data-type)
                          (values t boolean &optional))
                handle-data))
(defun handle-data (queue handle)
  (let ((index (valid-handle queue handle)))
    (if index
        (values (cdr handle) t)
        (values nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Modification

(declaim (inline delete adjust-priority))

(declaim (ftype (function (queue data-type) (values boolean &optional)) delete))
(defun delete (queue object)
  (declare (type queue queue)
           (type data-type object))
  (if (= 0 (%size queue))
      nil
      (let ((index (valid-handle queue object))
            (data-vector (%data-vector queue))
            (prio-vector (%prio-vector queue)))
        (when index
          (let ((deleted-prio (aref prio-vector index)))
            (decf (%size queue))
            (let ((old-data (aref data-vector (%size queue)))
                  (old-prio (aref prio-vector (%size queue))))
              (setf (aref data-vector index) old-data
                    (aref prio-vector index) old-prio)
              (setf (car (aref data-vector index)) index)
              (cond
                ((< old-prio deleted-prio)
                 (heapify-upwards data-vector prio-vector index))
                ((> old-prio deleted-prio)
                 (heapify-downwards data-vector prio-vector (%size queue) index)))))
          t))))

(declaim (ftype (function (queue data-type prio-type) (values boolean &optional)) adjust-priority))
(defun adjust-priority (queue object new-priority)
  (declare (type queue queue)
           (type data-type object)
           (type prio-type new-priority))
  (let ((index (valid-handle queue object))
        (data-vector (%data-vector queue))
        (prio-vector (%prio-vector queue)))
    (when index
      (let ((old-priority (aref prio-vector index)))
        (setf (aref prio-vector index) new-priority)
        (cond
          ((< new-priority old-priority)
           (heapify-upwards data-vector prio-vector index))
          ((> new-priority old-priority)
           (heapify-downwards data-vector prio-vector (%size queue) index))))
      t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conditions

(defun report-queue-size-limit-reached (condition stream)
  (let ((queue (queue-size-limit-reached-queue condition))
        (element (queue-size-limit-reached-object condition)))
    (format stream "Size limit (~D) reached for non-extensible ~
                    queue ~S while trying to enqueue element ~S onto it."
            (length (%data-vector queue)) queue element)))

(define-condition queue-size-limit-reached (error)
  ((%queue :reader queue-size-limit-reached-queue :initarg :queue)
   (%object :reader queue-size-limit-reached-object :initarg :element))
  (:default-initargs :queue (a:required-argument :queue)
                     :object (a:required-argument :object))
  (:report report-queue-size-limit-reached))
