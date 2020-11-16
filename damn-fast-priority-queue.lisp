;;;; damn-fast-priority-queue.lisp

(defpackage #:damn-fast-priority-queue
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:queue #:make-queue #:enqueue #:dequeue #:peek #:size #:trim))

(in-package #:damn-fast-priority-queue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Read-time variables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize-qualities*
    #+really-damn-fast-priority-queues
    ;; Good luck.
    `(optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0))
    #-really-damn-fast-priority-queues
    `(optimize (speed 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structure definition

(deftype data-type () 't)

(deftype data-vector-type () '(simple-array data-type (*)))

(deftype prio-type () '(unsigned-byte 32))

(deftype prio-vector-type () '(simple-array prio-type (*)))

(deftype extension-size-type () '(and (integer 1) a:array-length))

(declaim (inline %make %data-vector %prio-vector %size %extension-size))

(defstruct (queue (:conc-name #:%) (:constructor %make)
                  (:predicate nil) (:copier nil))
  (data-vector (make-array 256 :element-type 'data-type) :type data-vector-type)
  (prio-vector (make-array 256 :element-type 'prio-type) :type prio-vector-type)
  (size 0 :type a:array-length)
  (extension-size 256 :type extension-size-type))

(declaim (inline make-queue))

(declaim (ftype (function (&optional extension-size-type)
                          (values queue &optional))
                make-queue))
(defun make-queue (&optional (extension-size 256))
  (declare (type extension-size-type extension-size))
  (declare #.*optimize-qualities*)
  (%make :extension-size extension-size
         :data-vector (make-array extension-size :element-type 'data-type)
         :prio-vector (make-array extension-size :element-type 'prio-type)))

(defmethod print-object ((object queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (%size object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enqueueing

(declaim (inline heapify-upwards enqueue))

(declaim (ftype (function (data-vector-type prio-vector-type a:array-length)
                          (values null &optional))
                heapify-upwards))
(defun heapify-upwards (data-vector prio-vector size)
  (declare (type data-vector-type data-vector))
  (declare (type prio-vector-type prio-vector))
  (declare (type a:array-length size))
  (declare #.*optimize-qualities*)
  (do ((child-index size (truncate child-index 2))
       (parent-index (truncate size 2) (truncate parent-index 2)))
      ((= 0 child-index))
    (let ((child-priority (aref prio-vector child-index))
          (parent-priority (aref prio-vector parent-index)))
      (when (< child-priority parent-priority)
        (rotatef (aref prio-vector parent-index)
                 (aref prio-vector child-index))
        (rotatef (aref data-vector parent-index)
                 (aref data-vector child-index))))))

(defmacro vector-push-replace (new-element position vector min-extension)
  (a:with-gensyms (length new-length)
    (a:once-only (position)
      `(let ((,length (array-total-size ,vector)))
         (when (>= ,position ,length)
           (let ((,new-length (+ ,length ,min-extension)))
             (declare (type a:array-length ,new-length))
             (setf ,vector (adjust-array ,vector ,new-length))))
         (setf (aref ,vector ,position) ,new-element)))))

(declaim (ftype (function (queue t fixnum) (values null &optional)) enqueue))
(defun enqueue (queue object priority)
  (declare (type queue queue))
  (declare (type fixnum priority))
  (declare #.*optimize-qualities*)
  (let ((data-vector (%data-vector queue))
        (prio-vector (%prio-vector queue))
        (size (%size queue))
        (extension-size (%extension-size queue)))
    (vector-push-replace object size (%data-vector queue) extension-size)
    (vector-push-replace priority size (%prio-vector queue) extension-size)
    (heapify-upwards data-vector prio-vector (%size queue))
    (incf (%size queue)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dequeueing

(declaim (inline heapify-downwards dequeue))

(declaim (ftype (function (data-vector-type prio-vector-type a:array-length)
                          (values null &optional))
                heapify-downwards))
(defun heapify-downwards (data-vector prio-vector size)
  (declare (type data-vector-type data-vector))
  (declare (type prio-vector-type prio-vector))
  (declare (type a:array-length size))
  (declare #.*optimize-qualities*)
  (do ((parent-index 0))
      (nil)
    (let ((left-child-index (+ (* parent-index 2) 1))
          (right-child-index (+ (* parent-index 2) 2)))
      (cond ((and (< left-child-index size)
                  (> (aref prio-vector parent-index)
                     (aref prio-vector right-child-index))
                  (>= (aref prio-vector left-child-index)
                      (aref prio-vector right-child-index)))
             (rotatef (aref prio-vector parent-index)
                      (aref prio-vector right-child-index))
             (rotatef (aref data-vector parent-index)
                      (aref data-vector right-child-index))
             (setf parent-index right-child-index))
            ((and (< right-child-index size)
                  (> (aref prio-vector parent-index)
                     (aref prio-vector left-child-index))
                  (>= (aref prio-vector right-child-index)
                      (aref prio-vector left-child-index)))
             (rotatef (aref prio-vector parent-index)
                      (aref prio-vector left-child-index))
             (rotatef (aref data-vector parent-index)
                      (aref data-vector left-child-index))
             (setf parent-index left-child-index))
            (t (return))))))

(declaim (ftype (function (queue) (values t boolean)) dequeue))
(defun dequeue (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (if (= 0 (%size queue))
      (values nil nil)
      (let ((data-vector (%data-vector queue))
            (prio-vector (%prio-vector queue)))
        (multiple-value-prog1 (values (aref data-vector 0) t)
          (let ((old-data (aref data-vector (1- (%size queue))))
                (old-prio (aref prio-vector (1- (%size queue)))))
            (setf (aref data-vector 0) old-data
                  (aref prio-vector 0) old-prio))
          (decf (%size queue))
          (heapify-downwards data-vector prio-vector (%size queue))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Introspection and maintenance

(declaim (inline peek size trim))

(declaim (ftype (function (queue) (values t boolean)) peek))
(defun peek (queue)
  (declare (type queue queue))
  (declare #.*optimize-qualities*)
  (if (= 0 (%size queue))
      (values nil nil)
      (values (aref (%data-vector queue) 0) t)))

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
