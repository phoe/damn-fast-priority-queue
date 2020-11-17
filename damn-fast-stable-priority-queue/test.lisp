;;;; damn-fast-stable-priority-queue-test.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Main interface

(defpackage #:damn-fast-stable-priority-queue/test
  (:use #:cl)
  (:export #:run))

(in-package #:damn-fast-stable-priority-queue/test)

(defun run (&optional verbose)
  (when verbose (format t "~&;;; Testing with the same priorities."))
  (damn-fast-stable-priority-queue/test-same-priorities:run verbose)
  (when verbose (format t "~&;;; Testing with distinct priorities."))
  (damn-fast-stable-priority-queue/test-distinct-priorities:run verbose))
