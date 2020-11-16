;;;; damn-fast-priority-queue.asd

(asdf:defsystem #:damn-fast-priority-queue
  :description "A priority queue whose first and foremost priority is speed."
  :author "Michał \"phoe\" Herda <phoe@disroot.org>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "damn-fast-priority-queue")))
