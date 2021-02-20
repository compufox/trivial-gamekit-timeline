;;;; trivial-gamekit-timeline.asd

(asdf:defsystem #:trivial-gamekit-timeline
  :description "animation timeline module for trivial-gamekit"
  :author "ava fox"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:trivial-gamekit)
  :serial t
  :components ((:file "trivial-gamekit-timeline")))
