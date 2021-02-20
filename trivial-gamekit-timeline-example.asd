(asdf:defsystem #:trivial-gamekit-timeline-example
  :description "animation timeline module for trivial-gamekit"
  :author "ava fox"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:trivial-gamekit :trivial-gamekit-timeline)
  :components ((:file "example")))
