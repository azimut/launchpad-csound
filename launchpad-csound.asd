;;;; launchpad-csound.asd

(asdf:defsystem #:launchpad-csound
  :description "Describe launchpad-csound here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:launchpad
               #:cloud/udp
               #:parse-float
               #:cl-punch
               #:cl-arrows
               #:trivia
               #:scheduler
               #:ego
               #:cm
               #:atomics)
  :components ((:file "package")
               (:file "loop")
               (:file "launchpad-csound")
               (:file "scheduler")
               (:file "patterns")
               (:file "splitted")))
