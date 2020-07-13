;;;; launchpad-csound.asd

(asdf:defsystem #:launchpad-csound
  :description "Describe launchpad-csound here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:cl-arrows
               #:cl-punch
               #:cloud/udp
               #:cm
               #:ego
               #:launchpad
               #:parse-float
               #:scheduler
               #:trivia)
  :components ((:file "package")
               (:file "presets")
               (:file "launchpad-csound")
               (:file "scheduler")
               (:file "patterns")
               (:file "splitted")))
