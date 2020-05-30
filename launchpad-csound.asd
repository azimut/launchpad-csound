;;;; launchpad-csound.asd

(asdf:defsystem #:launchpad-csound
  :description "Describe launchpad-csound here"
  :author "azimut <azimut.github@protonmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:launchpad
               #:cloud/udp
               #:parse-float)
  :components ((:file "package")
               (:file "launchpad-csound")))
