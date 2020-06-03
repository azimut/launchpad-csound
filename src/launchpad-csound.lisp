(in-package #:launchpad-csound)

;; IDEA: split launchpad into two instruments
;;       show root keys
;;       show keys in KEY
;;       show accepted intervals for pressed note

(defvar *csound* nil)

;; (cloud:connect   *csound*)
;; (cloud:reconnect *csound*)

;; (cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 0")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 2, gisfnum1, 0, 42")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 3, gisfnum1, 0, 40")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 4, gisfnum1, 0, 8")

