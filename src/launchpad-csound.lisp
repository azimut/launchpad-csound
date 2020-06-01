(in-package #:launchpad-csound)

;; IDEA: split launchpad into two instruments
;;       show root keys
;;       show keys in KEY
;;       show accepted intervals for pressed note

(setf *csound* (make-instance 'patterns))

;; (cloud:connect   *csound*)
;; (cloud:reconnect *csound*)

(cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 95")
(cloud:send *csound* "fluidProgramSelect giengine1, 2, gisfnum1, 0, 42")

