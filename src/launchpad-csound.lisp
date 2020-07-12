(in-package #:launchpad-csound)

(defvar *csound* nil)

;; (cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 0")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 2, gisfnum1, 0, 42")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 3, gisfnum1, 0, 40")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 4, gisfnum1, 0, 8")

(defclass main (launchpad::controller cloud::udp)
  ())

(defun iname (instr dot)
  (parse-float:parse-float
   (format nil "~d.~d" instr dot)))
