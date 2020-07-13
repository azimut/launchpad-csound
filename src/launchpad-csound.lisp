(in-package #:launchpad-csound)

(defvar *csound* nil)

;; (cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 0")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 2, gisfnum1, 0, 42")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 3, gisfnum1, 0, 40")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 4, gisfnum1, 0, 8")

(defclass main (launchpad::controller cloud::udp)
  ((root   :initarg :root   :accessor root   :documentation "Pitch Class root note")
   (mode   :initarg :mode   :accessor mode   :documentation "Pitch Class :name")
   (layout :initarg :layout :accessor layout :documentation "Launchpad layout"))
  (:default-initargs
   :root 0
   :mode :minor
   :layout :drum))

(defmethod (setf root) :before (new-root (obj main))
  (check-type new-root (integer 0 7))
  (launchpad:reset)
  (launchpad:change-layout (layout obj))
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (light-up new-root (mode obj) (layout obj))))

(defmethod (setf mode) :before (new-mode (obj main))
  (assert (member new-mode (mapcar #'car ego::*scales*))
          (new-mode) "Invalid Pitch Class mode")
  (launchpad:reset)
  (launchpad:change-layout (layout obj))
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (light-up (root obj) new-mode (layout obj))))

(defmethod (setf layout) :before (new-layout (obj main))
  (assert (member new-layout '(:drum :xy))
          (new-layout) "Invalid Launchpad Layout")
  (launchpad:change-layout new-layout))

(defun iname (instr dot)
  (parse-float:parse-float
   (format nil "~d.~d" instr dot)))
