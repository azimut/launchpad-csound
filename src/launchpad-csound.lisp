(in-package #:launchpad-csound)

(defvar *csound* nil
  "csound/launchpad server, needed at times globally for different threads")

;; (cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 0")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 2, gisfnum1, 0, 42")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 3, gisfnum1, 0, 40")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 4, gisfnum1, 0, 8")

(defclass main (launchpad::controller cloud::udp scheduler)
  ((root   :initarg :root   :accessor root   :documentation "Pitch Class root note")
   (mode   :initarg :mode   :accessor mode   :documentation "Pitch Class :name")
   (layout :initarg :layout :accessor layout :documentation "Launchpad layout"))
  (:default-initargs
   :root 0
   :mode :minor
   :layout :drum))

(defmethod (setf root) :before (new-root (obj main))
  (check-type new-root (integer 0 11))
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

(defun next-root (current-root)
  (mod (1+ current-root) 12))

(let ((classes '(patterns splitted)))
  (defun next-class ()
    (nth (mod (1+ (or (position (class-name (class-of *csound*)) classes :test #'eql) 0))
              (length classes))
         classes)))

(let ((keys (mapcar #'car ego::*scales*)))
  (defun next-mode (mode)
    (nth (mod (1+ (position mode keys)) (length keys))
         keys)))

(defmethod launchpad:handle-input :after ((server main) raw-midi)
  (trivia:match raw-midi
    ((list 176 104 0) (launchpad:button-automap-off 0))
    ((list 176 105 0) (launchpad:button-automap-off 1))
    ((list 176 106 0) (launchpad:button-automap-off 2))
    ((list 176 107 0) (launchpad:button-automap-off 3))
    ((list 176 108 0) (launchpad:button-automap-off 4))
    ((list 176 109 0) (launchpad:button-automap-off 5))
    ((list 176 110 0) (launchpad:button-automap-off 6))
    ((list 176 104 127) (launchpad:button-automap-on 0 (launchpad:color :lo)))
    ((list 176 105 127) (launchpad:button-automap-on 1 (launchpad:color :lo)))
    ((list 176 106 127) (launchpad:button-automap-on 2 (launchpad:color :lo)))
    ((list 176 107 127) (launchpad:button-automap-on 3 (launchpad:color :lo)))
    ((list 176 108 127) (launchpad:button-automap-on 4 (launchpad:color :lo)))
    ((list 176 109 127) (launchpad:button-automap-on 5 (launchpad:color :lo)))
    ((list 176 110 127) (launchpad:button-automap-on 6 (launchpad:color :lo)))))
