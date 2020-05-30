(in-package #:launchpad-csound)

(defvar *csound* (cloud:make-csound))
(cloud:connect *csound*)
(cloud:disconnect *csound*)
(cloud:send *csound* "fluidProgramSelect giengine, 0, gisfnum, 0, 52")
(launchpad:change-layout :drum)

(defun iname (note)
  (parse-float:parse-float
   (format nil "~d.~d" 1 note)))

(defun handle-input (raw-midi)
  ;;(print raw-midi)
  (trivia:match raw-midi
    ((list 144 note 127)
     (launchpad:raw-command (list 144 note (launchpad:color :lg)))
     (cloud:schedule *csound* (iname note) 0 60 note 60))
    ((list 144 note   0)
     (cloud:schedule *csound* (iname note) 0 0 note 0)
     (launchpad:raw-command (list 128 note 0)))))

(defun main-loop ()
  "IN debug print what is pressed"
  (cl-rtmidi::with-midi-oss-io ("/dev/midi1")
    (loop (handle-input
           (slot-value (cl-rtmidi:read-midi-message)
                       'cl-rtmidi::raw-midi))
          (force-output))))

;; (bt:make-thread #'main-loop :name "midi-input")
