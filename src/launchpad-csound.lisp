(in-package #:launchpad-csound)

(defvar *csound* nil)

;; (cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 0")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 2, gisfnum1, 0, 42")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 3, gisfnum1, 0, 40")
;; (cloud:send *csound* "fluidProgramSelect giengine1, 4, gisfnum1, 0, 8")

(defclass launchpad (cloud::csound)
  ())

(defmethod cloud:disconnect :after ((server launchpad))
  (stop-io-thread))

(defmethod cloud:connect :before ((server launchpad))
  (start-io-thread))

(defmethod cloud:reconnect :after ((server launchpad))
  (launchpad:reset))

(defgeneric handle-input (server raw-midi))

(defun main-loop ()
  "IN debug print what is pressed"
  (cl-rtmidi::with-midi-oss-io ("/dev/midi1")
    (loop (handle-input *csound* (slot-value (cl-rtmidi:read-midi-message) 'cl-rtmidi::raw-midi)))))

(defun get-io-thread ()
  (find "midi-input" (bt:all-threads)
        :key #'bt:thread-name
        :test #'string=))

(defun alivep ()
  (alexandria:when-let ((thread (get-io-thread)))
    (bt:thread-alive-p thread)))

(defun start-io-thread ()
  (when (not (alivep))
    (bt:make-thread #'main-loop :name "midi-input")))

(defun stop-io-thread ()
  (when (alivep)
    (bt:destroy-thread (get-io-thread))))

(defun iname (instr dot)
  (parse-float:parse-float
   (format nil "~d.~d" instr dot)))
