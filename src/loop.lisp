(in-package #:launchpad-csound)

(defclass launchpad (cloud::csound)
  ())

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
