(in-package #:launchpad-csound)

;; IDEA: split launchpad into two instruments
;;       show root keys
;;       show keys in KEY
;;       show accepted intervals for pressed note

(defvar *csound* (cloud:make-csound))

(defparameter *light-pressure* :lo)
(defparameter *light-root*     :ho)


(cl-punch:enable-punch-syntax)

(cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 52")
(cloud:send *csound* "fluidProgramSelect giengine1, 2, gisfnum1, 0, 34")

(defun iname (note)
  (let ((instrument (if (left-p note) 1 2)))
    (parse-float:parse-float
     (format nil "~d.~d" instrument note))))

(let ((root  8)
      (scale :dorian))
  (reset)
  (cl-rtmidi::with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (light-up root scale))
  (defun handle-input (raw-midi)
    (trivia:match raw-midi
      ((list 144 note 127)
       (progn (print (list note (mod note 12)))
              (launchpad:raw-command (list 144 note (launchpad:color *light-pressure*)))
              (cloud:schedule *csound* (iname note) 0 60 note 60)))
      ((list 144 note 0)
       (progn (launchpad:raw-command (list 128 note 0))
              (cloud:schedule *csound* (iname note) 0 0 note 0))
       (light-up root scale)))))

(defun main-loop ()
  "IN debug print what is pressed"
  (cl-rtmidi::with-midi-oss-io ("/dev/midi1")
    (loop (handle-input
           (slot-value (cl-rtmidi:read-midi-message) 'cl-rtmidi::raw-midi)))))

(defun alivep ()
  (when-let ((thread (find "midi-input" (bt:all-threads) :test #'string= :key #'bt:thread-name)))
    (bt:thread-alive-p thread)))

(defun start ()
  (when (not (alivep))
    (bt:make-thread #'main-loop :name "midi-input")))

(defun reset ()
  (start)
  (cloud:reconnect *csound*)
  (launchpad:reset)
  (launchpad:change-layout :drum))

;;--------------------------------------------------

(defun left-p (note)
  "T if left on drum layout"
  (<= 36 note 67))

(defun keys ()
  "list of keys on drum layout"
  (alexandria:iota (* 8 8) :start 36))

(defun get-roots (root)
  (remove-if-not ^(= root (mod _ 12))
                 (keys)))

(defun removed-roots (root scale)
  (let ((pc (remove root (ego::scale root scale))))
    (remove-if-not ^(member (mod _ 12) pc)
                   (keys))))

(defun light-up (root scale)
  (mapcar (lambda (_) (launchpad:raw-command (list 144 _ (launchpad:color *light-root*))))
          (get-roots root))
  (mapcar (lambda (_) (launchpad:raw-command (list 144 _ (launchpad:color :lg))))
          (removed-roots root scale)))
