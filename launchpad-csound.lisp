(in-package #:launchpad-csound)

;; IDEA: split launchpad into two instruments
;;       show root keys
;;       show keys in KEY
;;       show accepted intervals for pressed note

(defvar *csound* (cloud:make-csound))

(defparameter *light-pressure* :lo)
(defparameter *light-root*     :lr)

(cloud:connect *csound*)
(cloud:disconnect *csound*)
(cloud:send *csound* "fluidProgramSelect giengine1, 1, gisfnum1, 0, 53")
(cloud:send *csound* "fluidProgramSelect giengine2, 1, gisfnum2, 0, 93")

(defun reset ()
  (launchpad:reset)
  (launchpad:change-layout :drum))

(defun iname (note)
  (let ((instrument 2; (if (<= 36 note 67) 1 2)
            ))
    (parse-float:parse-float
     (format nil "~d.~d" instrument note))))

(defun handle-input (raw-midi)
  (light-root 3)
  (trivia:match raw-midi
    ((list 144 note 127)
     (progn (print (list note (mod note 12)))
            (launchpad:raw-command (list 144 note (launchpad:color *light-pressure*)))
            (cloud:schedule *csound* (iname note) 0 60 note 60)))
    ((list 144 note 0)
     (progn (launchpad:raw-command (list 128 note 0))
            (cloud:schedule *csound* (iname note) 0 0 note 0)))))

(defun main-loop ()
  "IN debug print what is pressed"
  (cl-rtmidi::with-midi-oss-io ("/dev/midi1")
    (loop (handle-input
           (slot-value (cl-rtmidi:read-midi-message)
                       'cl-rtmidi::raw-midi))
          (force-output))))

;; (bt:make-thread #'main-loop :name "midi-input")
(cl-punch:enable-punch-syntax)

(defun keys ()
  (alexandria:iota (* 8 8) :start 36))

(defun mod12keys ()
  (mapcar (lambda (x) (list (mod x 12) x))
          (keys)))

(defun root-keys (root)
  "> (root-keys 60)
  (36 48 60 72 84 96)"
  (let ((root12 (mod root 12)))
    (mapcar #'cadr (remove root12 (mod12keys)
                           :key #'car
                           :test-not #'=))))

(defun light-root (root)
  (mapcar (lambda (_) (launchpad:raw-command (list 144 _ (launchpad:color *light-root*))))
          (root-keys root)))
