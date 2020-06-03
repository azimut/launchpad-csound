(in-package #:launchpad-csound)

(defclass splitted (launchpad)
  ())

(defmethod cloud:connect :after ((server splitted))
  (launchpad:change-layout :drum))

(cl-punch:enable-punch-syntax)

(defparameter *light-pressure* :lo)
(defparameter *light-root*     :ho)
(defparameter *light-scale*    :lg)

(defun left-p (note)
  "T if left on drum layout"
  (<= 36 note 67))

(defun get-roots (root)
  (remove-if-not ^(= (mod _ 12) (mod root 12))
                 (launchpad:get-keys :drum)))

(defun removed-roots (root scale)
  (let ((pc (remove root (ego::scale root scale))))
    (remove-if-not ^(member (mod _ 12) pc)
                   (launchpad:get-keys :drum))))

(defun light-up (root scale)
  (mapcar (lambda (_) (launchpad:raw-command
                  (list 144 _ (launchpad:color *light-root*))))
          (get-roots root))
  (mapcar (lambda (_) (launchpad:raw-command
                  (list 144 _ (launchpad:color *light-scale*))))
          (removed-roots root scale)))

(let ((root  3)
      (scale :minor))
  ;; (cloud:reconnect *csound*)
  ;; (cl-rtmidi::with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
  ;;   (light-up root scale))
  (defmethod handle-input ((server splitted) raw-midi)
    (trivia:match raw-midi
      ((list 144 note 127)
       (progn (print (list note (mod note 12)))
              (launchpad:raw-command (list 144 note (launchpad:color *light-pressure*)))
              (cloud:schedule server (iname 1 note) 0 60 note 60)))
      ((list 144 note 0)
       (progn (launchpad:raw-command (list 128 note 0))
              (cloud:schedule server (iname 1 note) 0 0 note 0))
       (light-up root scale)))))
