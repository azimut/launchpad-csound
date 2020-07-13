(in-package #:launchpad-csound)

(defclass splitted (main)
  ()
  (:default-initargs :layout :drum))

(defmethod change-class :after (obj (new (eql 'splitted)) &rest initargs)
  (declare (ignore initargs))
  (launchpad:reset)
  (launchpad:connect obj)
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (light-up (root obj) (mode obj) (layout obj))))

(defmethod launchpad:connect :after ((obj splitted))
  (setf (layout obj) :drum))

(cl-punch:enable-punch-syntax)

(defparameter *light-pressure* :lo)
(defparameter *light-root*     :ho)
(defparameter *light-scale*    :lg)

(defun left-p (note)
  "T if left on drum layout"
  (<= 36 note 67))

(defun get-roots (root layout)
  (remove-if-not ^(= (mod _ 12) (mod root 12))
                 (launchpad:get-keys layout)))

(defun removed-roots (root mode layout)
  (let ((pc (remove root (ego::scale root mode))))
    (remove-if-not ^(member (mod _ 12) pc)
                   (launchpad:get-keys layout))))

(defun light-up (root mode layout)
  (mapcar (lambda (_) (launchpad:raw-command 144 _ (launchpad:color *light-root*)))
          (get-roots root layout))
  (mapcar (lambda (_) (launchpad:raw-command 144 _ (launchpad:color *light-scale*)))
          (removed-roots root mode layout)))

(defparameter *preset*
  (list (a:iota 100)
        (a:iota 100)
        (a:iota 100)
        (a:iota 100)))

(defmethod launchpad:handle-input :after ((server splitted) raw-midi)
  (let ((i 3))
    (trivia:match raw-midi
      ((list 176 104 127) (cloud:schedule *csound* 101 0 .1 i (car (setf (elt *preset* i) (a:rotate (copy-seq (elt *preset* i)) -1)))))
      ((list 176 105 127) (cloud:schedule *csound* 101 0 .1 i (car (setf (elt *preset* i) (a:rotate (copy-seq (elt *preset* i)) +1)))))
      ((list 144 100 127) (change-class server (next-class)))
      ((list 144 note 127)
       (progn (launchpad:raw-command 144 note (launchpad:color *light-pressure*))
              (cloud:schedule server (iname i note) 0 60 note (cm:between 60 64))))
      ((list 144 note 0)
       (progn (launchpad:raw-command 128 note 0)
              (cloud:schedule server (iname i note) 0  0 note  0))
       (light-up (root server) (mode server) (layout server))))))
