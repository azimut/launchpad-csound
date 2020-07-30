(in-package #:launchpad-csound)

;; TODO: record notes, quantize in a beat-slice
;; TODO: arpeggiator, customizable

(defclass splitted (main)
  ()
  (:default-initargs :layout :drum))

(defmethod (setf root) :before (new-root (obj splitted))
  (launchpad:reset)
  (launchpad:change-layout (layout obj))
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (light-up (first new-root) (mode obj) (layout obj))))

(defmethod (setf mode) :before (new-mode (obj splitted))
  (launchpad:reset)
  (launchpad:change-layout (layout obj))
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (light-up (root obj) (first new-mode) (layout obj))))

(defmethod change-class :after (obj (new (eql 'splitted)) &rest initargs)
  (declare (ignore initargs))
  (launchpad:reset)
  (launchpad:connect obj)
  (setf (slot-value obj 'idx) 8 )
  (setf (slot-value obj 'controls) '(program bank reverb chorus velocity root mode))
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (light-up (root obj) (mode obj) (layout obj))))

(defmethod launchpad:connect :after ((obj splitted))
  (setf (layout obj) :drum))

(cl-punch:enable-punch-syntax)

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
  (mapcar (lambda (_) (launchpad:raw-command 144 _ *light-root*))
          (get-roots root layout))
  (mapcar (lambda (_) (launchpad:raw-command 144 _ *light-scale*))
          (removed-roots root mode layout)))

(defmethod launchpad:handle-input :after ((obj splitted) raw-midi)
  (let ((chan (1+ (idx obj))))
    (trivia:match raw-midi
      ((list 176 111 127) (change-class obj (next-class)))
      ((trivia:guard (list 144 note 127) (> note 99))); ignore side
      ((list 144 note 127)
       (progn (launchpad:raw-command 144 note *light-pressure*)
              (cloud:schedule obj (iname chan note) 0 60 note (velocity obj))))
      ((list 144 note 0)
       (progn (launchpad:raw-command 128 note 0)
              (cloud:schedule obj (iname chan note) 0  0 note  0)
              (light-up (root obj) (mode obj) (layout obj)))))))
