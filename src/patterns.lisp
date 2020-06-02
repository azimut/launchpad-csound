(in-package #:launchpad-csound)

;; Scenes sharing a long pattern (?
;; TODO: schedule ahead fix
;; TODO: side lights with state, show current pattern or if pattern has anything
;; TODO: special light color? root and intervals?
;; TODO: multiple touches/colors for different effects

(defun make-cycles ()
  (loop :repeat 8 :collect
        (cm:new cm:cycle :of (alexandria:iota 8))))

(defclass patterns (launchpad)
  ((index  :initform 0
           :accessor index
           :documentation "Current SCENE index")
   (beats  :initform '(2.0 2.0 1.0 1.0 0.5 0.5 0.5 0.5)
           :reader   beats
           :documentation "Beat durations for each SCENE")
   (cycles :initform (make-cycles)
           :reader   cycles)))

(defvar *scheduler* nil)
(defvar *keys* nil)

(defun remove-key (&rest key)
  (sb-ext:with-locked-hash-table (*keys*)
    (remhash key *keys*)))
(defun add-key (&rest key)
  (multiple-value-bind (row col) (launchpad:xy (second key))
    (sb-ext:with-locked-hash-table (*keys*)
      (or (gethash key *keys*)
          (setf (gethash key *keys*) (list row col))))))

(defun reset-cycles ()
  (dolist (cycle (cycles *csound*))
    (cm:next cycle t)))

(defun schedule-all ()
  (let ((time (scheduler:sched-quant *scheduler* 2)))
    (scheduler:sched-add *scheduler* time #'top-row-meter time)
    (dotimes (idx 8)
      (scheduler:sched-add *scheduler* time #'beat time idx))))

(defmethod cloud:disconnect :after ((server patterns))
  (when *scheduler*
    (scheduler:sched-stop *scheduler*)
    (scheduler:sched-clear *scheduler*))
  (reset-cycles))

(defmethod cloud:connect :after ((server patterns))
  (unless *scheduler*
    (setf *scheduler* (make-instance 'scheduler:scheduler)))
  (scheduler:sched-run *scheduler*)
  (launchpad:change-layout :xy)
  (if *keys*
      (clrhash *keys*)
      (setf *keys* (make-hash-table :test #'equal :synchronized t)))
  (schedule-all))

(defmacro at (time function &rest arguments)
  `(scheduler:sched-add
    *scheduler*
    (+ ,time (scheduler:sched-time *scheduler*))
    ,function ,@arguments))

(defun top-row-meter (time)
  (let ((next (nth (index *csound*) (beats *csound*)))
        (x    (+ 104 (cm:pattern-value
                      (nth (index *csound*) (cycles *csound*))))))
    (launchpad:command (list 176 x (launchpad:color :lg)))
    (at next #'launchpad:command (list 176 x 0))
    (at next #'top-row-meter (+ time next))))

(defun relight-scene (new-scene to-color)
  (alexandria:maphash-keys
   (lambda (key)
     (destructuring-bind (scene note) key
       (when (= scene new-scene)
         (launchpad:raw-command
          (list (if (zerop to-color) 128 144)
                note
                to-color)))))
   *keys*))

(defmethod (setf index) :before (new-value (server patterns))
  (check-type new-value (integer 0 7))
  (relight-scene (slot-value server 'index) 0))
(defmethod (setf index) (new-value (server patterns))
  (let ((old-value (slot-value *csound* 'index)))
    (atomics:cas (slot-value *csound* 'index) old-value new-value)))
(defmethod (setf index) :after (new-value (server patterns))
  (relight-scene new-value (launchpad:color :lg)))

(defun imap (scene)
  (max (case scene
         ((0 2) 3)
         ((6) 1)
         (t 2))
       1))

(defun idur (scene)
  (case scene
    ((0 1) 1.7)
    ((2 3)  .7)
    (t .3)))

;; LOGGING
#+nil
(defmethod cloud:schedule :before ((server cloud::csound) instrument &rest rest)
  (print rest))

(defun step-keys (stepping-scene stepping-column)
  (serapeum:do-hash-table (k v *keys*)
    (destructuring-bind (scene midi) k
      (destructuring-bind (row col) v
        (when (and (= col stepping-column)
                   (= scene stepping-scene))
          (cloud:schedule
           *csound*
           (iname (imap scene) midi)
           0
           (idur scene)
           ;;(+ 60 (car (cm:next scale (+ 1 row))))
           (ego::pc-relative (+ 48 (* 24 (mod scene 2)))
                             row
                             (ego::scale 0 :yo))
           25))))))

(defun beat (time idx)
  (let ((offset (nth idx (beats *csound*)))
        (column (cm:next (nth idx (cycles *csound*)))))
    (step-keys idx column)
    (scheduler:sched-add *scheduler* (+ offset time)
                         #'beat (+ offset time) idx)))

(defmethod handle-input ((server patterns) raw-midi)
  (trivia:match raw-midi
    ((trivia:guard (list 144 key 127)
                   (or (= key 8)
                       (= key 24)
                       (= key 40)
                       (= key 56)
                       (= key 72)
                       (= key 88)
                       (= key 104)
                       (= key 120)))
     (progn (launchpad:raw-command (list 144 key (launchpad:color :lg)))
            (setf (index server) (floor key 16))))
    ((trivia:guard (list 144 key 0)
                   (or (= key 8)
                       (= key 24)
                       (= key 40)
                       (= key 56)
                       (= key 72)
                       (= key 88)
                       (= key 104)
                       (= key 120)))
     (progn (launchpad:raw-command (list 128 key 0))))
    ((list 144 key 127)
     (let ((s (index server)))
       (if (gethash (list s key) *keys*)
           (progn (launchpad:raw-command (list 128 key 0))
                  (remove-key s key))
           (progn (launchpad:raw-command (list 144 key (launchpad:color :lg)))
                  (add-key s key)))))))


