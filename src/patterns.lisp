(in-package #:launchpad-csound)
;; TODO: speedup, slowdown (might be beat might be overall tempo)
;; TODO: reset next operation on change-class and or change of scene
;; TODO: operation change to work on splitted (a subset at least)
;; TODO: Offset keys (will have to move all current keys)
;;       Might be don't delete keys outside grid
;; TODO: Special light color?
;;       Multiple touches/colors for different effects
;; TODO: Arpeggiator OR repeat key Nth times in timeslot
;;       Pick random in row
;;       Play 1 each Nth of cycles
;;       Random key prob of play, mutuallyy exclusive in row?
;;       Transpose each nth cycles
;; TODO: chan prob of drop a note
;; TODO: chain 2 channels with a "compressor" or just rsinr/rcosr
(defvar *keys* nil)

(defclass patterns (main)
  ((beats  :initform #(2.0 2.0 1.0 1.0 0.5 0.5 0.25 0.25)
           :reader   beats
           :documentation "Beat durations for each SCENE"))
  (:default-initargs
   :controls '(program bank reverb chorus velocity root mode offset delay legato)
   :layout :xy))

(defun relight-different (old new)
  (let ((muted (aref (slot-value *csound* 'muted) old))
        (playing (has-keys-p old)))
    (when (and (not playing) (not muted))
      (launchpad:button-scene-xy-off old))
    (when (and playing (not muted))
      (launchpad:button-scene-xy-on old (launchpad:color :lg)))
    (when muted
      (launchpad:button-scene-xy-on old (launchpad:color :lr))))
  (let ((muted (aref (slot-value *csound* 'muted) new)))
    (if muted
        (launchpad:button-scene-xy-on new (launchpad:color :lr))
        (launchpad:button-scene-xy-on new (launchpad:color :lo)))))

(defun relight-equal (scene)
  (let ((muted (aref (slot-value *csound* 'muted) scene)))
    (if muted
        (launchpad:button-scene-xy-on scene (launchpad:color :lo))
        (launchpad:button-scene-xy-on scene (launchpad:color :lr)))
    ;; flip
    (setf (aref (slot-value *csound* 'muted) scene)
          (not (aref (slot-value *csound* 'muted) scene)))))

(defun relight-page (scene)
  (let ((muted (aref (slot-value *csound* 'muted) scene))
        (playing (has-keys-p scene)))
    (when muted
      (launchpad:button-scene-xy-on scene (launchpad:color :lr)))
    (when (and playing (not muted))
      (launchpad:button-scene-xy-on scene (launchpad:color :lg)))))

(defmethod (setf idx) :around (new-value (server patterns))
  (if (= new-value (slot-value server 'idx)); un/mute if same
      (progn (relight-equal new-value))
      (progn (relight-different (slot-value server 'idx) new-value)
             (call-next-method))))
(defmethod (setf idx) :before (new-value (server patterns))
  (when (< (slot-value server 'idx) 8)
    (relight-scene (slot-value server 'idx) 0)))
(defmethod (setf idx) :after (new-value (server patterns))
  (relight-scene new-value (launchpad:color :lg)))

(defmethod change-class :after (obj (new (eql 'patterns)) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value obj 'controls) '(program bank reverb chorus velocity root mode offset delay legato))
  (setf (slot-value obj 'idx) 0)
  (launchpad:reset)
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (relight-scene 0 (launchpad:color :lg)))
  (mapcar #'relight-page (alexandria:iota 8))
  (setf (layout obj) :xy))

(defmethod launchpad:connect :after ((server patterns))
  (setf (layout server) :xy)
  (init-keys-hash)
  (schedule-all server))

(defun init-keys-hash ()
  (if *keys*
      (clrhash *keys*)
      (setf *keys* (make-hash-table :test #'equal :synchronized t))))
(defun has-keys-p (scene)
  (position scene (a:hash-table-keys *keys*)
            :key #'car
            :test #'=))
(defun key-pressed-p (key scene)
  (gethash (list scene key) *keys*))
(defun remove-key (&rest key)
  (sb-ext:with-locked-hash-table (*keys*)
    (remhash key *keys*)))
(defun add-key (&rest key)
  (multiple-value-bind (row col) (launchpad:xy (second key))
    (sb-ext:with-locked-hash-table (*keys*)
      (or (gethash key *keys*)
          (setf (gethash key *keys*) (list row col))))))

(defun schedule-all (server)
  (scheduler:sched-clear *scheduler*)
  (let ((time (scheduler:sched-quant *scheduler* 2)))
    (dotimes (idx 8)
      (let ((duration (aref (beats server) idx)))
        (eat time #'beat time idx duration (a:iota 8))))))

(defun relight-scene (new-scene to-color)
  (a:maphash-keys
   (lambda (key)
     (destructuring-bind (scene note) key
       (when (= scene new-scene)
         (launchpad:raw-command (if (zerop to-color) #x80 #x90)
                                note
                                to-color))))
   *keys*))

(defun ivel (scene)
  (case scene
    ((1) 60)
    (t 80)))
(defun idur (scene dur)
  (case scene
    ((1) 2)
    (t (* dur 1.7)))
  #+nil
  (case scene
    ((0 1) 1.8)
    ((2 3) 0.8)
    ((4 5) 0.4)
    (t 0.1)))

(defun step-keys (stepping-scene stepping-column scale dur vel legato delay offset)
  (serapeum:do-hash-table (k v *keys*)
    (destructuring-bind (scene midi) k
      (destructuring-bind (row col) v
        (when (and (= scene stepping-scene)
                   (= col stepping-column))
          (cloud:schedule
           *csound*
           (iname (1+ scene) midi)
           (+ 0 delay)
           (+ dur (* dur legato));;(+ (idur scene) (ego::cosr .2 .1 (+ scene)))
           #+nil
           (if (or (= scene 4))
               (+ 60 (car (cm:next cscale (+ 1 row)))))
           (ego::pc-relative (+ 48 (* 24 (mod scene 2)))
                             (+ row offset)
                             scale)
           ;;(ego::rcosr 15 5 (+ 1 scene))
           vel))))))

(defun light-beat (time duration column)
  (eat time #'launchpad:button-automap-on column (launchpad:color :lg))
  (eat (+ duration time) #'launchpad:button-automap-off column))

(defun beat (time idx dur cycle)
  (let ((column (first cycle))
        (next-time (+ dur time)))
    (when (= idx (idx *csound*))
      (light-beat time dur column))
    (when (not (aref (slot-value *csound* 'muted) idx))
      (eat time #'step-keys idx column
           (ego::scale (root *csound*) (mode *csound*))
           dur
           (first (aref (slot-value *csound* 'velocity) idx))
           (first (aref (slot-value *csound* 'legato)   idx))
           (first (aref (slot-value *csound* 'delay)    idx))
           (first (aref (slot-value *csound* 'offset)   idx))))
    (eat next-time #'beat
         next-time idx dur (a:rotate (copy-seq cycle) -1))))

(defun is-control (key)
  (member key '(8 24 40 56 72 88 104 120) :test #'=))

(defmethod launchpad:handle-input :after ((obj patterns) raw-midi)
  (let ((scene (idx obj)))
    (trivia:match raw-midi
      ((list 176 111 127) (change-class obj (next-class)))
      ((trivia:guard (list 144 key 127) (is-control key))
       (setf (idx obj) (launchpad:xy key)))
      ((trivia:guard (list 144 key 127) (not (is-control key)))
       (if (key-pressed-p key scene)
           (progn (launchpad:raw-command #x80 key 0)
                  (remove-key scene key))
           (progn (launchpad:raw-command #x90 key (launchpad:color :lg))
                  (add-key scene key)))))))

(defmethod inc-control progn ((obj patterns))
  (case (controls obj)
    (offset (next-offset obj))
    (delay  (next-delay  obj))
    (legato (next-legato obj))))

(defmethod dec-control progn ((obj patterns))
  (case (controls obj)
    (offset (prev-offset obj))
    (delay  (prev-delay  obj))
    (legato (prev-legato obj))))
