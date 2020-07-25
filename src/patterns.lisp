(in-package #:launchpad-csound)

;; Scenes sharing a long pattern (?
;; TODO: special light color? root and intervals?
;; TODO: multiple touches/colors for different effects

(defvar *keys* nil)

(defclass patterns (main)
  ((index :initform 0
          :accessor index
          :documentation "Current SCENE index")
   (beats :initform #(2.0 2.0 1.0 1.0 0.5 0.5 0.25 0.25)
          :reader   beats
          :documentation "Beat durations for each SCENE"))
  (:default-initargs :layout :xy))

(defmethod (setf index) :around (new-value (server patterns))
  (when (not (= new-value (slot-value server 'index)))
    (call-next-method)))
(defmethod (setf index) :before (new-value (server patterns))
  (check-type new-value (integer 0 7))
  (relight-scene (slot-value server 'index) 0)
  (relight-pages (slot-value server 'index)))
(defmethod (setf index) :after (new-value (server patterns))
  (relight-scene new-value (launchpad:color :hg)))

(defmethod change-class :after (obj (new (eql 'patterns)) &rest initargs)
  (declare (ignore initargs))
  (launchpad:reset)
  (cl-rtmidi:with-midi-oss-out (cl-rtmidi:*default-midi-out-stream* "/dev/midi1")
    (relight-scene 0 (launchpad:color :hg)))
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

(defun relight-pages (old-scene)
  (if (has-keys-p old-scene)
      (launchpad:button-scene-xy-on  old-scene (launchpad:color :lg))
      (launchpad:button-scene-xy-off old-scene)))

(defun idur (scene)
  (case scene
    ((0 1) 1.7)
    ((2 3)  .7)
    (t .3)))

(let* (;;(scale (ego::scale 4 :phrygian))
       ;;(cscale (cm:new cm:cycle :of scale))
       )
  (defun step-keys (stepping-scene stepping-column scale)
    (serapeum:do-hash-table (k v *keys*)
      (destructuring-bind (scene midi) k
        (destructuring-bind (row col) v
          (when (and (= scene stepping-scene)
                     (= col stepping-column))
            (cloud:schedule
             *csound*
             1;;(iname (+ (mod scene 4) 1) midi)
             0
             (idur scene) ;;(+ (idur scene) (ego::cosr .2 .1 (+ scene)))
             #+nil
             (if (or (= scene 4))
                 (+ 60 (car (cm:next cscale (+ 1 row)))))
             (ego::pc-relative (+ 48 (* 24 (mod scene 2)))
                               row
                               scale)
             ;;(ego::rcosr 60 10 (+ 1 scene))
             10
             )))))))

(defun light-beat (time duration column)
  (eat time #'launchpad:button-automap-on column (launchpad:color :lg))
  (eat (+ duration time) #'launchpad:button-automap-off column))

(defun beat (time idx dur cycle)
  (let ((column (first cycle))
        (next-time (+ dur time)))
    (when (and (slot-exists-p *csound* 'index); work on change-class
               (= idx (index *csound*)))
      (light-beat time dur column))
    (eat time #'step-keys idx column
         (ego::scale (root *csound*) (mode *csound*)))
    (eat next-time #'beat
         next-time idx dur (a:rotate (copy-seq cycle) -1))))

(defun is-control (n)
  (member n '(8 24 40 56 72 88 104 120) :test #'=))

(defmethod launchpad:handle-input :after ((server patterns) raw-midi)
  (let ((chan (mod (index server) 4))
        (scene (index server)))
    (trivia:match raw-midi
      ((list 176 104 127) (prev-program server chan))
      ((list 176 105 127) (next-program server chan))
      ((list 176 111 127) (change-class server (next-class)))
      ((trivia:guard ; Scene Buttons
        (list 144 key 127) (is-control key))
       (progn (launchpad:raw-command #x90 key (launchpad:color :lo))
              (setf (index server) (launchpad:xy key))))
      ((trivia:guard ; Grid Buttons
        (list 144 key 127) (not (is-control key)))
       (if (key-pressed-p key scene)
           (progn (launchpad:raw-command #x80 key 0)
                  (remove-key scene key))
           (progn (launchpad:raw-command #x90 key (launchpad:color :lg))
                  (add-key scene key)))))))
