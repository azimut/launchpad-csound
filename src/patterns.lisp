(in-package #:launchpad-csound)

;; Scenes sharing a long pattern (?
;; TODO: special light color? root and intervals?
;; TODO: multiple touches/colors for different effects

(defvar *keys* nil)

(defclass patterns (launchpad scheduler)
  ((index  :initform 0
           :accessor index
           :documentation "Current SCENE index")
   (beats  :initform #(2.0 2.0 1.0 1.0 0.5 0.5 0.25 0.25)
           :reader   beats
           :documentation "Beat durations for each SCENE")))

(defun remove-key (&rest key)
  (sb-ext:with-locked-hash-table (*keys*)
    (remhash key *keys*)))
(defun add-key (&rest key)
  (multiple-value-bind (row col) (launchpad:xy (second key))
    (sb-ext:with-locked-hash-table (*keys*)
      (or (gethash key *keys*)
          (setf (gethash key *keys*) (list row col))))))

(defun schedule-all ()
  (let ((time (scheduler:sched-quant *scheduler* 2)))
    (dotimes (idx 8)
      (let ((duration (aref (beats *csound*) idx)))
        (eat time #'beat time idx duration (a:iota 8))))))

(defun init-keys-hash ()
  (if *keys*
      (clrhash *keys*)
      (setf *keys* (make-hash-table :test #'equal :synchronized t))))

(defmethod cloud:connect :after ((server patterns))
  (init-keys-hash)
  (launchpad:change-layout :xy)
  (schedule-all))

(defun relight-scene (new-scene to-color)
  (a:maphash-keys
   (lambda (key)
     (destructuring-bind (scene note) key
       (when (= scene new-scene)
         (launchpad:raw-command (if (zerop to-color) #x80 #x90)
                                note
                                to-color))))
   *keys*))

(defun has-keys-p (scene)
  (position scene (a:hash-table-keys *keys*)
            :key #'car
            :test #'=))

(defun relight-pages (old-scene)
  (if (has-keys-p old-scene)
      (launchpad:button-scene-xy-on  old-scene (launchpad:color :lg))
      (launchpad:button-scene-xy-off old-scene)))

(defmethod (setf index) :before (new-value (server patterns))
  (check-type new-value (integer 0 7))
  (relight-scene (slot-value server 'index) 0)
  (relight-pages (slot-value server 'index)))
(defmethod (setf index) (new-value (server patterns))
  (let ((old-value (slot-value *csound* 'index)))
    (atomics:cas (slot-value *csound* 'index) old-value new-value)))
(defmethod (setf index) :after (new-value (server patterns))
  (relight-scene new-value (launchpad:color :lg)))

(defun imap (scene)
  (max (case scene
         ((0 2) 3)
         ((7) 1)
         ((6) 4)
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

(let* ((scale (ego::scale 4 :phrygian))
       (cscale (cm:new cm:cycle :of scale)))
  (defun step-keys (stepping-scene stepping-column)
    (serapeum:do-hash-table (k v *keys*)
      (destructuring-bind (scene midi) k
        (destructuring-bind (row col) v
          (when (and (= scene stepping-scene)
                     (= col stepping-column))
            (cloud:schedule
             *csound*
             (iname (imap scene) midi)
             0
             (idur scene) ;;(+ (idur scene) (ego::cosr .2 .1 (+ scene)))
             #+nil
             (if (or (= scene 4))
                 (+ 60 (car (cm:next cscale (+ 1 row)))))
             (ego::pc-relative (+ 48 (* 24 (mod scene 2)))
                               row
                               scale)
             ;;(ego::rcosr 30 10 (+ 1 scene))
             20
             )))))))

(defun light-beat (time duration column)
  (eat time #'launchpad:button-automap-on column (launchpad:color :lg))
  (eat (+ duration time) #'launchpad:button-automap-off column))

(defun beat (time idx dur cycle)
  (let ((column (first cycle))
        (next-time (+ dur time)))
    (when (= idx (index *csound*))
      (light-beat time dur column))
    (eat time #'step-keys idx column)
    (eat next-time #'beat
         next-time idx dur (a:rotate (copy-seq cycle) -1))))

(defun key-pressed-p (key scene)
  (gethash (list scene key) *keys*))

(defmethod handle-input ((server patterns) raw-midi)
  (trivia:match raw-midi
    ((trivia:guard (list 144 key 127)
                   (or (= key   8)
                       (= key  24)
                       (= key  40)
                       (= key  56)
                       (= key  72)
                       (= key  88)
                       (= key 104)
                       (= key 120)))
     (progn (launchpad:raw-command #x90 key (launchpad:color :lo))
            (setf (index server) (launchpad:xy key))))
    ((list 144 key 127)
     (let ((scene (index server)))
       (if (key-pressed-p key scene)
           (progn (launchpad:raw-command #x80 key 0)
                  (remove-key scene key))
           (progn (launchpad:raw-command #x90 key (launchpad:color :lg))
                  (add-key scene key)))))))
