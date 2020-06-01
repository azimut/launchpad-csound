(in-package #:launchpad-csound)

;; (cloud:connect   *csound*)
;; (cloud:reconnect *csound*)

(defclass patterns (launchpad)
  ())

(defvar *scheduler* (make-instance 'scheduler:scheduler))
(defvar *keys* (make-hash-table :synchronized t))

(defmethod cloud:connect :after ((server patterns))
  (launchpad:change-layout :xy)
  (scheduler:sched-clear *scheduler*)
  (scheduler:sched-run *scheduler*)
  (clrhash *keys*))

(defmacro at (time function &rest arguments)
  `(scheduler:sched-add *scheduler*
                        (+ ,time (scheduler:sched-time *scheduler*))
                        ,function ,@arguments))


(defun iname-step (row)
  (parse-float:parse-float
   (format nil "~d.~d" 2 (+ row 1))))

(defun top-row (cell)
  (let ((x (+ 104 cell)))
    (launchpad:command (list 176 x (launchpad:color :lg)))
    (at .5 #'launchpad:command (list 176 x (launchpad:color :off)))))

(let ((scale (cm:new cm:cycle :of (ego::scale 0 :minor))))
  (defun step-keys (current-col)
    (alexandria:maphash-values
     (lambda (col-and-row)
       (destructuring-bind (row col) col-and-row
         (when (= col current-col)
           (cloud:schedule *csound*
                           (iname-step row)
                           0
                           .15
                           ;;(+ 60 (car (cm:next scale (+ 1 row))))
                           (ego::pc-relative 60 row (ego::scale 0 :aeolian))
                           (ego::rcosr 40 5 5)))))
     *keys*)))

(let ((cols (cm:new cm:cycle :of '(0 1 2 3 4 5 6 7))))
  (defun beat ()
    (let ((col (cm:next cols)))
      (top-row col)
      (step-keys col))
    (at .5 #'beat)))

(defun add-key (key)
  (unless (gethash key *keys*)
    (multiple-value-bind (col row) (launchpad:xy key)
      (setf (gethash key *keys*) (list col row)))))

(defun remove-key (key)
  (remhash key *keys*))

(defmethod handle-input ((server patterns) raw-midi)
  (trivia:match raw-midi
    ((list 144 key 127)
     (if (gethash key *keys*)
         (progn
           (launchpad:raw-command (list 128 key 0))
           (remove-key key))
         (progn
           (launchpad:raw-command (list 144 key (launchpad:color :lg)))
           (add-key key))))))


