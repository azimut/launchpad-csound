(in-package #:launchpad-csound)

;; (cloud:connect   *csound*)
;; (cloud:reconnect *csound*)

(defvar *scheduler* (make-instance 'scheduler:scheduler))
(defvar *keys* (make-hash-table))

(defmacro at (time function &rest arguments)
  `(scheduler:sched-add *scheduler*
                        (+ ,time (scheduler:sched-time *scheduler*))
                        ,function ,@arguments))

(defun top-row (cell)
  (let ((x (+ 104 cell)))
    (launchpad:command (list 176 x (launchpad:color :lg)))
    (at .5 #'launchpad:command (list 176 x (launchpad:color :off)))))

(defun beat (&optional (cells '(0 1 2 3 4 5 6 7)))
  (let ((col (first cells)))
    (top-row col)
    (step-keys col))
  (at .5 #'beat (rotate cells -1)))

(defun iname-step (row)
  (parse-float:parse-float
   (format nil "~d.~d" 2 (+ row 1))))
(cloud:reconnect *csound*)
(defun step-keys (current-col)
  (alexandria:maphash-values
   (lambda (col-and-row)
     (destructuring-bind (row col) col-and-row
       (when (= col current-col)
         (cloud:schedule *csound*
                         (iname-step row)
                         0
                         (+ .1 (random .05))
                         (+ 60 (nth row (ego::scale 0 :minor)))
                         40))))
   *keys*))

(defmethod cloud:connect :after ((server patterns))
  (launchpad:change-layout :xy)
  (scheduler:sched-clear *scheduler*)
  (scheduler:sched-run *scheduler*)
  (clrhash *keys*))

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
