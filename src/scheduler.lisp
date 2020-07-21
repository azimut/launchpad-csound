(in-package #:launchpad-csound)

;; NOTE: 1 global scheduler in a class???!...welp yes

(defclass scheduler ()
  ())

(defvar *scheduler* nil)

(defmacro at (time function &rest arguments)
  `(scheduler:sched-add
    *scheduler*
    (+ ,time (scheduler:sched-time *scheduler*))
    ,function ,@arguments))

(defmacro eat (time function &rest arguments)
  `(scheduler:sched-add
    *scheduler*
    ,time
    ,function ,@arguments))

(defmethod launchpad:connect :after ((server scheduler))
  (init-scheduler)
  (run-scheduler))

(defmethod launchpad:disconnect :after ((server scheduler))
  (reset-scheduler))

(defun init-scheduler ()
  (unless *scheduler*
    (setf *scheduler* (make-instance 'scheduler:scheduler))))

(defun run-scheduler ()
  (scheduler:sched-run *scheduler*))

(defun reset-scheduler ()
  (when *scheduler*
    (scheduler:sched-stop *scheduler*)
    (scheduler:sched-clear *scheduler*)))
