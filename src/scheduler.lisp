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

(defmethod cloud:disconnect :after ((server scheduler))
  (when *scheduler*
    (scheduler:sched-stop *scheduler*)
    (scheduler:sched-clear *scheduler*)))

(defmethod cloud:connect :after ((server scheduler))
  (unless *scheduler*
    (setf *scheduler* (make-instance 'scheduler:scheduler)))
  (scheduler:sched-run *scheduler*))
