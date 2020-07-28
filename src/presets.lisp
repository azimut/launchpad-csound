(in-package #:launchpad-csound)

;; chorus/reverb
(defstruct chan
  (preset   (a:iota 110))
  (bank     (a:iota 2))
  (offset   (a:iota 10 :start 0 :step .1))
  (legato   (a:rotate (a:iota 20 :start .1 :step .1) 10))
  (muted    NIL)
  (velocity (a:rotate (a:iota 50 :start 20 :step 2) 25)))

(defvar *chans* nil)

(defun init-channels (&optional (nr 9))
  (unless *chans*
    (setf *chans* (->> (loop repeat nr collect (make-chan))
                       (make-array nr :initial-contents)))))

(defun change-program (server channel program)
  (let ((msg (format nil "fluidProgramSelect giengine1, ~d, gisfnum1, 0, ~d"
                     channel
                     program)))
    (cloud:send server msg)))

(defun prev-program (server channel)
  (change-program
   server
   channel
   (first
    (setf (chan-preset (aref *chans* (1- channel)))
          (a:rotate (copy-seq (chan-preset (aref *chans* (1- channel)))) +1)))))

(defun next-program (server channel)
  (change-program
   server
   channel
   (first
    (setf (chan-preset (aref *chans* (1- channel)))
          (a:rotate (copy-seq (chan-preset (aref *chans* (1- channel)))) -1)))))
