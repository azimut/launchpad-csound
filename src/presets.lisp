(in-package #:launchpad-csound)

;; chorus/reverb/pan
(defstruct chan
  (preset   (a:iota 110))
  (bank     (a:iota 2))
  (offset   (a:iota 10 :start 0 :step .1))
  (legato   (a:rotate (a:iota 40 :start -.9 :step .1) -9))
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

(defun curr-velocity (chan) (first (chan-velocity (aref *chans* (1- chan)))))
(defun prev-velocity (nchan)
  (setf (chan-velocity (aref *chans* (1- nchan)))
        (alexandria:rotate (copy-seq (chan-velocity (aref *chans* (1- nchan)))) +1)))
(defun next-velocity (nchan)
  (setf (chan-velocity (aref *chans* (1- nchan)))
        (alexandria:rotate (copy-seq (chan-velocity (aref *chans* (1- nchan)))) -1)))

(defun curr-legato (chan) (first (chan-legato (aref *chans* (1- chan)))))
(defun prev-legato (nchan)
  (setf (chan-legato (aref *chans* (1- nchan)))
        (alexandria:rotate (copy-seq (chan-legato (aref *chans* (1- nchan)))) +1)))
(defun next-legato (nchan)
  (setf (chan-legato (aref *chans* (1- nchan)))
        (alexandria:rotate (copy-seq (chan-legato (aref *chans* (1- nchan)))) -1)))

(defun curr-offset (chan) (first (chan-offset (aref *chans* (1- chan)))))
(defun prev-offset (nchan)
  (setf (chan-offset (aref *chans* (1- nchan)))
        (alexandria:rotate (copy-seq (chan-offset (aref *chans* (1- nchan)))) +1)))
(defun next-offset (nchan)
  (setf (chan-offset (aref *chans* (1- nchan)))
        (alexandria:rotate (copy-seq (chan-offset (aref *chans* (1- nchan)))) -1)))

(let ((controls '(velocity legato offset)))
  (defun curr-control ()
    (first controls))
  (defun pop-control ()
    (setf controls (a:rotate (copy-seq controls)))
    (print (curr-control))
    (force-output)))

(defun next-control (chan)
  (case (curr-control)
    (velocity (next-velocity chan))
    (legato   (next-legato   chan))
    (offset   (next-offset   chan))))
(defun prev-control (chan)
  (case (curr-control)
    (velocity (prev-velocity chan))
    (legato   (prev-legato   chan))
    (offset   (prev-offset   chan))))
