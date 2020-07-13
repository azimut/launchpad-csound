(in-package #:launchpad-csound)

(defvar *preset*
  (list (a:iota 100)
        (a:iota 100)
        (a:iota 100)
        (a:iota 100)))

(defun change-program (channel program)
  (let ((msg (format nil "fluidProgramSelect giengine1, ~d, gisfnum1, 0, ~d"
                     channel
                     program)))
    (cloud:send *csound* msg)))

(defun prev-program (channel)
  (change-program
   channel
   (first (setf (elt *preset* channel) (a:rotate (copy-seq (elt *preset* channel)) -1)))))

(defun next-program (channel)
  (change-program
   channel
   (first (setf (elt *preset* channel) (a:rotate (copy-seq (elt *preset* channel)) +1)))))


