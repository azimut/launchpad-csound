(in-package #:launchpad-csound)

(defvar *preset*
  (list (a:iota 100)
        (a:iota 100)
        (a:iota 100)
        (a:iota 100))
  "Global per channel program presets, needs persist between change-class")

(defun change-program (server channel program)
  (let ((msg (format nil "fluidProgramSelect giengine1, ~d, gisfnum1, 0, ~d"
                     channel
                     program)))
    (cloud:send server msg)))

(defun prev-program (server channel)
  (change-program
   server
   channel
   (first (setf (elt *preset* channel) (a:rotate (copy-seq (elt *preset* channel)) -1)))))

(defun next-program (server channel)
  (change-program
   server
   channel
   (first (setf (elt *preset* channel) (a:rotate (copy-seq (elt *preset* channel)) +1)))))


