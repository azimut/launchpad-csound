(in-package #:launchpad-csound)

;; TODO: emm...kind if had an idea, but got weird, hence the (case)

(defparameter *light-pressure* (launchpad:color :lo))
(defparameter *light-root*     (launchpad:color :ho))
(defparameter *light-scale*    (launchpad:color :lg))

(let ((classes '(patterns splitted)))
  (defun next-class ()
    (nth (mod (1+ (or (position (class-name (class-of *csound*)) classes :test #'eql) 0))
              (length classes))
         classes)))

(defun iname (instr dot)
  (parse-float:parse-float
   (format nil "~d.~d" instr dot)))

(defvar *csound* nil
  "csound/launchpad server, needed at times globally for different threads")

(defclass main (launchpad::controller cloud::udp scheduler)
  ((root     :initarg :root     :accessor root     :documentation "Pitch Class root note")
   (mode     :initarg :mode     :accessor mode     :documentation "Pitch Class :name")
   (layout   :initarg :layout   :accessor layout   :documentation "Launchpad layout")
   (program  :initarg :program  :accessor program)
   (bank     :initarg :bank     :accessor bank)
   (pan      :initarg :pan      :accessor pan)
   (reverb   :initarg :reverb   :accessor reverb)
   (chorus   :initarg :chorus   :accessor chorus)
   (muted    :initarg :muted    :accessor muted)
   (velocity :initarg :velocity :accessor velocity)
   (idx      :initarg :idx      :accessor idx)
   (controls :initarg :controls :accessor controls)
   (offset   :initarg :offset   :accessor offset)
   (delay    :initarg :delay    :accessor delay)
   (legato   :initarg :legato   :accessor legato))
  (:default-initargs
   :idx      8
   :controls '(program bank reverb chorus velocity root mode)
   :legato   (make-array 9 :initial-contents (loop repeat 9 collect (a:rotate (a:iota 40 :start -.9 :step .1) -9)))
   :delay    (make-array 9 :initial-contents (loop repeat 9 collect (a:iota 10 :start 0 :step .1)))
   :offset   (make-array 9 :initial-contents (loop repeat 9 collect (a:rotate (a:iota 23 :start -11) -11)))
   :program  (make-array 9 :initial-contents (loop repeat 9 collect (a:iota 110)))
   :bank     (make-array 9 :initial-contents (loop repeat 9 collect '(0 1 2 128)))
   :pan      (make-array 9 :initial-contents (loop repeat 9 collect (a:rotate (a:iota 43 :step 3) 22)))
   :reverb   (make-array 9 :initial-contents (loop repeat 9 collect (a:rotate (a:iota 43 :step 3) 22)))
   :chorus   (make-array 9 :initial-contents (loop repeat 9 collect (a:rotate (a:iota 43 :step 3) 22)))
   :muted    (make-array 9 :initial-contents (loop repeat 9 collect NIL))
   :velocity (make-array 9 :initial-contents (loop repeat 9 collect (a:rotate (a:iota 50 :start 20 :step 2) 25)))
   :root     (a:iota 12)
   :mode     (mapcar #'car ego::*scales*)
   :layout :drum))

(defmethod (setf layout) :before (new-layout (obj main))
  (assert (member new-layout '(:drum :xy))
          (new-layout) "Invalid Launchpad Layout")
  (launchpad:change-layout new-layout))

(defmethod root ((obj main)) (first (slot-value obj 'root)))
(defmethod prev-root ((obj main))
  (print (first (setf (root obj) (alexandria:rotate (copy-seq (slot-value obj 'root)) +1))))
  (force-output))
(defmethod next-root ((obj main))
  (print (first (setf (root obj) (alexandria:rotate (copy-seq (slot-value obj 'root)) -1))))
  (force-output))

(defmethod mode ((obj main)) (first (slot-value obj 'mode)))
(defmethod prev-mode ((obj main))
  (print (first (setf (mode obj) (alexandria:rotate (copy-seq (slot-value obj 'mode)) +1))))
  (force-output))
(defmethod next-mode ((obj main))
  (print (first (setf (mode obj) (alexandria:rotate (copy-seq (slot-value obj 'mode)) -1))))
  (force-output))

(defmethod controls ((obj main)) (first (slot-value obj 'controls)))
(defmethod prev-controls ((obj main))
  (print (first (setf (controls obj) (alexandria:rotate (copy-seq (slot-value obj 'controls)) +1))))
  (force-output))
(defmethod next-controls ((obj main))
  (print (first (setf (controls obj) (alexandria:rotate (copy-seq (slot-value obj 'controls)) -1))))
  (force-output))

(defgeneric inc-control (obj) (:method-combination progn))
(defgeneric dec-control (obj) (:method-combination progn))

(defmethod inc-control progn ((obj main))
  (case (controls obj)
    (program  (next-program  obj))
    (bank     (next-bank     obj))
    (reverb   (next-reverb   obj))
    (chorus   (next-chorus   obj))
    (velocity (next-velocity obj))
    (root     (next-root     obj))
    (mode     (next-mode     obj))))

(defmethod dec-control progn ((obj main))
  (case (controls obj)
    (program  (prev-program  obj))
    (bank     (prev-bank     obj))
    (reverb   (prev-reverb   obj))
    (chorus   (prev-chorus   obj))
    (velocity (prev-velocity obj))
    (root     (prev-root     obj))
    (mode     (prev-mode     obj))))

(defmethod launchpad:handle-input :after ((obj main) raw-midi)
  (trivia:match raw-midi
    ((list 176 104 000) (launchpad:button-automap-off 0))
    ((list 176 105 000) (launchpad:button-automap-off 1))
    ((list 176 106 000) (launchpad:button-automap-off 2))
    ((list 176 107 000) (launchpad:button-automap-off 3))
    ((list 176 108 000) (launchpad:button-automap-off 4))
    ((list 176 109 000) (launchpad:button-automap-off 5))
    ((list 176 110 000) (launchpad:button-automap-off 6))
    ((list 176 111 000) (launchpad:button-automap-off 7))
    ((list 176 104 127) (launchpad:button-automap-on  0 #x10) (dec-control   obj))
    ((list 176 105 127) (launchpad:button-automap-on  1 #x10) (inc-control   obj))
    ((list 176 106 127) (launchpad:button-automap-on  2 #x10) (prev-controls obj))
    ((list 176 107 127) (launchpad:button-automap-on  3 #x10) (next-controls obj))
    ((list 176 108 127) (launchpad:button-automap-on  4 #x10))
    ((list 176 109 127) (launchpad:button-automap-on  5 #x10))
    ((list 176 110 127) (launchpad:button-automap-on  6 #x10))
    ((list 176 111 127) (launchpad:button-automap-on  7 #x10)
     ;;(change-class obj (next-class))
     )))

(defmacro defrotation (name class-name)
  "defines a reader, and rotation for list of values at class"
  (let ((prev (intern (format nil "~A-~A" 'prev name)))
        (next (intern (format nil "~A-~A" 'next name))))
    `(progn
       (defmethod ,name ((obj ,class-name)) (first (aref (slot-value obj (quote ,name)) (idx obj))))
       (defmethod ,prev ((obj main))
         (setf (aref (slot-value obj (quote ,name)) (idx obj))
               (alexandria:rotate (copy-seq (aref (slot-value obj (quote ,name)) (idx obj))) +1))
         (send obj (,name obj)))
       (defmethod ,next ((obj main))
         (setf (aref (slot-value obj (quote ,name)) (idx obj))
               (alexandria:rotate (copy-seq (aref (slot-value obj (quote ,name)) (idx obj))) -1))
         (send obj (,name obj))))))

(defmethod send ((obj main) value)
  (print value)
  (force-output)
  (case (controls obj)
    (program (change-program obj (1+ (idx obj)) (bank obj) value))
    (bank    (change-program obj (1+ (idx obj)) value (program obj)))
    (pan     (change-cc obj (1+ (idx obj)) 10 value))
    (reverb  (change-cc obj (1+ (idx obj)) 91 value))
    (chorus  (change-cc obj (1+ (idx obj)) 93 value))))

(defrotation velocity main)
(defrotation muted    main)
(defrotation chorus   main)
(defrotation reverb   main)
(defrotation pan      main)
(defrotation bank     main)
(defrotation program  main)
(defrotation offset   main)
(defrotation delay    main)
(defrotation legato   main)

(defun change-program (server chan bank program)
  (cloud:send server (format nil "fluidProgramSelect giengine1, ~d, gisfnum1, ~d, ~d"
                             chan bank program)))

(defun change-cc (server chan cc value)
  (cloud:send server (format nil "fluidCCi giengine1, ~d,~d,~d" chan cc value)))
