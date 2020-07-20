(in-package #:launchpad-csound)

;; TODO: lights, handler, key to filename/instrument

(defvar *file-to-instrument* nil
  "Hash that maps filename with instrument number, no multiple instances of the same filename are expected.")
(defvar *file-instruments*  0)

(defvar *sampler-instrument*
  "instr ~d
     ibcount active ~d
     if (ibcount == 1) then
       a1,a2 diskin2 ~s, p4, p5, p6, 0, 32
       aout  linseg  0, 2, p7
             outs    a1 * aout, a2 * aout
     else
       turnoff
     endif
   endin")

(defun next-instrument (filename)
  (or (gethash filename *file-to-instrument*)
      (setf (gethash filename *file-to-instrument*) (incf *file-instruments*))))

(defun format-instr (filename)
  (let ((n (next-instrument filename)))
    (cloud:send *csound* (format nil *sampler-instrument* n n filename))))

(defclass sampler (main)
  ((dir :initform (error "dir needs to be defined to a directory with samples")
        :documentation "read .wav samples from this directory"
        :accessor dir :initarg :dir)))

(defun init-hash ()
  (setf *file-to-instrument* (make-hash-table :test #'equal)))

(defun reset-hash ()
  (alexandria:maphash-values (lambda (i) (cloud:send *csound* (format nil "delete ~d" i))) *file-instruments*)
  (clrhash *file-instruments*))

(defmethod initialize-instance :before ((obj sampler) &key)
  (if *file-to-instrument*
      (reset-hash)
      (init-hash)))

(defmethod initialize-instance :after ((obj sampler) &key dir)
  (setf (dir obj) dir))

(defmethod (setf dir) :before (new-dir (obj sampler))
  (assert (uiop:directory-exists-p new-dir))
  (assert (plusp (length (process-dir new-dir)))))

(defun process-dir (dir)
  (let ((*default-pathname-defaults* (pathname dir)))
    (append (directory "*.wav")
            (directory "*.WAV"))))

(defun make-sampler (dir)
  (make-instance 'sampler :dir dir))
