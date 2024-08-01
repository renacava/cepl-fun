(in-package #:cepl-fun)

(defparameter *vert-gpu-array* nil)
(defparameter *vert-array-buffer-stream* nil)

(defun init ()
  (setf *vert-gpu-array* (make-gpu-array
                        (list (v! -0.5 0.5)
                              (v! -0.5 -0.5)
                              (v! 0.5 -0.5)
                              (v! 0.5 0.5)))))

(defun main ())
