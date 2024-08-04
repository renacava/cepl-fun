(in-package #:cepl-fun)

(defparameter *vert-gpu-array* nil)
(defparameter *vert-gpu-index-array* nil)
(defparameter *vert-array-buffer-stream* nil)

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun init ()
  (try-free-objects *vert-gpu-array* *vert-gpu-index-array* *vert-array-buffer-stream*)
  
  (setf *vert-gpu-index-array* (make-gpu-array (list 0 1 2 0 2 3)))
  (setf *vert-gpu-array* (make-gpu-array
                          (list (v! -0.5 0.5)
                                (v! -0.5 -0.5)
                                (v! 0.5 -0.5)
                                (v! 0.5 0.5))))
  (setf *vert-array-buffer-stream* (make-buffer-stream *vert-gpu-array* :index-array *vert-gpu-index-array*)))

(defun main ())
