(in-package #:cepl-fun)

(defparameter *vert-gpu-array* nil)
(defparameter *vert-gpu-index-array* nil)
(defparameter *vert-array-buffer-stream* nil)
(defparameter *projection-matrix* nil)

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun-g vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (rot-mat :mat4))
  (let* ((pos (* rot-mat (vec4 vert 0)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 1)))
         ;;(pos (* rot-mat (vec4 pos 1)))
         )
    (* proj pos)))

(defun-g frag-stage ()
  (vec4 1 0 0 1))

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defun init ()
  ;;(cepl:quit)
  ;;(cepl:repl)
  (try-free-objects *vert-gpu-array* *vert-gpu-index-array* *vert-array-buffer-stream*)
  
  (setf *vert-gpu-index-array* (make-gpu-array (list 0 1 2 0 2 3
                                                     4 5 6 4 6 7
                                                     8 10 9 8 11 10
                                                     12 14 15 12 15 13
                                                     16 17 18 16 18 19
                                                     20 21 22 20 23 21)
                                               :element-type :uint))
  (setf *vert-gpu-array* (make-gpu-array
                          (list (v! -0.5 0.5 -0.5) ;;0   FRONT
                                (v! -0.5 -0.5 -0.5) ;;1
                                (v! 0.5 -0.5 -0.5) ;;2
                                (v! 0.5 0.5 -0.5) ;;3

                                (v! -0.5 0.5 0.5) ;;4   BACK
                                (v! 0.5 0.5 0.5) ;;5
                                (v! 0.5 -0.5 0.5) ;;6
                                (v! -0.5 -0.5 0.5) ;;7

                                (v! -0.5 0.5 -0.5) ;;8   LEFT
                                (v! -0.5 -0.5 -0.5) ;;9
                                (v! -0.5 -0.5 0.5) ;;10
                                (v! -0.5 0.5 0.5) ;;11

                                (v! 0.5 0.5 -0.5) ;;12   RIGHT
                                (v! 0.5 0.5 0.5) ;;13
                                (v! 0.5 -0.5 -0.5) ;;14
                                (v! 0.5 -0.5 0.5) ;;15

                                (v! -0.5 0.5 0.5) ;;16  TOP
                                (v! -0.5 0.5 -0.5) ;;17
                                (v! 0.5 0.5 -0.5) ;;18
                                (v! 0.5 0.5 0.5) ;;19

                                (v! -0.5 -0.5 0.5) ;;20  BOTTOM
                                (v! 0.5 -0.5 -0.5) ;;21
                                (v! -0.5 -0.5 -0.5) ;;22
                                (v! 0.5 -0.5 0.5) ;;23
                                )))
  (setf *vert-array-buffer-stream* (make-buffer-stream *vert-gpu-array* :index-array *vert-gpu-index-array*))
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                              (y (resolution (current-viewport)))
                                                              0.1
                                                              30f0
                                                              60f0)))

(defun step-rendering ()
  (clear)
  (map-g #'basic-pipeline *vert-array-buffer-stream*
         :now (now)
         :proj *projection-matrix*
         :rot-mat (rtg-math.matrix4:rotation-from-euler (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now)))))
  (step-host)
  (swap)
  )


(defparameter main-loop-func (lambda ()
                               (step-rendering)
                               ;;(step-host)
                               (sleep 0.025)
                               ))

(defun main ()
  (cepl:repl)
  (init)
  (loop (funcall main-loop-func)))
