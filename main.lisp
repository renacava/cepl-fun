(in-package #:cepl-fun)

(defparameter *vert-gpu-array* nil)
(defparameter *vert-gpu-index-array* nil)
(defparameter *vert-array-buffer-stream* nil)
(defparameter *projection-matrix* nil)

(defparameter cube-1 (list (v! -0.5 0.5 -0.5) ;;0   FRONT
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
                           ))

(defparameter cube-2 (list (v! -0.2 0.2 -0.2) ;;0   FRONT
                           (v! -0.2 -0.2 -0.2) ;;1
                           (v! 0.2 -0.2 -0.2) ;;2
                           (v! 0.2 0.2 -0.2) ;;3

                           (v! -0.2 0.2 0.2) ;;4   BACK
                           (v! 0.2 0.2 0.2) ;;5
                           (v! 0.2 -0.2 0.2) ;;6
                           (v! -0.2 -0.2 0.2) ;;7

                           (v! -0.2 0.2 -0.2) ;;8   LEFT
                           (v! -0.2 -0.2 -0.2) ;;9
                           (v! -0.2 -0.2 0.2) ;;10
                           (v! -0.2 0.2 0.2) ;;11

                           (v! 0.2 0.2 -0.2) ;;12   RIGHT
                           (v! 0.2 0.2 0.2) ;;13
                           (v! 0.2 -0.2 -0.2) ;;14
                           (v! 0.2 -0.2 0.2) ;;15

                           (v! -0.2 0.2 0.2) ;;16  TOP
                           (v! -0.2 0.2 -0.2) ;;17
                           (v! 0.2 0.2 -0.2) ;;18
                           (v! 0.2 0.2 0.2) ;;19

                           (v! -0.2 -0.2 0.2) ;;20  BOTTOM
                           (v! 0.2 -0.2 -0.2) ;;21
                           (v! -0.2 -0.2 -0.2) ;;22
                           (v! 0.2 -0.2 0.2) ;;23
                           ))

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun-g vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (rot :vec3)
                     ;;(rot-mat :mat4)
                     )
  (let* ((pos (* (rtg-math.matrix4:rotation-from-euler rot) (vec4 vert 1)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 0))))
    (values (* proj pos)
            vert)))

(defun-g frag-stage ((col :vec3))
  (let ((col (+ col (vec3 0.5 0.5 0.5))))
    (vec4 col 1)))

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage :vec3))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defun init ()
  (try-free-objects *vert-gpu-array* *vert-gpu-index-array* *vert-array-buffer-stream*)
  
  (setf *vert-gpu-index-array* (make-gpu-array (list 2 1 0 3 2 0
                                                     6 5 4 7 6 4
                                                     9 10 8 10 11 8
                                                     15 14 12 13 15 12
                                                     18 17 16 19 18 16
                                                     22 21 20 21 23 20)
                                               :element-type :uint))
  (setf *vert-gpu-array* (make-gpu-array
                          cube-2))
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
         :rot (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now))))
  (step-host)
  (swap))


(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 
                                 (livesupport:update-repl-link)
                                 (step-rendering)
                                 (step-host)
                                 ;;(sleep 0.025)
                                 )))

(defun main ()
  (cepl:repl)
  (init)
  (loop (funcall main-loop-func)))

