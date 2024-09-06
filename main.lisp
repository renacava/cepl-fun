(in-package #:cepl-fun)

(defmacro without-depth (&body body)
  `(progn
     (gl:disable :depth-test)
     ,@body
     (gl:enable :depth-test)))

(defmacro setf-if-nil (place value)
  `(unless ,place
     (setf ,place ,value)))

(defparameter *vert-gpu-array* nil)
(defparameter *vert-gpu-index-array* nil)
(defparameter *vert-array-buffer-stream* nil)
(defparameter *projection-matrix* nil)
(defparameter *transform-feedback-gpu-array* nil)
(defparameter *transform-feedback-stream* nil)
 
(defparameter cube-1 (list (vec3 0.0 1.0 0.0) ;;0   FRONT
                           (vec3 0.0 0.0 0.0) ;;1
                           (vec3 1.0 0.0 0.0) ;;2
                           (vec3 1.0 1.0 0.0) ;;3

                           (vec3 0.0 1.0 1.0) ;;4   BACK
                           (vec3 1.0 1.0 1.0) ;;5
                           (vec3 1.0 0.0 1.0) ;;6
                           (vec3 0.0 0.0 1.0) ;;7

                           (vec3 0.0 1.0 0.0) ;;8   LEFT
                           (vec3 0.0 0.0 0.0) ;;9
                           (vec3 0.0 0.0 1.0) ;;1.00
                           (vec3 0.0 1.0 1.0) ;;1.0

                           (vec3 1.0 1.0 0.0) ;;1.02   RIGHT
                           (vec3 1.0 1.0 1.0) ;;1.03
                           (vec3 1.0 0.0 0.0) ;;1.04
                           (vec3 1.0 0.0 1.0) ;;1.05

                           (vec3 0.0 1.0 1.0) ;;1.06  TOP
                           (vec3 0.0 1.0 0.0) ;;1.07
                           (vec3 1.0 1.0 0.0) ;;1.08
                           (vec3 1.0 1.0 1.0) ;;1.09

                           (vec3 0.0 0.0 1.0) ;;20  BOTTOM
                           (vec3 1.0 0.0 0.0) ;;21.0
                           (vec3 0.0 0.0 0.0) ;;22
                           (vec3 1.0 0.0 1.0) ;;23
                           ))

(defparameter plane-1 (list (list (vec3 -1.0 -1.0 0.0) (vec2 0.0 0.0))
                            (list (vec3 1.0 -1.0 0.0) (vec2 1.0 0.0))
                            (list (vec3 1.0 1.0 0.0) (vec2 1.0 1.0))
                            (list (vec3 -1.0 1.0 0.0) (vec2 0.0 1.0))))

(defparameter plane-indices (vector 0 1 2 0 2 3))

(defparameter plane-vert-array nil)
(defparameter plane-index-array nil)
(defparameter plane-buffer-stream nil)

(defun try-free (object)
  (when object (free object)))

(defun try-free-objects (&rest objects)
  (mapcar #'try-free objects))

(defun-g vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (rot :vec3))
  (let* ((pos (* (rtg-math.matrix4:rotation-from-euler rot) (vec4 vert 1)))
         (col (if (or (isinf (aref pos 0))
                      (isinf (aref pos 1))
                      (isinf (aref pos 2)))
                  (vec3 1.0 0.0 0.0)
                  (vec3 0.0 0.0 1.0)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 0))))
    (values (* proj pos)
            (:smooth (vec3 (aref vert 0)
                           (aref vert 1)
                           (aref vert 2)))
            (:flat (ivec4 (int (aref pos 0))
                                    (int (aref pos 1))
                                    (int (aref pos 2))
                                    (int (aref pos 3))))
            (vec4 1.0 1.0 1.0 1.0)
            )))

(defun-g my-cool-func-g ((uv :vec2) &uniform (sampler :sampler-buffer))
  (let ((uv-index (int (mod (* 300 (+ (* 64 (aref uv 0))
                                      (* 57 (aref uv 1))))
                            3648))))
    (texel-fetch sampler uv-index)))

(defstruct-g ssbo-struct
  (data (:vec4 3648) :accessor data))

(defun-g frag-stage ((col :vec3) (my-ivec3 :ivec4) (my-sample :vec4) &uniform (my-ssbo ssbo-struct :ssbo))
  (let* ((col (vec4 (mod (aref col 0) 1.0)
                   (mod (aref col 1) 1.0)
                   (mod (aref col 2) 1.0)
                   1.0)))
    col))

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage :vec3 :ivec4 :vec4))


(defun-g plane-vert-stage ((vert g-pt))
  (values (vec4 (pos vert) 1.0)
          (:smooth (pos vert))
          (:smooth (tex vert))))

(defun-g plane-frag-stage ((pos :vec3) (texture-coordinate :vec2)
                           &uniform
                           (tex-sampler :sampler-buffer)
                           (ssbo ssbo-struct :ssbo))
  (let ((uv-index (int (mod (* 300 (+ (* 64 (aref texture-coordinate 0))
                                      (* 57 (aref texture-coordinate 1))))
                        3648))))
    (texel-fetch tex-sampler uv-index))

  ;; (vec4 (mod (aref pos 0) 1.0)
  ;;       (mod (aref pos 1) 1.0)
  ;;       (mod (aref pos 2) 1.0)
  ;;       1.0)
  )

(defpipeline-g plane-pipeline ()
  (plane-vert-stage g-pt)
  (plane-frag-stage :vec3 :vec2))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defparameter render-context nil)
(defparameter cube-render-func (lambda () (livesupport:continuable (step-rendering-cube))))

(defun start-cube-render-thread ()
  (unless render-context (setf render-context (make-context-shared-with-current-context)))
  (sb-thread:make-thread (lambda ()
                           (with-cepl-context (ctx render-context)
                             (try-free-objects *vert-gpu-array* *vert-gpu-index-array* *vert-array-buffer-stream*)
                             (setf *vert-gpu-index-array* (make-gpu-array (list 0 1 2 0 2 3
                                                                                4 5 6 4 6 7
                                                                                8 10 9 8 11 10
                                                                                12 14 15 12 15 13
                                                                                16 17 18 16 18 19
                                                                                20 21 22 20 23 21)
                                                                          :element-type :uint))
                             (setf *vert-gpu-array* (make-gpu-array
                                                     cube-1
                                                     :element-type :vec3))
                             (setf *vert-array-buffer-stream* (make-buffer-stream *vert-gpu-array* :index-array *vert-gpu-index-array*))
                             (setf *transform-feedback-gpu-array* (make-gpu-array nil :dimensions 24 :element-type :vec4))
                             (setf *transform-feedback-stream* (make-transform-feedback-stream *transform-feedback-gpu-array*))
                             (gl:enable :depth-test)
                             (temp-func-setup-rendering)
                             (loop (funcall cube-render-func))
                             ))
                         :name "cube-rendering-thread"))

(defparameter my-cool-fbo nil)
(defparameter my-cool-fbo-texture nil)
(defparameter my-cool-fbo-texture-sampler nil)
(defparameter fbo-texture-lock (bt:make-lock "fbo-texture-lock"))
(defparameter fbo-tex-mutex (sb-thread:make-mutex))
(defparameter cute-tex nil)
(defparameter cute-sampler nil)

(defun temp-func-setup-rendering ()
  (try-free-objects ;;my-cool-fbo
                    my-cool-fbo-texture my-cool-fbo-texture-sampler
                    cute-tex cute-sampler)

  (setf (cepl:depth-test-function) #'<)
  (setq my-depth-func (cepl:depth-test-function))
  (setf my-cool-fbo (make-fbo 0 :d))
  (setf my-cool-fbo-texture (attachment-tex my-cool-fbo 0))
  (setf my-cool-fbo-texture-sampler (sample my-cool-fbo-texture))
  (setf cute-tex (dirt:load-image-to-texture "3.jpg"))
  (setf cute-sampler (sample cute-tex)))

(defparameter my-cool-image-data-c-array nil)
(defparameter my-cool-buffer-backed-texture nil)
(defparameter my-cool-sampler-buffer nil)

(defun init ()
  (setf-if-nil my-cool-image-data-c-array (dirt:load-image-to-c-array "3.jpg"))
  (setf-if-nil my-cool-buffer-backed-texture (make-texture my-cool-image-data-c-array :buffer-storage t))
  (setf-if-nil my-cool-sampler-buffer (sample my-cool-buffer-backed-texture))

  (try-free-objects plane-vert-array plane-index-array plane-buffer-stream)

  (setf plane-vert-array (make-gpu-array plane-1 :element-type 'g-pt)
        plane-index-array (make-gpu-array plane-indices :element-type :uint))
  (setf plane-buffer-stream (make-buffer-stream plane-vert-array :index-array plane-index-array))
  (ignore-errors
   (setf (resolution (current-viewport))
         (get-cepl-context-surface-resolution)))
  (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                             (y (resolution (current-viewport)))
                                                             0.1
                                                             30f0
                                                             60f0)))


(defparameter rendering-paused? nil)
(defparameter blending-params nil)
(defparameter frame-queued? t)

(defparameter gate t)

(defun step-rendering-cube ()
  (unless rendering-paused?
    ;; (when *vert-array-buffer-stream*
    ;;   (clear)
    ;;   (map-g #'basic-pipeline *vert-array-buffer-stream*
    ;;          :now (now)
    ;;          :proj *projection-matrix*
    ;;          :rot (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now)))
    ;;          :my-ssbo my-cool-ssbo
    ;;          )
    ;;   (swap)
    ;;   (gl:finish)
    ;;   )
    (sleep 0.0001)
    ))

(defun step-rendering ()
  (unless rendering-paused?


    
   (gl:enable :depth-test)
    (when (and plane-buffer-stream
               my-cool-sampler-buffer)
      (clear)
      (map-g #'plane-pipeline plane-buffer-stream
             :tex-sampler my-cool-sampler-buffer)
      (swap))))

(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (livesupport:update-repl-link)
                                 (step-rendering)
                                 (step-host)
                                 (sleep 0.0001)
                                 )))

(defun main ()
  (cepl:repl)
  (init)
  (gl:enable :depth-test)
  (start-cube-render-thread)
  (loop (funcall main-loop-func)))

