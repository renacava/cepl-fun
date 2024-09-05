(in-package #:cepl-fun)

(defmacro without-depth (&body body)
  `(progn
     (gl:disable :depth-test)
     ,@body
     (gl:enable :depth-test)))

(defparameter *vert-gpu-array* nil)
(defparameter *vert-gpu-index-array* nil)
(defparameter *vert-array-buffer-stream* nil)
(defparameter *projection-matrix* nil)
;;(defparameter *transform-feedback-gpu-array* nil)
;;(defparameter *transform-feedback-stream* nil)
 
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

(defun-g int-to-cube-float ((my-int :uint))
  (float my-int)

  (if (= my-int 0)
      -0.5f0
      (if (= my-int 1)
          0.5f0
          123f0
          ;;(float my-int)
          ;; (if (= my-int 0)
          ;;     0.0
          ;;     1.0
          ;;     ;;(float my-int)
          ;;     )
          ))
  ;; (let ((my-float (float my-int)))
  ;;   (* my-float 0.5))
  )

(defun-g vert-stage ((vert :vec3)
                     &uniform
                     (now :float)
                     (proj :mat4)
                     (rot :vec3)
                     ;;(rot-mat :mat4)
                     )
  (let* ((pos (* (rtg-math.matrix4:rotation-from-euler rot) (vec4 vert 1)))
         ;; (pos (ivec4 (int (aref vert 0))
         ;;             (int (aref vert 1))
         ;;             (int (aref vert 2))
         ;;             1))
         
         (col (if (or (isinf (aref pos 0))
                      (isinf (aref pos 1))
                      (isinf (aref pos 2)))
                  (vec3 1.0 0.0 0.0)
                  (vec3 0.0 0.0 1.0)))
         ;;(pos (+ pos (vec4 -200 2 -300.5 0)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 0)))
         ;;(pos (+ pos (vec4 0 0 -5 0)))
         )
    (values (* proj pos)
     
            (vec3 (aref pos 0)
                 (aref pos 1)
                 (aref pos 2))
            (:feedback :flat (ivec4 (int (aref pos 0))
                                    (int (aref pos 1))
                                    (int (aref pos 2))
                                    (int (aref pos 3))))
            ;;(vec3 (aref vert 0) (aref vert 1) (aref vert 2))
            )))

(defun-g frag-stage ((col :vec3) (my-ivec3 :ivec4))
  
  ;; (v4:+s (vec4 col 1) 0.5)
  (let ((col (vec4 (mod (aref col 0) 1.0)
                   (mod (aref col 1) 1.0)
                   (mod (aref col 2) 1.0)
                   1.0)))
    col)
  
  ;; (let ((col (+ col (vec3 1.0 1.0 1.0))))
  ;;   (vec4 col 1))
  )

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage :vec3 :ivec4))


(defun-g plane-vert-stage ((vert g-pt))
  (values (vec4 (pos vert) 1.0)
          (pos vert)
          (tex vert)))

(defun-g plane-frag-stage ((pos :vec3) (texture-coordinate :vec2) &uniform (tex-sampler :sampler-2d))
  (vec4 (mod (aref pos 0) 1.0)
        (mod (aref pos 1) 1.0)
        (mod (aref pos 2) 1.0)
        1.0)
  (texture tex-sampler texture-coordinate)
  )

(defpipeline-g plane-pipeline ()
  (plane-vert-stage g-pt)
  (plane-frag-stage :vec3 :vec2))

(defun now ()
  (float (/ (get-internal-real-time) 1000)))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defparameter render-context nil)
(defparameter cube-render-func (lambda () (step-rendering-cube)))

(defun start-cube-render-thread ()
  (unless render-context (setf render-context (make-context-shared-with-current-context)))
  (sb-thread:make-thread (lambda ()
                           (with-cepl-context (ctx render-context)
                             (try-free-objects *vert-gpu-array* *vert-gpu-index-array* *vert-array-buffer-stream*)
                             (setf *vert-gpu-index-array* (make-gpu-array (list 2 1 0 3 2 0
                                                                                6 5 4 7 6 4
                                                                                9 10 8 10 11 8
                                                                                15 14 12 13 15 12
                                                                                18 17 16 19 18 16
                                                                                22 21 20 21 23 20)
                                                                          :element-type :uint))
                             (setf *vert-gpu-array* (make-gpu-array
                                                     cube-1
                                                     :element-type :vec3))
                             (setf *vert-array-buffer-stream* (make-buffer-stream *vert-gpu-array* :index-array *vert-gpu-index-array*))

                             (loop (funcall cube-render-func))
                             ))
                         :name "cube-rendering-thread"))

(defun init ()
  (try-free-objects plane-vert-array plane-index-array plane-buffer-stream)

  (setf plane-vert-array (make-gpu-array plane-1 :element-type 'g-pt)
        plane-index-array (make-gpu-array plane-indices :element-type :uint))
  (setf plane-buffer-stream (make-buffer-stream plane-vert-array :index-array plane-index-array)))

(defparameter my-second-buffer nil)
(defparameter my-second-array nil)
(defparameter my-cool-fbo nil)
(defparameter my-cool-fbo-texture nil)
(defparameter my-cool-fbo-texture-sampler nil)
(defparameter fbo-texture-lock (bt:make-lock "fbo-texture-lock"))
(defparameter fbo-tex-mutex (sb-thread:make-mutex))

(defparameter rendering-paused? nil)
(defparameter blending-params nil)
(defparameter frame-queued? t)

(defun step-rendering-cube ()
  (unless rendering-paused?
    (unless my-cool-fbo
      (setf my-cool-fbo (make-fbo 0)))
    (unless my-cool-fbo-texture
      (setf my-cool-fbo-texture (attachment-tex my-cool-fbo 0)))
    (unless my-cool-fbo-texture-sampler
      (setf my-cool-fbo-texture-sampler (sample my-cool-fbo-texture)))
    
    (when *vert-array-buffer-stream*
      (bt:with-lock-held (fbo-texture-lock)
        (with-fbo-bound (my-cool-fbo)
          (clear)
          (map-g #'basic-pipeline *vert-array-buffer-stream*
                 :now (now)
                 :proj *projection-matrix*
                 :rot (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now))))
          (gl:finish))))
    (sleep 0.0001)))

(defun step-rendering ()
  (unless rendering-paused?
    (ignore-errors
     (setf (resolution (current-viewport))
           (get-cepl-context-surface-resolution)))
    (setf *projection-matrix* (rtg-math.projection:perspective (x (resolution (current-viewport)))
                                                               (y (resolution (current-viewport)))
                                                               0.1
                                                               30f0
                                                               60f0))

    (when (and plane-buffer-stream
               my-cool-fbo-texture-sampler)
      (bt:with-lock-held (fbo-texture-lock)
        (clear)
        (map-g #'plane-pipeline plane-buffer-stream
               :tex-sampler my-cool-fbo-texture-sampler)
        (swap)))
    (step-host)
    (sleep 0.0001)))


(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 (livesupport:update-repl-link)
                                 (step-rendering)
                                 (step-host))))

(defparameter dirty? nil)
(defparameter last-time-run (now))
(defparameter second-context nil)
;; (defparameter second-thread-func
;;   (lambda ()
;;     (livesupport:continuable
;;       (if dirty?
;;           (with-cepl-context (woop my-shared-context)
;;             (setf *vert-array-buffer-stream* (make-buffer-stream (make-gpu-array cube-2) :index-array *vert-gpu-index-array*))
;;             (setf dirty? nil)
;;             (gl:finish))
;;           (sleep 0.1))))

;;   ;; (lambda ()
                                   
;;                                  ;;   ;;(sleep 0.1)
;;                                  ;;   ;; (with-cepl-context (my-cool-context my-shared-context)
                                     
;;                                  ;;   ;;   (sleep 0.1)
;;                                  ;;   (livesupport:continuable
;;                                  ;;     (if dirty?
;;                                  ;;         (let* ()
;;                                  ;;           (setf last-time-run (now))
;;                                  ;;           ;;(setf *vert-array-buffer-stream* (make-buffer-stream (make-gpu-array cube-2) :index-array *vert-gpu-index-array*))
;;                                  ;;           (setf *vert-gpu-array* (make-gpu-array cube-2))
;;                                  ;;           (setf dirty? nil))
;;                                  ;;         (sleep 0.1)))
;;                                  ;;   ;;   )
;;                                  ;;   )
;;   )

(defparameter last-run (now))
(defparameter ctx nil)
(defparameter dirty? nil)
(defparameter flipflop nil)
(defparameter inner-loader-thread-func (lambda ()
                                         (if dirty?
                                             (progn
                                               (setf flipflop (not flipflop))
                                               (setf dirty? nil)

                                               (if my-second-array
                                                   (progn
                                                     (try-free my-second-array)
                                                     (setf my-second-array nil))
                                                   
                                                   (progn
                                                     (setf my-second-array (make-gpu-array cube-2))
                                                     (gl:finish))))
                                             
                                             (sleep 0.1))))

(defun init-ctx ()
  (setf ctx (or ctx (cepl.context:make-context-shared-with-current-context))))

(defun make-loader-thread ()
  (init-ctx)
  
  (bt:make-thread (lambda ()
                    (loop (livesupport:continuable (funcall inner-loader-thread-func))))))

;; we could have an issue wherein the context is defined outside of the lambda... like a binding issue, we're not doing dynamic binding but rather binding on definition. sigh.

(defun main ()
  (cepl:repl)
  (init)
  (make-loader-thread)
  (loop (funcall main-loop-func)))




;;=======================
(defparameter *chunk-width* 16)
(defparameter *chunk-height* 128)
(defparameter *texture-atlas-size* 256)

(defparameter my-cube (list
                       (list (vec3 0.0 0.0 0.0)
                             (vec3 1.0 0.0 0.0)
                             (vec3 1.0 1.0 0.0)
                             (vec3 0.0 1.0 0.0)
                             (vec3 0.0 0.0 1.0)
                             (vec3 1.0 0.0 1.0)
                             (vec3 1.0 1.0 1.0)
                             (vec3 0.0 1.0 1.0))
                       (list 0 1 2 0 2 3
                             3 2 6 3 6 7
                             1 5 6 1 6 2
                             4 0 3 4 3 7
                             5 4 7 5 7 6
                             5 1 0 5 0 4)
                       ))

(defun 3d-to-1d (x y z &optional (cols *chunk-width*) (depth *chunk-height*))
  (+ x (* y cols) (* z cols depth)))

(defun 2d-to-1d (x y &optional (cols *texture-atlas-size*))
  (+ x (* y cols)))


(defun tack (place obj)
  "Tacks place at the end of obj, returns a proper list"
  (if (and
       (not (listp place))
       (not (listp obj)))
      (list place obj)
      (if (not (listp place))
	  (append (list place) obj)
	  (append place (cons obj nil)))))

(defun elt-insert (sequence index value)
  "Non-destructively inserts an element in a sequence"
  (let ((temp (copy-tree sequence)) (true-index index))
    (when (< true-index 0)
      (setf true-index 0))
    (when (> true-index (length sequence))
      (setf true-index (length sequence)))	     
    (append (tack (subseq temp 0 true-index) value) (subseq temp true-index))))


(defun left-pad-t (sequence times &optional (padding 0))
  "Pads the sequence on the left times number of times"
  (let ((result (copy-seq sequence)))
    (dotimes (i times)
      (setf result (elt-insert result 0 padding)))
    result))

(defun left-pad-l (sequence desired-length &optional (padding 0))
  "Pads the sequence on the left until its length is equal to desired-length"
  (left-pad-t sequence (max 0 (- desired-length (length sequence))) padding))

(defun left-pad (sequence &key desired-length times (padding 0))
  "Pads the sequence on the left, either with the desired length, or the number of times to pad, not both"
  (if (or (and desired-length times) (and (not desired-length) (not times)))
      (error "Can't set both a :desired-length and :times in left pad")
      (if times
	  (left-pad-t sequence times padding)
	  (left-pad-l sequence desired-length padding))))

(defun pad-int-left-to-str (int-to-pad desired-length)
  (let* ((int-str (format nil "~a" int-to-pad))
         (int-lst (coerce int-str 'list)))
    (coerce
     (left-pad int-lst :desired-length desired-length :padding #\0)
     'string)))

(defun encode-vert-data (pos-index uv-index face-light-float texture-atlas-index)
  (let ((face-light-float (pad-int-left-to-str (truncate face-light-float) 2))
        (texture-atlas-index (pad-int-left-to-str (truncate texture-atlas-index) 5)))
    (parse-integer
     (format nil "1~a~a~a~a"
             (truncate pos-index)
             (truncate uv-index)
             face-light-float
             texture-atlas-index))))

(defun 1d-to-3dc (index cols depth)
  (let* ((z (truncate (/ index (* cols depth))))
         (index (- index (* z cols depth)))
         (x (mod index cols))
         (y (truncate (/ index cols))))
    (vec3 (float x) (float y) (float z))))

(defun 1d-to-2dc (index cols)
  (let* ((x (mod index cols))
         (y (truncate (/ index cols))))
    (vec2 (float x) (float y))))

(defun decode-vert-data (vert-int)
  (let* ((vert (format nil "~a" vert-int))
         (pos (parse-integer (format nil "~a" (char vert 1))))
         (uv (parse-integer (format nil "~a" (char vert 2))))
         (face-float (parse-integer (subseq vert 3 5)))
         (texture-atlas-index (parse-integer (subseq vert 5 10))))
    (list :pos (1d-to-3dc pos 2 2)
          :uv (1d-to-2dc uv 2)
          :face-float face-float
          :texture-atlas-index (1d-to-2dc texture-atlas-index 128))))

(defun decode-vert-data-nostrings (vert-int)
  (let* ((vert (- vert-int 1000000000))
         (pos (truncate (/ vert 100000000)))
         (vert (- vert (* pos 100000000)))

         (uv (truncate (/ vert 10000000)))
         (vert (- vert (* uv 10000000)))
         (uv (1d-to-2dc uv 2))
         (face-float (float (truncate (/ vert 100000))))
         (vert (- vert (* face-float 100000)))

         (texture-atlas-index (1d-to-2dc vert 128)))
    (list :pos (1d-to-3dc pos 2 2)
          :uv uv
          :face-float face-float
          :texture-atlas-index texture-atlas-index)))

(defun decode-test(vert-int)
  (let* ((vert (- vert-int 1000000000))
         (pos (truncate (/ vert 100000000)))
         (vert (- vert (* pos 100000000)))

         (uv (truncate (/ vert 10000000)))
         (vert (- vert (* uv 10000000)))
         ;;(uv (1d-to-2dc uv 2))
         (face-float (float (truncate (/ vert 100000))))
         (vert (- vert (* face-float 100000)))

         (texture-atlas-index vert))
    (list :pos pos
          :uv uv
          :face-float face-float
          :texture-atlas-index texture-atlas-index)))
;;==========================
