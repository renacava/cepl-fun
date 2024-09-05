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
                     ;;(sampler :sampler-2d)
                     )
  (let* ((pos (* (rtg-math.matrix4:rotation-from-euler rot) (vec4 vert 1)))
         (col (if (or (isinf (aref pos 0))
                      (isinf (aref pos 1))
                      (isinf (aref pos 2)))
                  (vec3 1.0 0.0 0.0)
                  (vec3 0.0 0.0 1.0)))
         (pos (+ pos (vec4 (* 2 (sin now)) (* 3 (cos now)) -5 0)))
         ;; (my-sample (texture sampler (vec2 (mod (aref pos 0) 1.0)
         ;;                                   (mod (aref pos 1) 1.0))))
         )
    (values (* proj pos)
     
            ;; (vec3 (aref vert 0)
            ;;       (aref vert 1)
            ;;       (aref vert 2))
            (:smooth (vec3 (aref vert 0)
                           (aref vert 1)
                           (aref vert 2)))
            (:flat (ivec4 (int (aref pos 0))
                                    (int (aref pos 1))
                                    (int (aref pos 2))
                                    (int (aref pos 3))))
            ;;my-sample
            (vec4 1.0 1.0 1.0 1.0)
            )))

;; (defun-g 1d-to-2d ((index :int) (cols :int))
;;   (let* ((x (mod index cols))
;;          (y (int (/ index cols))))
;;     (vec2 (float x) (float y))))

(defun-g my-cool-func-g ((uv :vec2) &uniform (sampler :sampler-2d))
  ;;(texture sampler uv)
  (texel-fetch sampler (ivec2 (int (aref uv 0)) (int (aref uv 1))) 0)
  )

(defstruct-g ssbo-struct
  (data (:vec4 3648) :accessor data))

(defun-g frag-stage ((col :vec3) (my-ivec3 :ivec4) (my-sample :vec4) &uniform (my-ssbo ssbo-struct :ssbo))
  (let* (;; (col (vec4 (mod (aref col 0) 1.0)
         ;;           (mod (aref col 1) 1.0)
         ;;           (mod (aref col 2) 1.0)
         ;;           1.0))
         ;; (my-sample (texture sampler (vec2 (mod (aref col 0) 1.0)
         ;;                                   (mod (aref col 1) 1.0))))
         ;; (my-texel (texel-fetch sampler
         ;;                        (ivec2 16
         ;;                               16)
         ;;                        0))
         ;; (modulated-sample (vec4 (mod (aref my-sample 0) 1.0)
         ;;                         (mod (aref my-sample 1) 1.0)
         ;;                         (mod (aref my-sample 2) 1.0)
         ;;                         1.0))
         (uv (vec2 (aref col 0)
                   (aref col 1)))
         (uv-index (int (mod (+ (* 640 (aref uv 0))
                                (* 570 (aref uv 1))
                                -1)
                             3648)))
         (my-ssbo-data (/ (aref (data my-ssbo) uv-index) 255.0)))
    col
    my-ssbo-data;;modulated-sample
    ;;my-texel
    )
  ;;(vec4 1.0 1.0 1.0 1.0)
  ;;(texture sampler (vec2 (mod (aref col 0) 1.0) (mod (aref col 1) 1.0)))
  ;;col
  )

(defpipeline-g basic-pipeline ()
  (vert-stage :vec3)
  (frag-stage :vec3 :ivec4 :vec4))


(defun-g plane-vert-stage ((vert g-pt))
  (values (vec4 (pos vert) 1.0)
          (:smooth (pos vert))
          (:smooth (tex vert))))

(defun-g plane-frag-stage ((pos :vec3) (texture-coordinate :vec2)
                           &uniform
                           (tex-sampler :sampler-2d)
                           (ssbo ssbo-struct :ssbo))
  (let* ((uv-index (int (mod (+ (* 64 (aref pos 0))
                                (* 57 (aref pos 1)))
                             3648)))
         (my-data (/ (aref (data ssbo) uv-index) 255.0)))
    my-data
    ;;(+ my-data (vec4 0.0 0.0 0.0 1.0))
    
    )
  ;; (vec4 (mod (aref pos 0) 1.0)
  ;;       (mod (aref pos 1) 1.0)
  ;;       (mod (aref pos 2) 1.0)
  ;;       1.0)
  
  ;; (vec4 (abs (aref pos 0))
  ;;       (abs (aref pos 1))
  ;;       (abs (aref pos 2)))
  ;;(texture tex-sampler texture-coordinate)
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
  (setf cute-sampler (sample cute-tex))
  
  ;; (unless my-cool-fbo
  ;;   )
  ;; (unless my-cool-fbo-texture
  ;;   )
  ;; (unless my-cool-fbo-texture-sampler
  ;;   )
  ;;(loop (funcall cube-render-func))
  )

(defun init ()
  (unless my-cool-image-data
    (setf my-cool-image-data (list (list (loop for row in (pull-g (dirt:load-image-to-c-array "3.jpg"))
                                               append (mapcar #'v! row))))))
  (unless my-cool-texture-c-array
    (setf my-cool-texture-c-array (make-c-array my-cool-image-data
                                                :dimensions 1
                                                :element-type 'ssbo-struct)))
  (unless my-cool-gpu-array
    (setf my-cool-gpu-array (make-gpu-array my-cool-texture-c-array :element-type 'ssbo-struct)))
  (unless my-cool-ssbo
    (setf my-cool-ssbo (make-ssbo my-cool-gpu-array 'ssbo-struct)))
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

(defparameter my-second-buffer nil)
(defparameter my-second-array nil)


(defparameter rendering-paused? nil)
(defparameter blending-params nil)
(defparameter frame-queued? t)

(defparameter gate t)


(defun step-rendering-cube ()
  (unless rendering-paused?
    
    ;; (when gate
    ;;   (setf gate nil)
    ;;   (setq my-data
    ;;         (funcall-g 'my-cool-func-g (vec2 (* 0.5) (* 0.5)) :sampler cute-sampler)))
    
    
    
    ;; (when *vert-array-buffer-stream*
      ;; (bt:with-lock-held (fbo-texture-lock)
      ;;   (when (and ;;frame-queued?
      ;;              *vert-array-buffer-stream*)
      ;;     ;; (with-fbo-bound (my-cool-fbo)
      ;;     ;;   ;;(gl:clear-depth 0)
      ;;     ;;   )
      ;;     (clear)
      ;;     (map-g #'basic-pipeline *vert-array-buffer-stream*
      ;;            :now (now)
      ;;            :proj *projection-matrix*
      ;;            :rot (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now)))
      ;;            :sampler cute-sampler)
      ;;     (swap)
      ;;     (gl:flush)
    ;;     (setf frame-queued? nil)))

    ;; (when (and ;;frame-queued?
    ;;        ;;gate
    ;;        *vert-array-buffer-stream*)

    ;;   (clear)
    ;;   (map-g #'basic-pipeline *vert-array-buffer-stream*
    ;;          :now (now)
    ;;          :proj *projection-matrix*
    ;;          :rot (v! (* 90 0.03 (now)) (* 90 0.02 (now)) (* 90 0.01 (now)))
    ;;          :my-ssbo my-cool-ssbo
    ;;          )
    ;;   (swap)
    ;;   (gl:finish)

    ;;   (setf gate nil)
    ;;   )
    (sleep 0.0001)
      ;;(gl:enable :depth-test)
      
      ;; (with-fbo-bound (my-cool-fbo)
;;         (with-temp-sampler (temp-sampler (dirt:load-image-to-texture "3.jpg"))
;; )
;;         ;;(swap)
;;         ;;(gl:finish)
;;         )
      

      
    ;;(sleep 0.001)
      )
    ;;(gl:disable :depth-test)
    ;;(sleep 0.0001)
    )

(defun step-rendering ()
  (unless rendering-paused?


    
   (gl:enable :depth-test)
    (when (and ;;(not frame-queued?)
           plane-buffer-stream
           ;;my-cool-fbo-texture-sampler
           my-cool-ssbo)
        (bt:with-lock-held (fbo-texture-lock)
          (progn
            (clear)
            (map-g #'plane-pipeline plane-buffer-stream
                   ;;:tex-sampler my-cool-fbo-texture-sampler
                   :ssbo my-cool-ssbo)
            (swap)
            ;; (gl:flush)
            ;;(gl:finish)
            ;;(step-host)
            ;;(setf frame-queued? t)
            ))
        ;;(step-host)
      
      )
    
    ;;(sleep 0.0001)
    ))

(defparameter my-cool-gpu-array nil)
(defparameter my-cool-image-data nil)
(defparameter my-cool-texture-c-array nil)
(defparameter my-cool-ssbo nil)

(defparameter main-loop-func (lambda ()
                               (livesupport:continuable
                                 
                                 (livesupport:update-repl-link)
                                 
                                 (step-rendering)
                                 (step-host)
                                 (sleep 0.0001)
                                 ;; (when (boundp 'my-data)
                                 ;;   (print my-data))
                                 ;; (unless render-context
                                 ;;   )
                                 
                                 
                                 )))

(defparameter dirty? nil)
(defparameter last-time-run (now))
(defparameter second-context nil)

(defparameter last-run (now))
(defparameter ctx nil)
(defparameter dirty? nil)
(defparameter flipflop nil)
;; (defparameter inner-loader-thread-func (lambda ()
;;                                          (if dirty?
;;                                              (progn
;;                                                (setf flipflop (not flipflop))
;;                                                (setf dirty? nil)

;;                                                (if my-second-array
;;                                                    (progn
;;                                                      (try-free my-second-array)
;;                                                      (setf my-second-array nil))
                                                   
;;                                                    (progn
;;                                                      (setf my-second-array (make-gpu-array cube-2))
;;                                                      (gl:finish))))
                                             
;;                                              (sleep 0.1))))

;; (defun init-ctx ()
;;   (setf ctx (or ctx (cepl.context:make-context-shared-with-current-context))))

;; (defun make-loader-thread ()
;;   (init-ctx)
  
;;   (bt:make-thread (lambda ()
;;                     (loop (livesupport:continuable (funcall inner-loader-thread-func))))))

;; we could have an issue wherein the context is defined outside of the lambda... like a binding issue, we're not doing dynamic binding but rather binding on definition. sigh.


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

;;(defparameter separate-rendering-thread? t)

(defun main ()
  (cepl:repl)
  (init)
  ;;(make-loader-thread)
  ;;(start-cube-render-thread)
  ;;(temp-func-setup-rendering)
  ;;(setf (cepl.sdl2::vsync) nil)
  (gl:enable :depth-test)
  (start-cube-render-thread)
  (loop (funcall main-loop-func)))

