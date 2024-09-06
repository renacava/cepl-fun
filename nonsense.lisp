(in-package #:cepl-fun)

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
