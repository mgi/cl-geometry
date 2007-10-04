(in-package :ximage-polygon)

(defclass vertex ()
  ((%x :initarg :x :reader x)
   (%y :initarg :y :reader y)
   (%reflex-p :initform nil :accessor reflex-p)
   (%ear-p :initform nil :accessor ear-p)
   (%on-ear-stack-p :initform nil :accessor on-ear-stack-p)
   (%next :initform nil :accessor next)
   (%prev :initform nil :accessor prev)))

;;; build a doubly-linked circular list of vertices from a list of
;;; points
(defun build-vertices (points)
  (let* ((first-point (car points))
	 (first-vertex (make-instance 'vertex :x (car first-point) :y (cdr first-point)))
	 (last-vertex first-vertex))
    (loop for (x . y) in (cdr points)
	  do (let ((vertex (make-instance 'vertex :x x :y y)))
	       (setf (next last-vertex) vertex
		     (prev vertex) last-vertex
		     last-vertex vertex)))
    (setf (prev first-vertex) last-vertex
	  (next last-vertex) first-vertex)
    first-vertex))
	
;;; return t if there is a turn to the left in point 2 going from
;;; point 1 to point 3
(defun reflex-point-p (x1 y1 x2 y2 x3 y3)
  (plusp (- (* (- y3 y1) (- x2 x1))
	    (* (- x3 x1) (- y2 y1)))))

(defun inside-triangle (x y x1 y1 x2 y2 x3 y3)
  (and (not (reflex-point-p x1 y1 x2 y2 x y))
       (not (reflex-point-p x2 y2 x3 y3 x y))
       (not (reflex-point-p x3 y3 x1 y1 x y))))
	    
(defun reflex-vertex-p (vertex)
  (reflex-point-p (x (prev vertex))
		  (y (prev vertex))
		  (x vertex)
		  (y vertex)
		  (x (next vertex))
		  (y (next vertex))))

;;; use the ear-clipping method.  This method is quadratic, 
;;; but fairly simple to implement.
;;; We keep all the vertices in a doubly-linked circular list
(defun triangulate-simple-polygon-with-no-holes (points fun)
  (declare (optimize (debug 3)))
  (let* ((number-of-points (length points))
	 (ear-stack (make-array number-of-points))
	 (ear-stack-pointer 0)
	 (reflex-points (make-array number-of-points))
	 (number-of-reflex-points 0)
	 (first-vertex (build-vertices points)))
    (loop repeat number-of-points
	  for vertex = first-vertex then (next vertex)
	  do (when (reflex-point-p (x (prev vertex)) (y (prev vertex))
				   (x vertex) (y vertex)
				   (x (next vertex)) (y (next vertex)))
	       (setf (reflex-p vertex) t)
	       (setf (aref reflex-points number-of-reflex-points) vertex)
	       (incf number-of-reflex-points)))
    (flet ((is-ear (vertex)
	     (let ((x1 (x (prev vertex)))
		   (y1 (y (prev vertex)))
		   (x2 (x vertex))
		   (y2 (y vertex))
		   (x3 (x (next vertex)))
		   (y3 (y (next vertex))))
	       (loop for reflex-point from 0
		     do (loop until (or (= reflex-point number-of-reflex-points)
					(and (reflex-p (aref reflex-points reflex-point))
					     (reflex-p (aref reflex-points (1- number-of-reflex-points)))))
			      do (when (and (not (reflex-p (aref reflex-points reflex-point)))
					    (reflex-p (aref reflex-points (1- number-of-reflex-points))))
				   (setf (aref reflex-points reflex-point)
					 (aref reflex-points (1- number-of-reflex-points))))
			      do (decf number-of-reflex-points))
		     until (= reflex-point number-of-reflex-points)
		     do (let* ((v (aref reflex-points reflex-point))
			       (x (x v))
			       (y (y v)))
			  (when (and (not (eq v (prev vertex)))
				     (not (eq v (next vertex)))
				     (inside-triangle x y x1 y1 x2 y2 x3 y3))
			    (return nil)))
		     finally (return t)))))
      (loop repeat number-of-points
	    for vertex = first-vertex then (next vertex)
	    do (when (and (not (reflex-p vertex))
			  (is-ear vertex))
		 (setf (ear-p vertex) t)
		 (setf (on-ear-stack-p vertex) t)
		 (setf (aref ear-stack ear-stack-pointer) vertex)
		 (incf ear-stack-pointer)))
      (finish-output *trace-output*)
      (loop repeat (- number-of-points 3)
	    do (loop until (ear-p (aref ear-stack (1- ear-stack-pointer)))
		     do (decf ear-stack-pointer))
	    do (let ((ear (aref ear-stack (decf ear-stack-pointer))))
		 (setf (on-ear-stack-p ear) nil)
		 (funcall fun 
			  (x (prev ear)) (y (prev ear))
			  (x ear) (y ear)
			  (x (next ear)) (y (next ear)))
		 (setf (next (prev ear)) (next ear)
		       (prev (next ear)) (prev ear))
		 (let ((v (prev ear)))
		   (unless (reflex-vertex-p v)
		     (setf (reflex-p v) nil)
		     (setf (ear-p v) (is-ear v))
		     (when (ear-p v)
		       (setf (ear-p v) t)
		       (unless (on-ear-stack-p v)
			 (setf (aref ear-stack ear-stack-pointer) v)
			 (incf ear-stack-pointer)))))
		 (let ((v (next ear)))
		   (unless (reflex-vertex-p v)
		     (setf (reflex-p v) nil)
		     (setf (ear-p v) (is-ear v))
		     (when (ear-p v)
		       (setf (ear-p v) t)
		       (unless (on-ear-stack-p v)
			 (setf (aref ear-stack ear-stack-pointer) v)
			 (incf ear-stack-pointer))))))))
    ;; report the last triangle
    (let ((ear (aref ear-stack (decf ear-stack-pointer))))
      (funcall fun 
	       (x (prev ear)) (y (prev ear))
	       (x ear) (y ear)
	       (x (next ear)) (y (next ear))))))

(defun triangulate-polygon (points fun)
  (let ((area (loop repeat (length points)
		    for ((x1 . y1) (x2 . y2) . rest) on (append points (list (car points)))
		    sum (- (* x1 y2) (* x2 y1)))))
    (when (plusp area)
      (setf points (reverse points)))
    (triangulate-simple-polygon-with-no-holes points fun)))