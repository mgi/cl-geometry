(defpackage :test-geometry (:use :common-lisp :xlib :2d-geometry :vecto :iterate)
            (:export #:test-triangulate
                     #:test-decompose
                     #:test-bentley-ottmann
                     #:test-decompose-bo
                     #:test-decompose-triangle))

(in-package :test-geometry)

(defparameter *test-polygon*
  (make-polygon-from-coords
   10 10 20 20 20 70 70 70 70 20)
  "Test concave polygon")

(defparameter *test-polygon2*
  (make-polygon-from-coords
   20 20 10 40 20 70 70 70 70 20)
  "Test convex polygon")

(defparameter *test-polygon3*
  (make-polygon-from-coords
   20 20 10 40 70 40 70 70)
  "Test small complex polygon")

(defparameter *test-polygon4*
  (make-polygon-from-coords
   20 20 10 30 70 15 70 40 30 35 50 10 45 35 25 60))

(defparameter *m*
  (make-polygon-from-coords
   0 0 20 0 160 100 300 0
   320 0 320 320 300 320 300 20
   160 120 20 20 20 320 0 320))

(defparameter *poly*
  (make-polygon-from-coords
   0 0 20 20 40 0 60 20 80 0 100 20 120 0 200 200 40 120))

(defun circle (radius xoff yoff)
  (let ((step (/ pi 100)))
    (apply #'make-polygon-from-coords
           (loop for theta from 0 to (- (* 2 pi) step) by step
                 nconc (list (+ (round (* radius (cos theta))) xoff)
                             (+ (round (* radius (sin theta))) yoff))))))

(defparameter *colors*
  (labels ((color-scale (x) (truncate (* x #xffff) 255))
           (parse-line (line start)
             (multiple-value-bind (value pos) (read-from-string line nil 'eol :start start)
                 (cond ((or (eql value 'eol)
                            (symbolp value)) (list #xffff))
                       (t (cons (color-scale value) (parse-line line pos)))))))
    (with-open-file (in #p"/usr/X11R6/share/X11/rgb.txt")
      (do ((line (read-line in nil 'eof) (read-line in nil 'eof))
           res)
          ((eql line 'eof) (nreverse res))
        (push (parse-line line 0) res)))))
  "List of opaque, rgba, xrender compatible colors from rgb.txt")

(defun test-triangulate (polygon w h &optional (x 0) (y 0))
  (let ((point-list (point-list polygon)))
   (with-canvas (:width w :height h)
     (translate x y)
     (set-rgb-fill 0 0 0.8)
     (move-to (x (car point-list))(y (car point-list)))
     (dolist (tk point-list)
       (line-to (x tk)(y tk)))
     (line-to (x (car point-list))(y (car point-list)))
     (fill-path)
     (set-rgb-stroke 0 1.0 0)
     (set-line-width 2)
     (set-line-join :bevel)
     (dolist (tk (triangulate polygon))
       (let ((point-list (point-list tk)))
         (move-to (x (car point-list))(y (car point-list)))
         (dolist (kk (cdr point-list))
           (line-to (x kk)(y kk)))
         (line-to (x (car point-list))(y (car point-list)))
         (stroke)))
     (save-png "test-geometry.png"))))

;; (test-triangulate *test-polygon* 100 100)
;; (test-triangulate *test-polygon2* 100 100)

(defun test-decompose (polygon w h &optional (x 0) (y 0))
  (let ((point-list (point-list polygon)))
   (with-canvas (:width w :height h)
     (translate x y)
     (set-rgba-fill 0 0 0.8 1.0)
     (set-rgba-stroke 0 0.8 0 0.5)
     (move-to (x (car point-list))(y (car point-list)))
     (dolist (tk point-list)
       (line-to (x tk)(y tk)))
     (line-to (x (car point-list))(y (car point-list)))
     (fill-and-stroke)
     (let ((d-polys (decompose-complex-polygon-nondisjoint polygon)))
       (dolist (tk (mapcar #'point-list d-polys))
         (translate 100 0)
         (set-rgba-fill (random 1.0)(random 1.0)(random 1.0) 1.0)
         (move-to (x (car tk))(y (car tk)))
         (dolist (kk (cdr tk))
           (line-to (x kk)(y kk)))
         (line-to (x (car tk))(y (car tk)))
         (fill-path)))
     (save-png "test-geometry.png"))))

;; (test-decompose *test-polygon3* 300 100)
;; (test-decompose *test-polygon4* 400 100)

(defun test-bentley-ottmann (polygon)
  (if (frustrated-polygon-p polygon)
      'frustrated
      (let ((in-points (bentley-ottmann (edge-list polygon)))
            (point-list (point-list polygon)))
        (with-canvas (:width 400 :height 400)
          (scale 4 4)
          (set-rgb-stroke 0 0 1.0)
          (set-line-width 1/5)
          (move-to (x (car point-list))
                   (y (car point-list)))
          (dolist (tk (cdr point-list))
            (line-to (x tk)(y tk)))
          (line-to (x (car point-list))(y (car point-list)))
          (stroke)
          (set-rgba-fill 0 1.0 0 0.4)
          (dolist (tk in-points)
            (set-rgba-stroke (random 1.0) (random 1.0) (random 0.5) 0.5)
            (move-to 0 (y tk))
            (line-to 100 (y tk))
            (move-to (x tk) 0)
            (line-to (x tk) 100)
            (stroke)
            (centered-circle-path (x tk)(y tk) 1)
            (fill-path))
          (save-png "test-geometry.png")))))

;; (test-bentley-ottmann *test-polygon3*)
;; (test-bentley-ottmann *test-polygon4*)

(defun test-decompose-bo (polygon w h &optional (x 0) (y 0))
  (let ((point-list (point-list polygon)))
    (with-canvas (:width w :height h)
      (translate x y)
      (set-rgba-fill 0 0 0.8 1.0)
      (set-rgba-stroke 0 0.8 0 0.5)
      (move-to (x (car point-list))(y (car point-list)))
      (dolist (tk (cdr point-list))
        (line-to (x tk)(y tk)))
      (line-to (x (car point-list))(y (car point-list)))
      (fill-and-stroke)
      (let ((d-polys (decompose-complex-polygon-bentley-ottmann polygon)))
        (dolist (tk (mapcar #'point-list d-polys))
          (translate 100 0)
          (set-rgba-fill (random 1.0)(random 1.0)(random 1.0) 1.0)
          (move-to (x (car tk))(y (car tk)))
          (dolist (kk (cdr tk))
            (line-to (x kk)(y kk)))
          (line-to (x (car tk))(y (car tk)))
          (fill-path)))
      (save-png "test-geometry.png"))))

;;(test-decompose-bo *test-polygon3* 300 100)
;;(test-decompose-bo *test-polygon4* 400 100)

(defun test-decompose-triangle (polygon w h)
  (let ((point-list (point-list polygon)))
    (with-canvas (:width w :height h)
      (translate 10 10)
      (set-rgba-fill 0 0 0.8 1.0)
      (set-rgba-stroke 0 0.8 0 0.5)
      (move-to (x (car point-list))(y (car point-list)))
      (dolist (tk (cdr point-list))
        (line-to (x tk)(y tk)))
      (line-to (x (car point-list))(y (car point-list)))
      (even-odd-fill-and-stroke)
      (translate 0 0)
      (let ((d-polys (mapcar #'point-list
                             (decompose-complex-polygon-triangles polygon :in-test 'geometry:point-in-polygon-crossing-p))))
        (set-rgba-fill 0 1.0 0 0.2)
        (dolist (tk d-polys)
          (move-to (x (car tk))(y (car tk)))
          (dolist (kk tk)
            (line-to (x kk)(y kk)))
          (line-to (x (car tk))(y (car tk)))
          (fill-and-stroke)))
      (save-png "test-geometry.png"))))

(defun brush (window color)
  (let* ((fmt (first (find-matching-picture-formats (window-display window)
                                                    :alpha 8 :red 8 :green 8 :blue 8)))
         (pixmap (create-pixmap :drawable window
                                :width 1
                                :height 1
                                :depth (picture-format-depth fmt)))
         (picture (render-create-picture pixmap :format fmt :repeat :on)))
    (render-fill-rectangle picture :src color 0 0 1 1)
    (free-pixmap pixmap)
    picture))

(defun draw-triangle (op brush drawingarea mask-format triangle)
  "Here triangle is a list of 3 points."
  (let ((a (first triangle))
        (b (second triangle))
        (c (third triangle)))
    (render-triangles drawingarea op brush 0 0 mask-format
                      (list  (x a) (y a)
                             (x b) (y b)
                             (x c) (y c)))))

(defun draw-dot (op brush drawingarea mask-format point)
  (let* ((dx 1)
         (dy 1)
         (x (x point))
         (y (y point))
         (square (list (- y dy) (+ y dy)
                       (- x dx) (- y dy) (- x dx) (+ y dy)
                       (+ x dx) (- y dy) (+ x dx) (+ y dy))))
    (render-trapezoids drawingarea op brush 0 0 mask-format square)))

(defun one-of (lst)
  (nth (random (length lst)) lst))

(defun triangulate-demo (poly)
  (let* ((dpy (open-display ""))
         (screen (first (display-roots dpy)))
         (root (screen-root screen))
         (win (create-window :parent root :x 0 :y 0 :width 320 :height 320
                             :event-mask (make-event-mask :key-press
                                                          :structure-notify)))
         (win-h (drawable-height win))
         (win-w (drawable-width win))
         (win-fmt (find-window-picture-format win))
         (background (create-pixmap :drawable win :width win-w :height win-h
                                    :depth (picture-format-depth win-fmt)))
         (drawingarea (render-create-picture background :format win-fmt
                                                        :poly-edge :smooth
                                                        :poly-mode :imprecise))
         (bg-color (list #xffff #xffff #xffff #xffff))
         (mask-format (first (find-matching-picture-formats dpy :alpha 8
                                                                :red 0
                                                                :green 0
                                                                :blue 0))))
    (setf (window-background win) background)
    (render-fill-rectangle drawingarea :src bg-color 0 0 win-w win-h)
    (map-window win)
    (do () ((event-case (dpy) (:map-notify () t))))
    (unwind-protect (progn
                      (loop for triangle in (mapcar #'point-list
                                                    (decompose-complex-polygon-triangles poly))
                            for brush = (brush win (one-of *colors*))
                            do (draw-triangle :over brush drawingarea mask-format triangle))
                      (do ()
                          ((event-case (dpy :timeout (/ 50 1000))
                             (:key-press () t)))
                        (clear-area win)))
      (render-free-picture drawingarea)
      (free-pixmap background)
      (unmap-window win)
      (close-display dpy))))

#|
(triangulate-demo *test-polygon2*)
(triangulate-demo *test-polygon3*)
(triangulate-demo *poly*)
(triangulate-demo *m*)
(triangulate-demo (circle 150 150 150))
|#
