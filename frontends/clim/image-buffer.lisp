(defpackage :lem-clim/image-buffer
  (:use :cl :lem)
  (:import-from :clim
                :compose-transformations
                :make-pattern-from-bitmap-file
                :pattern-height
                :pattern-width
                          :draw-design)
  (:shadowing-import-from :clim
                          :make-translation-transformation
                          :make-scaling-transformation)
  (:export :render
           :image-buffer
           :image-fit-to-height
           :image-fit-to-screen
           :image-fit-to-width
           :image-zoom-help
           :image-zoom-in
           :image-zoom-out
           :image-zoom-reset))

(in-package :lem-clim/image-buffer)

(defclass image-buffer (text-buffer) 
  ((%fit
    :initarg :none
    :accessor image-fit)))

(defun buffer-image (buffer)
  (buffer-value buffer 'image))

(defun (setf buffer-image) (image buffer)
  (setf (buffer-value buffer 'image) image))

(defun buffer-scaling (buffer)
  (buffer-value buffer 'scaling))

(defun (setf buffer-scaling) (scaling buffer)
  (setf (buffer-value buffer 'scaling) scaling))

(define-major-mode image-viewer-mode ()
    (:name "Image Viewer"
     :keymap *image-viewer-keymap*)
  (modeline-add-status-list 'image-information (current-buffer))
  (setf (lem:buffer-read-only-p (current-buffer)) t))

(defun image-information (window)
  (let ((image (buffer-image (window-buffer window))))
    (format nil "  ~Dx~D (x~,2F)"
            (pattern-width image)
            (pattern-height image)
            (buffer-scaling (window-buffer window)))))

;; Zoom.
(define-key *image-viewer-keymap* "C-+" 'image-zoom-in)
(define-key *image-viewer-keymap* "+" 'image-zoom-in)
(define-key *image-viewer-keymap* "C--" 'image-zoom-out)
(define-key *image-viewer-keymap* "-" 'image-zoom-out)
(define-key *image-viewer-keymap* "C-0" 'image-zoom-reset)
(define-key *image-viewer-keymap* "0" 'image-zoom-reset)
(define-key *image-viewer-keymap* "?" 'image-zoom-help)
(define-key *image-viewer-keymap* "C-h" 'image-zoom-help)
;; Fit to width, height, screen.
(define-key *image-viewer-keymap* "f" 'image-fit-to-screen)
(define-key *image-viewer-keymap* "w" 'image-fit-to-width)
(define-key *image-viewer-keymap* "h" 'image-fit-to-height)

(defun clim-scale (scale)
  (clim:make-scaling-transformation scale scale))

(defun compute-scale (image-len display-len)
  (let* ((ratio (/ image-len display-len))
          (percent (- 100 (/ 100.0 ratio))))
     (* -1 (/ percent 100))))

(defmethod render ((buffer image-buffer) medium x y display-width display-height)
  ;;(log:info "image-buffer render called ")
  ;;(let* ((image (buffer-image buffer))
  ;;       (width (pattern-width image))
  ;;       (height (pattern-height image))
  ;;       (scale
  ;;         (case (image-fit buffer)
  ;;           (:none  (buffer-scaling buffer))
  ;;           (:width (compute-scale width display-width))
  ;;           (:height (compute-scale height display-height))
  ;;           (:screen
  ;;            (if (>= (/ height width) (/ display-height display-width))
  ;;                (compute-scale height display-height)
  ;;                (compute-scale width display-width))))))
  ;;  
  ;;  (setf (buffer-scaling buffer) scale)
    (draw-design medium 
                 (buffer-image buffer)
                 :transformation (compose-transformations
                                  (clim:make-translation-transformation x y)
                                  (clim-scale (buffer-scaling buffer)))))
  ;;)

(defun scale-buffer-image (buffer scale-offset)
  (setf (image-fit buffer) :none)
  (incf (buffer-scaling buffer) scale-offset))
  
(defun reset-buffer-scale (buffer)
  (setf (image-fit buffer) :none)
  (setf (buffer-scaling buffer) 1))

(defun fit-to-width (buffer)
  (setf (image-fit buffer) :width))

(defun fit-to-height (buffer)
  (setf (image-fit buffer) :height))

(defun fit-to-screen (buffer)
  (setf (image-fit buffer) :screen))

(define-command image-zoom-in () ()
  (scale-buffer-image (current-buffer) 0.1))

(define-command image-zoom-out () ()
  (scale-buffer-image (current-buffer) -0.1))

(define-command image-zoom-reset () ()
  "Set the image to its original size."
  (reset-buffer-scale (current-buffer)))

(define-command image-fit-to-width () ()
  "Make the image as large as the display width."
  (fit-to-width (current-buffer)))

(define-command image-fit-to-height () ()
  "Make the image as big as the display height."
  (fit-to-height (current-buffer)))

(define-command image-fit-to-screen () ()
  "Enlarge or shrink the image to fit the display."
  (fit-to-screen (current-buffer)))

(defclass clim-find-file-executor (lem:find-file-executor) ())

(defmethod lem:execute-find-file ((executor clim-find-file-executor) mode pathname)
  (cond ((member (pathname-type pathname)
                 '("png" "jpg" "jpeg" "bmp" "gif")
                 :test #'equal)
         (open-image-buffer pathname))
        (t
         (call-next-method))))

(defun open-image-buffer (pathname)
  (let ((image (make-pattern-from-bitmap-file pathname))
        (buffer (lem:make-buffer (file-namestring pathname)
                                 :directory (expand-file-name
                                             (namestring (uiop:pathname-directory-pathname pathname))))))
    (change-class buffer 'image-buffer)
    (setf (buffer-image buffer) image)
    (setf (buffer-scaling buffer) 1)
    (fit-to-screen buffer)
    (change-buffer-mode buffer 'image-viewer-mode)
    buffer))

(setf lem:*find-file-executor* (make-instance 'clim-find-file-executor))

(define-command image-zoom-help () ()
  (with-pop-up-typeout-window (s (make-buffer "*image-zoom-help*") :erase t)
    (format s "Open an image file in Lem and use these keys to zoom in and out:~&")
    (format s "Zoom in: + or C - + (M-x image-zoom-in)~&")
    (format s "Zoom out: - or C - - (M-x image-zoom-out)~&")
    (format s "Reset: 0 or C - 0 (M-x image-zoom-reset)~&")
    (format s "~%")
    (format s "Fit the image to the screen:~&")
    (format s "Fit to screen: f (M-x image-fit-to-screen)~&")
    (format s "Fit to width: w (M-x image-fit-to-width)~&")
    (format s "Fit to height: h (M-x image-fit-to-height)~&")))