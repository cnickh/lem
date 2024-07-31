(defpackage :lem-clim/frame
  (:use :clim :clim-lisp :lem-clim/utils :lem-clim/resource)
  (:local-nicknames
   (:view :lem-clim/view)
   (:input :lem-clim/input))
  (:export :init-display
           :enable-resize
           :disable-resize
           :display
           :views
           :foreground
	   :background
           :display-char-width
	   :display-char-height
	   :display-width
	   :display-height
           :lem))

(in-package :lem-clim/frame)

(defvar +display-height+ 700)
(defvar +display-width+ 800)

(defvar +resize+ nil)

(defclass lem-stream-pane (clim-stream-pane)
  ())

(define-application-frame lem ()
  ;;State needs to include display (global parameters for displaying)
  ;; a set of views (contain the a set of charaters to be drawn)
  ;; TODO enable incremental redisplay
  ((%views
    :initform '()
    :accessor views)
   (%display-height
    :initform +display-height+
    :accessor display-height)
   (%display-width
    :initform +display-width+
    :accessor display-width)
   (%display-char-height
    :initform nil
    :accessor display-char-height)
   (%display-char-width
    :initform nil
    :accessor display-char-width)
   (%background
    :initform (lem:make-color 0 0 0)
    :accessor background)
   (%foreground
    :initform (lem:make-color 0 0 0)
    :accessor foreground))

  (:menu-bar nil)

  (:icon 
   (get-resource-pathname "resources/icon.png"))
  
  (:pane
   (restraining (:min-height +display-height+ 
               :min-width +display-width+
               :scroll-bars nil)
       (make-clim-stream-pane
        :name "lem-stream"
        :type 'lem-stream-pane
        :scroll-bar nil
        :display-function 'compose-display
        :incremental-redisplay t))))

(defun enable-resize ()
  (setq +resize+ T))

(defun disable-resize ()
  (setq +resize+ nil))

(defmethod resize-app-frame ((pane pane) event)
  (let* ((frame (pane-frame pane))
         (rect (window-event-native-region event))
         (height (bounding-rectangle-height rect))
         (width (bounding-rectangle-width rect)))
    ;;(log:info "resize called ~a h:~a w:~a" rect height width)

    (when (<= +display-height+ height) 
      (setf (display-height frame) height)
      (setf (display-char-height frame) (floor height (text-height pane))))

    (when (<= +display-width+ width) 
      (setf (display-width frame) width)
      (setf (display-char-width frame) (floor width (text-width pane))))
    
    ;;(sleep 1)
    (lem:update-on-display-resized)))

(defmethod handle-event :around ((stream pane) event)
  ;;(log:info "event ~a ~%" event)
  (unless (typep event 'character)
    (case (event-type event)
     (:window-configuration (when +resize+ (resize-app-frame stream event)))
     (:pointer-scroll (input:pointer-scroll event stream))
     (:pointer-exit (input:pointer-exit event stream))
     (:pointer-motion (input:pointer-motion event stream))
     (:pointer-button-press (input:pointer-press event stream))
     (:pointer-button-release (input:pointer-release event stream))
     (:key-press (input:key-press event))))
  (call-next-method))

(defun compose-display (frame pane)
;;loop through views detect change & update
  (log:info "have views ~a " (views frame))
  (handler-case
      (progn
        (draw-rectangle pane 
                        (make-point 0 0) 
                        (make-point (display-width frame) (display-height frame)) 
                        :ink (parse-raw-color (background frame)))
        
        (setf (medium-background pane) (parse-raw-color (background frame)))
        (setf (medium-foreground pane) (parse-raw-color (foreground frame)))
        
        (loop for view 
              in (views frame) 
              do (updating-output (pane :cache-value view :cache-test 'view:view-cmp)
                   (view:draw-view view pane (display-width frame) (display-height frame)))))
  (error (e)
         (log:info "ERR got ~a" e))))

