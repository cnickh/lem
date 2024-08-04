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
           :editor-thread
           :com-redisplay
           :lem
           :frame-lock))

(in-package :lem-clim/frame)

(defvar +display-height+ 400)
(defvar +display-width+ 600)

(defvar frame-lock (bt:make-recursive-lock))

(defclass lem-stream-pane (application-pane)
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
    :initform 100
    :accessor display-char-height)
   (%display-char-width
    :initform 100
    :accessor display-char-width)
   (%background
    :initform (lem:make-color 0 0 0)
    :accessor background)
   (%foreground
    :initform (lem:make-color 0 0 0)
    :accessor foreground)
   (%editor-thread
    :initform nil
    :accessor editor-thread))

  (:menu-bar nil)

  (:top-level (editor-loop-launcher))

  (:icon 
   (get-resource-pathname "resources/icon.png"))
  
  (:pane
   (restraining
       (:min-height +display-height+
        :min-width +display-width+
        :scroll-bars nil)
     (make-clim-stream-pane
      :name "lem-stream"
      :type 'lem-stream-pane
      :scroll-bar nil
      :display-function 'compose-display
      ;;:incremental-redisplay t
      ))))

(define-lem-command (com-redisplay :name t) () 
  ;;(log:info "com-redisplay ~a" (bt:current-thread))
  )

(defun join-editor-thread()
  (bt:join-thread
   (find-if
    (lambda (th)
      (search "editor" (bt:thread-name th)))
    (bt:all-threads))))

(defun editor-loop-launcher (frame)
  (funcall (editor-thread frame))
  (default-frame-top-level frame))

(defmethod resize-app-frame ((pane pane) event)
    (let* ((frame (pane-frame pane))
           (rect (window-event-native-region event))
           (height (bounding-rectangle-height rect))
           (width (bounding-rectangle-width rect)))
      (log:info "resize called ~a h:~a w:~a" rect height width)
      (lem:send-event
       (lambda ()
         ;;(bt:with-recursive-lock-held (frame-lock)
           (when (<= +display-height+ height)
             (setf (display-height frame) height)
             (setf (display-char-height frame) (floor height (text-height pane))))
           
           (when (<= +display-width+ width) 
             (setf (display-width frame) width)
             (setf (display-char-width frame) (floor width (text-width pane))))
           
           ;;(log:info "passing control to lem")
           (lem:update-on-display-resized)))));;)

(defmethod handle-event :around ((stream pane) event)
  ;;(log:info "event ~a ~%" (event-type event))
  (unless (typep event 'character)
    (case (event-type event)
     (:window-configuration (resize-app-frame stream event))
     (:pointer-scroll (input:pointer-scroll event stream))
     (:pointer-exit (input:pointer-exit event stream))
     (:pointer-motion (input:pointer-motion event stream))
     (:pointer-button-press (input:pointer-press event stream))
     (:pointer-button-release (input:pointer-release event stream))
     (:key-press (input:key-press event))))

  ;;(unless (eq :execute-command (event-type event))
  ;;  (execute-frame-command *application-frame* '(com-redisplay)))

  (call-next-method))

(defun compose-display (frame pane)
  ;;loop through views detect change & update
  ;;(log:info "have views ~a " (views frame))
  (handler-case
     ;;(bt:with-recursive-lock-held (frame-lock)
        (progn
          ;;(log:info "Running on ~a" (bt:current-thread))
          (draw-rectangle pane
                          (make-point 0 0) 
                          (make-point (display-width frame) (display-height frame)) 
                          :ink (parse-raw-color (background frame)))
        
          (setf (medium-background pane) (parse-raw-color (background frame)))
          (setf (medium-foreground pane) (parse-raw-color (foreground frame)))
        
          (loop for view 
                in (views frame) 
                do (view:draw-view view pane (display-width frame) (display-height frame))));;)
    (error (e)
      (log:info "ERR got ~a" e))))

