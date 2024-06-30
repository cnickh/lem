(defpackage :lem-clim/frame
  (:use :clim :clim-lisp :lem-clim/utils)
  (:local-nicknames
   (:view :lem-clim/view)
   (:input :lem-clim/input))
  (:export :init-display
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

(defvar +display-height+ 400)
(defvar +display-width+ 500)

(defclass lem-stream-pane (application-pane)
  ())

(define-application-frame lem ()

  ;;State needs to include display (global parameters for displaying)
  ;; a set of views (contain the a set of charaters to be drawn)

  
  
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
  (:pane 
   (make-clim-stream-pane 
    :type 'lem-stream-pane
    :height +display-height+ 
    :width +display-width+ 
    :scroll-bar nil
    ;;:icon (&key name pixmap clippingmaks) 
    :display-function 'compose-display)))

;;(defmethod stream-read-gesture :around ((stream lem-stream-pane)
;;                                        &key &allow-other-keys)
;;  (let* ((results (multiple-value-list (call-next-method)))
;;         (gesture (car results)))
;;    (when gesture
;;      (input:send-lem gesture stream))
;;    (values-list results)))

(defmethod handle-event :around ((stream lem-stream-pane) event)
  (input:send-lem event stream)
  (call-next-method))


(defun compose-display (frame pane)
    ;;(log:info "composing pane -- ~% have views ~a~%" (length (views frame)))

  ;;loop through views detect change & update
  (loop for view in (views frame) 
        do (view:draw-view view pane))
  ) 

